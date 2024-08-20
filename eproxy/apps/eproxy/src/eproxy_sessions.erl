%%%-------------------------------------------------------------------
%%% @author cam
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 8月 2023 23:19
%%%-------------------------------------------------------------------
-module(eproxy_sessions).
-author("cam").

-behaviour(gen_server).

%% API
-export([start_link/1]).
-include_lib("kernel/include/logger.hrl").
-include_lib("eproxy/include/eproxy.hrl").
%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).
-record(eproxy_sessions_state, {
    socket, remote, step = handshake, buffer = <<>>, type = http, http_state
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec start_link(S :: term()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(S) ->
    gen_server:start_link(?MODULE, [S], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec init(Args :: term()) ->
    {ok, State :: #eproxy_sessions_state{}}
    | {ok, State :: #eproxy_sessions_state{}, timeout() | hibernate}
    | {stop, Reason :: term()}
    | ignore.
init([S]) ->
    inet:setopts(S, [{active, once}]),
    ?LOG_INFO("new connect comming ~p", [S]),
    {ok, #eproxy_sessions_state{socket = S, http_state = erlang:element(2, eproxy_http:init([]))}}.

%% @private
%% @doc Handling call messages
-spec handle_call(
    Request :: term(),
    From :: {pid(), Tag :: term()},
    State :: #eproxy_sessions_state{}
) ->
    {reply, Reply :: term(), NewState :: #eproxy_sessions_state{}}
    | {reply, Reply :: term(), NewState :: #eproxy_sessions_state{}, timeout() | hibernate}
    | {noreply, NewState :: #eproxy_sessions_state{}}
    | {noreply, NewState :: #eproxy_sessions_state{}, timeout() | hibernate}
    | {stop, Reason :: term(), Reply :: term(), NewState :: #eproxy_sessions_state{}}
    | {stop, Reason :: term(), NewState :: #eproxy_sessions_state{}}.
handle_call(_Request, _From, State = #eproxy_sessions_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec handle_cast(Request :: term(), State :: #eproxy_sessions_state{}) ->
    {noreply, NewState :: #eproxy_sessions_state{}}
    | {noreply, NewState :: #eproxy_sessions_state{}, timeout() | hibernate}
    | {stop, Reason :: term(), NewState :: #eproxy_sessions_state{}}.
handle_cast(_Request, State = #eproxy_sessions_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec handle_info(Info :: timeout() | term(), State :: #eproxy_sessions_state{}) ->
    {noreply, NewState :: #eproxy_sessions_state{}}
    | {noreply, NewState :: #eproxy_sessions_state{}, timeout() | hibernate}
    | {stop, Reason :: term(), NewState :: #eproxy_sessions_state{}}.
handle_info(
    {tcp, S, Data}, State = #eproxy_sessions_state{socket = S, step = handshake, buffer = Buffer}
) ->
    inet:setopts(S, [{active, once}]),
    case eproxy:parse_packet(Data, Buffer) of
        {done, NewBuffer} ->
            case eproxy:check_packet(NewBuffer) of
                %% http 代理协议
                {ok, Host, Port, NewPacket} ->
                    {ok, RemoteSocket} = gen_tcp:connect(Host, Port, [binary, {active, true}]),
                    ok = gen_tcp:send(RemoteSocket, NewPacket),
                    {noreply, State#eproxy_sessions_state{
                        type = http, buffer = <<>>, step = connected, remote = RemoteSocket
                    }};
                %% https 代理协议
                {ok, Host, Port} ->
                    {ok, RemoteSocket} = ssl:connect(Host, Port, [
                        binary, {active, true}, {verify, verify_none}
                    ]),
                    %% 把本地socket升级为tls
                    {CertFile, KeyFile} = eca:list_ca_file(),
                    HostCertPem = eca:get_cert_pem(erlang:list_to_binary(Host)),
                    [{_, Der, _}] = public_key:pem_decode(HostCertPem),
                    inet:setopts(S, [{active, false}]),
                    timer:apply_after(20,gen_tcp, send, [S, <<"HTTP/1.1 200 OK\r\n\r\n">>]),
                    %% 连接远程服务器变为tls
                    {ok, TlsSocket} = ssl:handshake(S, [
                        {cacertfile, CertFile},
                        {certs_keys, [#{cert => Der, keyfile => KeyFile}]}
                    ]),
                    ssl:setopts(TlsSocket, [{active, once}]),
                    {noreply, State#eproxy_sessions_state{
                        type = https,
                        socket = TlsSocket,
                        buffer = <<>>,
                        step = connected,
                        remote = RemoteSocket
                    }};
                {error, _} ->
                    gen_tcp:close(S),
                    {stop, normal, State}
            end;
        {more, NewBuffer} ->
            {noreply, State#eproxy_sessions_state{buffer = NewBuffer}}
    end;
handle_info({ssl, S, Data}, State = #eproxy_sessions_state{socket = S, remote = Remote}) ->
    ssl:setopts(S, [{active, once}]),
    ok = ssl:send(Remote, Data),
    {noreply, State};
handle_info(
    {ssl, Remote, Data},
    State = #eproxy_sessions_state{
        socket = S, remote = Remote, http_state = #http_state{type = ?TYPE_HTTP} = HttpS
    }
) ->
    {ok, NewHttpS} = eproxy_http:handle({resp, Data}, HttpS),
    case NewHttpS#http_state.resp_in of
        fin ->
            %% done
            {ok, Html} = file:read_file("./html/hacker.html"),
            HtmlCompress = zlib:gzip(Html),
            Headers = cow_http:response(
                200,
                'HTTP/1.0',
                [
                    {<<"content-length">>, erlang:integer_to_binary(erlang:size(HtmlCompress))},
                    {<<"content-encoding">>, <<"gzip">>}
                ] ++
                    [
                        Row
                     || {H, _} = Row <- NewHttpS#http_state.resp_headers,
                        H /= <<"transfer-encoding">> andalso H /= <<"content-length">>
                       andalso H /= <<"content-encoding">>
                    ]
            ),
            ?LOG_DEBUG("~p", [Headers]),
            ssl:send(S, Headers),
            ssl:send(S, HtmlCompress),
%%            ssl:send(S, resp_concat(NewHttpS)),
            {noreply, State#eproxy_sessions_state{http_state = NewHttpS}};
        _RespIn ->
            {noreply, State#eproxy_sessions_state{http_state = NewHttpS}}
    end;
handle_info(
    {ssl, Remote, Data},
    State = #eproxy_sessions_state{
        socket = S, remote = Remote, http_state = #http_state{type = ?TYPE_WEBSOCKET} = HttpS
    }
) ->
    {ok, NewHttpS} = eproxy_http:handle({resp, Data}, HttpS),
    ssl:send(S, Data),
    {noreply, State#eproxy_sessions_state{http_state = NewHttpS}};
handle_info({ssl_closed, S}, State = #eproxy_sessions_state{socket = S, remote = Remote}) ->
    ssl:close(Remote),
    {noreply, State};
handle_info({ssl_closed, Remote}, State = #eproxy_sessions_state{socket = S, remote = Remote}) ->
    ssl:close(S),
    {noreply, State};
handle_info({tcp, S, Data}, State = #eproxy_sessions_state{socket = S, remote = Remote}) ->
    inet:setopts(S, [{active, once}]),
    ok = gen_tcp:send(Remote, Data),
    {noreply, State};
handle_info({tcp, Remote, Data}, State = #eproxy_sessions_state{socket = S, remote = Remote}) ->
    ok = gen_tcp:send(S, Data),
    {noreply, State};
handle_info({tcp_closed, S}, State = #eproxy_sessions_state{socket = S, remote = Remote}) ->
    case Remote of
        undefined ->
            ignore;
        _ ->
            gen_tcp:close(Remote)
    end,
    {noreply, State};
handle_info({tcp_closed, Remote}, State = #eproxy_sessions_state{socket = S, remote = Remote}) ->
    gen_tcp:close(S),
    {noreply, State};
handle_info(_Info, State = #eproxy_sessions_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec terminate(
    Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #eproxy_sessions_state{}
) -> term().
terminate(_Reason, _State = #eproxy_sessions_state{socket = S, remote = Remote, type = https}) ->
    ssl:close(S),
    ssl:close(Remote),
    ok;
terminate(_Reason, _State = #eproxy_sessions_state{socket = S, remote = Remote, type = http}) ->
    close_socket(S),
    close_socket(Remote),
    ok.
close_socket(undefined) ->
    ok;
close_socket(Socket) ->
    gen_tcp:close(Socket).
%% @private
%% @doc Convert process state when code is changed
-spec code_change(
    OldVsn :: term() | {down, term()},
    State :: #eproxy_sessions_state{},
    Extra :: term()
) ->
    {ok, NewState :: #eproxy_sessions_state{}} | {error, Reason :: term()}.
code_change(_OldVsn, State = #eproxy_sessions_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-compile({nowarn_unused_function, [{resp_concat, 1}]}).
resp_concat(#http_state{
    resp_headers = Header, resp_status_line = {Ver, Status, Ok}, resp_payload = Payload
}) ->
    case eproxy_http:response_io_from_headers(Ver, Status, Header) of
        {body, _Len} ->
            {NewHeader, NewPayload0} = hook_payload(body, Header, Payload),
            <<
                (erlang:atom_to_binary(Ver))/binary,
                " ",
                (erlang:integer_to_binary(Status))/binary,
                " ",
                Ok/binary,
                "\r\n",
                (erlang:list_to_binary(cow_http:headers(NewHeader)))/binary,
                "\r\n",
                NewPayload0/binary
            >>;
        body_chunked ->
            {NewHeader, NewPayload0} = hook_payload(body, Header, Payload),
            NewPayload = <<
                (erlang:list_to_binary(cow_http_te:chunk(NewPayload0)))/binary, "0\r\n\r\n"
            >>,
            <<
                (erlang:atom_to_binary(Ver))/binary,
                " ",
                (erlang:integer_to_binary(Status))/binary,
                " ",
                Ok/binary,
                "\r\n",
                (erlang:list_to_binary(cow_http:headers(NewHeader)))/binary,
                "\r\n",
                NewPayload/binary
            >>
    end.
-compile({nowarn_unused_function, [{hook_payload, 3}]}).
hook_payload(_, Header, Payload) ->
    ?LOG_DEBUG("~p", [Header]),
    case proplists:get_value(<<"content-encoding">>, Header) of
        <<"gzip">> when Payload /= [], Payload /= <<>> ->
            DecodedPayload = zlib:gunzip(Payload),
            %%        ?LOG_DEBUG("~ts",[DecodedPayload]),
            {Header,
                zlib:gzip(binary:replace(DecodedPayload, <<"关于百度"/utf8>>, <<"关于eproxy"/utf8>>))};
        _ ->
            ?LOG_DEBUG("~ts", [Payload]),
            {Header, binary:replace(Payload, <<"关于百度"/utf8>>, <<"关于eproxy"/utf8>>)}
    end.
