%%%-------------------------------------------------------------------
%%% @author cam
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. 9月 2023 14:45
%%%-------------------------------------------------------------------
-author("cam").

%% API
-type in() :: head | body | body_chunked | fin.
-define(TYPE_HTTP, 0).
-define(TYPE_HTTP2, 1).
-define(TYPE_WEBSOCKET, 2).

-record(http_state, {
    id = 0 :: integer(),
    type = ?TYPE_HTTP :: integer(),
    req_buffer = <<>> :: binary(),
    req_buffer_rest = <<>> :: binary(),
    req_headers = [] :: list(),
    req_method = <<>> :: binary(),
    req_path = <<>> :: binary(),
    req_ver = 'HTTP/1.1' :: atom(),
    req_len = 0 :: integer(),
    req_payload = <<>> :: term(),
    req_in = head :: in(),
    resp_status_line = {'HTTP/1.1', 200, <<"OK">>},
    resp_headers = [] :: list(),
    resp_len = 0 :: integer(),
    resp_buffer = <<>> :: binary(),
    resp_buffer_rest = <<>> :: binary(),
    resp_payload = <<>> :: binary(),
    resp_in = head :: in(),
    resp_chunked_state = {0, 0} :: tuple()
}).
