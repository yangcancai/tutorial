-module(eproxy_test).

-include_lib("eunit/include/eunit.hrl").

succeed() ->
    ok.

succeeding_test() ->
    succeed().

succeeding_fun_test_() ->
    fun() -> succeed() end.

succeeding_simple_test_() ->
    ?_test(succeed()).

parse_packet_test() ->
    {more, Buffer} = eproxy:parse_packet(<<"GET http://localhost:80/abc HTTP/1.1\r\n">>, <<>>),
    {done, Packet} = eproxy:parse_packet(<<"Host: localhost:80\r\n\r\nhello">>, Buffer),
    ?assertEqual(<<"GET http://localhost:80/abc HTTP/1.1\r\nHost: localhost:80\r\n\r\nhello">>, Packet),
    {ok, "localhost", 80, <<"GET /abc HTTP/1.1\r\nHost: localhost:80\r\n\r\nhello">> } = eproxy:check_packet(Packet),
    ok.