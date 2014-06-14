%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Long response time handler for herdverb.
%% @end
-module(herdverb_time_handler).

-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

-include("herdverb_log.hrl").

-define(CRLF, <<"\r\n">>).


init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ReqIOList, Req2} = to_iolist(Req),
    {RespTimeMS, Req3} = cowboy_req:binding(time, Req2, 1000),
    TimePerChar = erlang:max(RespTimeMS div iolist_size(ReqIOList), 1),
    ?INFO("Responding at ~p ms/byte: ~s", [TimePerChar, ReqIOList]),
    {ok, Req4} =
        cowboy_req:chunked_reply(200,
                                 [{<<"Content-Type">>, <<"text/plain">>}],
                                 Req3),
    drip_feed(iolist_to_binary(ReqIOList), TimePerChar, Req4, State).

drip_feed(<<>>, _TimePerChar, Req, State) ->
    {ok, Req, State};
drip_feed(<<This, Rest/binary>>, TimePerChar, Req, State) ->
    case cowboy_req:chunk(<<This>>, Req) of
        ok ->
            timer:sleep(TimePerChar),
            drip_feed(Rest, TimePerChar, Req, State);
        {error, Why} ->
            ?ERR("~p while drip-feeding.", [Why]),
            {ok, Req, State}
    end.

terminate(_Req, _Reason, _State) ->
    ok.

to_iolist(Req) ->
    {Status, Req2} = status_to_iolist(Req),
    {Headers, Req3} = headers_to_iolist(Req2),
    {[Status, ?CRLF,
      Headers,
      ?CRLF],
     Req3}.

status_to_iolist(Req) ->
    {Method, Req2} = cowboy_req:method(Req),
    {Path, Req3} = cowboy_req:path(Req2),
    {Version, Req4} = cowboy_req:version(Req3),
    {[Method, " ", Path, " ", atom_to_binary(Version, latin1)],
     Req4}.

headers_to_iolist(Req) ->
    {Headers, Req2} = cowboy_req:headers(Req),
    {[ [K, <<": ">>, V, ?CRLF]
       || {K, V} <- Headers ],
     Req2}.
