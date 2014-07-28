%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Default HTTP handler for herdverb.
%% @end
-module(herdverb_http_handler).

-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

-include("herdverb_log.hrl").

-define(CRLF, <<"\r\n">>).


init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ReqIOList, Req2} = to_iolist(Req),
    ?INFO("Request: ~s", [ReqIOList]),
    {ok, Req3} = cowboy_req:reply(200,
                                  [{<<"Content-Type">>, <<"text/plain">>}],
                                  ReqIOList,
                                  Req2),
    {ok, Req3, State}.

terminate(_Req, _Reason, _State) ->
    ok.

to_iolist(Req) ->
    {Status, Req2} = status_to_iolist(Req),
    {Headers, Req3} = headers_to_iolist(Req2),
    {Body, Req4} = body_to_iolist(Req3),
    {[Status, ?CRLF,
      Headers,
      ?CRLF,
      Body],
     Req4}.

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

body_to_iolist(Req) ->
    case cowboy_req:body(Req) of
        {error, Why} ->
            {io_lib:format("Couldn't read body: ~p", [Why]), Req};
        {ok, Binary, Req1} ->
            {Binary, Req1}
    end.
