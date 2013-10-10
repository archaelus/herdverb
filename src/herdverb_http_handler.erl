%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Default HTTP handler for herdverb.
%% @end
-module(herdverb_http_handler).

-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

-include("herdverb_log.hrl").

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    ?INFO("at=request method='~s'", [Method]),
    {Body, Req3} = format(Req2),
    {ok, Req4} = cowboy_req:reply(200,
                                  [{<<"Content-Type">>, <<"text/plain">>}],
                                  Body,
                                  Req3),
    {ok, Req4, State}.

terminate(_Req, _Reason, _State) ->
    ok.

format(Req) ->
    {{Fmt, Args}, Req2} = serialize(Req),
    {io_lib:format(Fmt, Args), Req2}.

serialize(Req) ->
    {[Method, Path, {V1, V2}, Headers], Req2} =
        lists:foldl(fun (F, {Acc, ReqM}) ->
                            {V, ReqM1} = cowboy_req:F(ReqM),
                            {[V | Acc], ReqM1}
                    end,
                    {[], Req},
                    lists:reverse([method, path, version, headers])),
    {{lists:flatten(["~s ~s HTTP/~p.~p~n",
                     ["~s: ~s~n"
                      || _ <- Headers ],
                     "~n"]),
      [Method, Path, V1, V2 |
       lists:append([[K, V] || {K, V} <- Headers])]},
     Req2}.
