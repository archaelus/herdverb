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
    {[Method, Path, Version, Headers], Req2} =
        lists:foldr(fun (F, {Acc, ReqM}) ->
                            {V, ReqM1} = cowboy_req:F(ReqM),
                            {[V | Acc], ReqM1}
                    end,
                    {[], Req},
                    [method, path, version, headers]),
    {ok, Body, Req3} = cowboy_req:body(Req2),
    {{lists:flatten(["~s ~s ~s~n",
                     ["~s: ~s~n"
                      || _ <- Headers ],
                     "~n~s"]),
      [Method, Path, atom_to_list(Version) |
       lists:append([[K, V] || {K, V} <- Headers]) ++ [Body]]},
     Req3}.
