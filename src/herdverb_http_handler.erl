%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Default HTTP handler for herdverb.
%% @end
-module(herdverb_http_handler).

-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

-include("herdverb_log.hrl").

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_http_req:method(Req),
    ?INFO("at=request method='~s'", [Method]),
    Body = io_lib:format("at=request method='~s'", [Method]),
    {ok, Req3} = cowboy_http_req:reply(200,
                                       [{'Content-Type', "text/plain"}],
                                       Body,
                                       Req2),
    {ok, Req3, State}.

terminate(_Req, _State) ->
    ok.
