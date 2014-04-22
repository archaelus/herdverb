%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Default HTTP handler for herdverb.
%% @end
-module(herdverb_respond_handler).

-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

-include("herdverb_log.hrl").

-define(CRLF, <<"\r\n">>).


init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ResponseHeaders, Req2} = response_headers(Req),
    {ReqIOList, Req3} = to_iolist(Req2),
    ?INFO("Request: ~s", [ReqIOList]),
    {ok, Req4} = cowboy_req:reply(200,
                                  ResponseHeaders,
                                  ReqIOList,
                                  Req2),
    {ok, Req4, State}.

terminate(_Req, _Reason, _State) ->
    ok.

response_headers(Req) ->
    {Headers, Req2} = cowboy_req:headers(Req),
    Split = byte_size(<<"response-">>),
    {[{binary:part(Header, Split, byte_size(Header) - Split),
       HeaderValue}
      || {Header, HeaderValue} <- Headers,
         byte_size(Header) > Split,
         binary:part(Header, 0, Split) =:= <<"response-">>],
     Req2}.

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
