%%%-------------------------------------------------------------------
%%% @author Adam Kovari <adam.kovari@altworx.com>
%%% @copyright (C) 2016, Altworx s.r.o.
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(elli_router_handler).

-callback handle(Req :: elli:req(), Bindings :: cowboy_router:bindings(), Args :: #{}) ->
    ignore | {elli:response_code(), [tuple()], binary()} | {ok, [tuple()], binary()}.
-callback handle_event(Event :: elli:elli_event(), Args :: [term()], Config :: [tuple()]) -> ok.

-export([handle/4, handle_event/4]).

handle(Mod, Req, Bindings, Args) ->
    Mod:handle(Req, Bindings, Args).

handle_event(Mod, Event, Args, Config) ->
    Mod:handle_event(Event, Args, Config).
