%%%-------------------------------------------------------------------
%%% @author Adam Kovari <adam.kovari@altworx.com>
%%% @copyright (C) 2016, Altworx s.r.o.
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(elli_router).

-behaviour(elli_handler).

-export([init/2, handle/2, handle_event/3, init_params/1]).

init(_Req, _Args) ->
    {ok, standard}.

handle(Req, Args) when is_list(Args) ->
    handle(Req, maps:from_list(Args));
handle(Req, #{router := Router, callback_args := CbArgs, host := Host} = _Args) ->
    Path = elli_request:path(Req),

    case elli_cowboy_router:execute(#{host => Host, path => Path}, #{dispatch => Router}) of
        {ok, Bindings, #{handler := Module, handler_opts := HandlerOpts}} ->
            elli_router_handler:handle(Module, Req, Bindings, maps:merge(HandlerOpts, CbArgs));
        {stop, 404} ->
            ignore;
        {stop, HttpCode} ->
            {HttpCode, [], <<>>}
    end.

%% @doc: Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return 'ok'.
handle_event(_Event, _Data, _Args) ->
    ok.

%% This needs to be called before registering this elli handler
init_params(Params) when is_list(Params) ->
    init_params(maps:from_list(Params));
init_params(#{routes := Routes, callback_args := CbArgs, host := Host}) ->
    Router = elli_cowboy_router:compile([{'_', Routes}]),
    #{router => Router, callback_args => CbArgs, host => Host}.
