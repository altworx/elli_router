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
            AllArgs = maps:merge(HandlerOpts, CbArgs),
            try
                elli_router_handler:handle(Module, Req, Bindings, AllArgs)
            catch throw:{ResponseCode, Headers, Body} when is_integer(ResponseCode) ->
                    {response, ResponseCode, Headers, Body};
                  throw:Exc ->
                    handle_event(request_throw, [Req, Exc, erlang:get_stacktrace()], AllArgs),
                    {response, 500, [], <<"Internal server error">>};
                  error:Error ->
                    handle_event(request_error, [Req, Error, erlang:get_stacktrace()], AllArgs),
                    {response, 500, [], <<"Internal server error">>};
                  exit:Exit ->
                    handle_event(request_exit, [Req, Exit, erlang:get_stacktrace()], AllArgs),
                    {response, 500, [], <<"Internal server error">>}
            end;
        {stop, 404} ->
            ignore;
        {stop, HttpCode} ->
            {HttpCode, [], <<>>}
    end.

%% @doc: Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return 'ok'.
handle_event(request_throw, [Req, Exc, Stacktrace], Args) ->
    error_logger:warning_msg("Elli Router throw event | Callback Args: ~p~nReqest: ~p~nStacktrace: ~s", [Args, Req, lager:pr_stacktrace(lists:reverse(Stacktrace), {throw, Exc})]),
    ok;
handle_event(request_error, [Req, Error, Stacktrace], Args) ->
    error_logger:error_msg("Elli Router error event | Callback Args: ~p~nReqest: ~p~nStacktrace: ~s", [Args, Req, lager:pr_stacktrace(lists:reverse(Stacktrace), {error, Error})]),
    ok;
handle_event(request_exit, [Req, Exit, Stacktrace], Args) ->
    error_logger:error_msg("Elli Router exit event | Callback Args: ~p~nReqest: ~p~nStacktrace: ~s", [Args, Req, lager:pr_stacktrace(lists:reverse(Stacktrace), {exit, Exit})]),
    ok;
handle_event(_Event, _Data, _Args) ->
    ok.

%% This needs to be called before registering this elli handler
init_params(Params) when is_list(Params) ->
    init_params(maps:from_list(Params));
init_params(#{routes := Routes, callback_args := CbArgs, host := Host}) ->
    Router = elli_cowboy_router:compile([{'_', Routes}]),
    #{router => Router, callback_args => CbArgs, host => Host}.
