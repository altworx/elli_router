-module(elli_router_test_handler).

-behaviour(elli_router_handler).

-export([handle/3, handle_event/3]).

-spec handle(elli:req(), cowboy_router:bindings(), #{}) -> {ok, [], binary()}.
handle(_Req, #{bindings := _Bindings, host := _Host, host_info := _HostInfo, path := _Path, path_info := _PathInfo}, _CbArgs) ->
    {ok, [], <<"this is a test">>}.


handle_event(_Event, _CbArgs, _Config) ->
    ok.
