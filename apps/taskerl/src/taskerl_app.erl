%%%-------------------------------------------------------------------
%% @doc taskerl application module.
%%
%% This module implements the application behavior for the taskerl service.
%% It manages the startup and shutdown of the Cowboy web server and the
%% application supervision tree.
%%
%% == API Endpoints ==
%% The application exposes the following HTTP endpoints:
%% - `POST /tasks/json` : Process tasks and return JSON result with sorted tasks
%% - `POST /tasks/shell` : Process tasks and return executable shell script
%%
%% == Request Format ==
%% The application expects JSON in the following format:
%% ```
%% {
%%     "tasks": [
%%         {
%%             "name": "task-1",
%%             "command": "touch /tmp/file1"
%%         },
%%         {
%%             "name": "task-2",
%%             "command": "echo 'Hello World!' > /tmp/file1",
%%             "requires": ["task-1"]
%%         }
%%     ]
%% }
%% '''
%%
%% == Configuration ==
%% The following runtime configuration is supported:
%% - `http_port` : Port number for HTTP listener (default: 8080)
%%
%% @end
%%%-------------------------------------------------------------------

-module(taskerl_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%% @doc Starts the taskerl application.
-spec start(StartType, StartArgs) -> {ok, pid()} | {ok, pid(), State} | {error, Reason}
    when StartType :: normal | {takeover, node()} | {failover, node()},
         StartArgs :: term(),
         State :: term(),
         Reason :: term().
start(_StartType, _StartArgs) ->
    Routes =
        cowboy_router:compile([{'_',
                                [{"/tasks/json", taskerl_route_handler, []},
                                 {"/tasks/shell", taskerl_route_handler, []}]}]),
    persistent_term:put(taskerl_routes, Routes),
    Host = application:get_env(taskerl, http_host, {127, 0, 0, 1}),
    Port = application:get_env(taskerl, http_port, 8080),
    {ok, _Pid} =
        cowboy:start_clear(taskerl_http_listener,
                           [{ip, Host}, {port, Port}],
                           #{env => #{dispatch => {persistent_term, taskerl_routes}}}),
    {ok, _} = taskerl_sup:start_link().

%% @doc Stops the taskerl application.
-spec stop(State :: term()) -> ok.
stop(_State) ->
    ok = cowboy:stop_listener(taskerl_http_listener).
