%%%-------------------------------------------------------------------
%% @doc Cowboy HTTP handler for task processing routes.
%%
%% Handles POST requests to:
%% - `/tasks/json`: Returns sorted tasks in JSON format
%% - `/tasks/shell`: Returns sorted tasks as a shell script
%% @end
%%%-------------------------------------------------------------------

-module(taskerl_route_handler).

-include("taskerl.hrl").

-behaviour(cowboy_handler).

-export([init/2]).

-type req() :: cowboy_req:req().
-type state() :: any().

% -spec init(req(), state()) -> {ok, req(), state()}.
%% @doc Initializes the handler and routes POST requests.
init(Request = #{method := <<"POST">>}, State) ->
    Path = cowboy_req:path(Request),
    handle_post(Path, Request, State).

%% @doc Handles POST requests based on the path.
-spec handle_post(binary(), req(), state()) -> {ok, req(), state()}.
handle_post(<<"/tasks/json">>, Request, State) ->
    ResponseData = process_json_body(Request),
    Response =
        cowboy_req:reply(200,
                         #{<<"content-type">> => <<"application/json">>},
                         jsx:encode(ResponseData),
                         Request),
    {ok, Response, State};
handle_post(<<"/tasks/shell">>, Request, State) ->
    #{?TASKS_KEY := SortedTasks} = process_json_body(Request),
    ListOfTasks = lists:map(fun(#{?TASK_COMMAND_KEY := Command}) -> Command end, SortedTasks),
    ShellScript = lists:join(<<"\n">>, [?SHELL_HEADER | ListOfTasks]),
    Response =
        cowboy_req:reply(200,
                         #{<<"content-type">> => <<"text/plain; charset=UTF-8">>},
                         binary:list_to_bin(ShellScript),
                         Request),
    {ok, Response, State}.

%% @doc Processes JSON tasks from request body.
%% 1. Reads request body
%% 2. Decodes JSON
%% 3. Sorts tasks based on dependencies
%% @throws {badarg, _} if JSON is invalid
%% @throws {circular_dependency, _, _} if circular dependencies exist
-spec process_json_body(req()) -> map().
process_json_body(Request) ->
    {ok, Body, _} = cowboy_req:read_body(Request),
    JsonData = jsx:decode(Body),
    taskerl_task_processor:sort_tasks(JsonData).
