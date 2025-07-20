%%%-------------------------------------------------------------------
%% @doc taskerl test suite.
%%%-------------------------------------------------------------------

-module(taskerl_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include("taskerl.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2,
         end_per_testcase/2]).
%% Test cases
-export([test_two_tasks_sorting/1, test_multiple_tasks_sorting/1, test_circular_deps/1,
         test_bad_requests/1]).

all() ->
    [test_two_tasks_sorting,
     test_multiple_tasks_sorting,
     test_circular_deps,
     test_bad_requests].

init_per_suite(Config) ->
    % Get host and port from config with defaults
    Host =
        case application:get_env(taskerl, http_host) of
            {ok, {A, B, C, D}} ->
                lists:concat([A, ".", B, ".", C, ".", D]);
            {ok, H} when is_list(H) ->
                H;
            _ ->
                "localhost"
        end,
    Port =
        case application:get_env(taskerl, http_port) of
            {ok, P} ->
                P;
            _ ->
                8080
        end,
    [{host, Host}, {port, Port} | Config].

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    application:ensure_all_started(taskerl),
    Config.

end_per_testcase(_, _Config) ->
    application:stop(taskerl),
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

test_two_tasks_sorting(Config) ->
    Host = ?config(host, Config),
    Port = ?config(port, Config),
    {ok, RequestBody} = load_from_inputs("two_tasks.json"),
    {ok, {{_, 200, _}, _, Body}} =
        httpc:request(post,
                      {lists:concat(["http://", Host, ":", Port, "/tasks/json"]),
                       [],
                       "application/json",
                       RequestBody},
                      [],
                      []),
    Response = jsx:decode(list_to_binary(Body)),
    {ok, ExpectedResponseRaw} = load_from_inputs("two_tasks_out.json"),
    ExpectedResponse = jsx:decode(ExpectedResponseRaw),
    ?assertEqual(ExpectedResponse, Response),

    {ok, {{_, 200, _}, _, BodySh}} =
        httpc:request(post,
                      {lists:concat(["http://", Host, ":", Port, "/tasks/shell"]),
                       [],
                       "application/json",
                       RequestBody},
                      [],
                      []),
    ExpectedResponseShList =
        lists:join(<<"\n">>,
                   [?SHELL_HEADER | lists:map(fun(#{?TASK_COMMAND_KEY := Command}) -> Command end,
                                              maps:get(?TASKS_KEY, ExpectedResponse))]),
    ExpectedResponseSh = binary:list_to_bin(ExpectedResponseShList),
    ?assertEqual(binary_to_list(ExpectedResponseSh), BodySh),
    ok.

test_multiple_tasks_sorting(Config) ->
    Host = ?config(host, Config),
    Port = ?config(port, Config),
    {ok, RequestBody} = load_from_inputs("multiple_tasks.json"),
    {ok, {{_, 200, _}, _, Body}} =
        httpc:request(post,
                      {lists:concat(["http://", Host, ":", Port, "/tasks/json"]),
                       [],
                       "application/json",
                       RequestBody},
                      [],
                      []),
    Response = jsx:decode(list_to_binary(Body)),
    {ok, ExpectedResponseRaw} = load_from_inputs("multiple_tasks_out.json"),
    ExpectedResponse = jsx:decode(ExpectedResponseRaw),
    ?assertEqual(ExpectedResponse, Response),

    {ok, {{_, 200, _}, _, BodySh}} =
        httpc:request(post,
                      {lists:concat(["http://", Host, ":", Port, "/tasks/shell"]),
                       [],
                       "application/json",
                       RequestBody},
                      [],
                      []),
    ExpectedResponseShList =
        lists:join(<<"\n">>,
                   [?SHELL_HEADER | lists:map(fun(#{?TASK_COMMAND_KEY := Command}) -> Command end,
                                              maps:get(?TASKS_KEY, ExpectedResponse))]),
    ExpectedResponseSh = binary:list_to_bin(ExpectedResponseShList),
    ?assertEqual(binary_to_list(ExpectedResponseSh), BodySh),
    ok.

test_circular_deps(Config) ->
    Host = ?config(host, Config),
    Port = ?config(port, Config),
    {ok, RequestBody} = load_from_inputs("circular_deps.json"),
    {ok, {{_, 500, _}, _, _}} =
        httpc:request(post,
                      {lists:concat(["http://", Host, ":", Port, "/tasks/json"]),
                       [],
                       "application/json",
                       RequestBody},
                      [],
                      []),
    ok.

test_bad_requests(Config) ->
    Host = ?config(host, Config),
    Port = ?config(port, Config),
    {ok, {{_, 500, _}, _, _}} =
        httpc:request(post,
                      {lists:concat(["http://", Host, ":", Port, "/tasks/json"]),
                       [],
                       "application/json",
                       <<"{}">>},
                      [],
                      []),
    {ok, {{_, 404, _}, _, _}} =
        httpc:request(post,
                      {lists:concat(["http://", Host, ":", Port, "/tasks"]),
                       [],
                       "application/json",
                       <<"{}">>},
                      [],
                      []),
    ok.

%%%===================================================================
%%% Helper Functions
%%%===================================================================
load_from_inputs(FileName) ->
    Dir = filename:join([code:lib_dir(taskerl, test), "inputs"]),
    Path = filename:join(Dir, FileName),
    file:read_file(Path).
