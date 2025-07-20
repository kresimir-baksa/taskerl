%%%-------------------------------------------------------------------
%% @doc Task processing and dependency resolution module.
%%
%% This module handles:
%% 1. Sorting tasks based on their dependencies
%% 2. Detecting circular dependencies
%% 3. Producing a valid execution order
%%
%% == Algorithm ==
%% Uses depth-first search to resolve dependencies and detect cycles.
%% @end
%%%-------------------------------------------------------------------

-module(taskerl_task_processor).

-include("taskerl.hrl").

-export([sort_tasks/1]).

-type task() :: map().
-type task_map() :: map().
-type task_name() :: any().
-type task_list() :: [task()].

%% @doc Sorts tasks based on their dependencies.
%% @param TasksMap A map containing a list of tasks under the "tasks" key
%% @return Map with sorted tasks under "tasks" key
-spec sort_tasks(map()) -> task_map().
sort_tasks(#{?TASKS_KEY := TaskList}) ->
    {TaskNames, TaskMap} =
        lists:foldl(fun(#{?TASK_NAME_KEY := Name} = Task, {Names, Map}) ->
                       {[Name | Names], maps:put(Name, Task, Map)}
                    end,
                    {[], maps:new()},
                    TaskList),
    {_, Res} = resolve_task_order(TaskNames, TaskMap, sets:new(), sets:new(), []),
    #{?TASKS_KEY => lists:reverse(Res)}.

%% @doc Recursively resolves task dependencies.
-spec resolve_task_order(TaskList :: list(),
                         TaskMap :: task_map(),
                         ResolvedTasks :: sets:set(),
                         Path :: sets:set(),
                         ResultAggr :: task_list()) ->
                            {UpdateResolvedTasks :: sets:set(), Result :: task_list()}.
resolve_task_order([], _, ResolvedTasks, _, Result) ->
    {ResolvedTasks, Result};
resolve_task_order([Name | OtherTasks], TaskMap, ResolvedTasks, Path, Result) ->
    % Crash here if it the cycle is detected to prevent stack overflow
    false = is_cycle(Name, Path),
    case sets:is_element(Name, ResolvedTasks) of
        true ->
            % This task is already resolved, skip it
            resolve_task_order(OtherTasks, TaskMap, ResolvedTasks, Path, Result);
        false ->
            % Resolve this task and its dependencies
            resolve_new_task(Name, OtherTasks, TaskMap, ResolvedTasks, Path, Result)
    end.

%% @doc Checks for circular dependencies.
-spec is_cycle(Name, Path) -> boolean() when Path :: sets:set(Name).
is_cycle(Name, Path) ->
    sets:is_element(Name, Path).

%% @doc Resolves dependencies for a new unprocessed task.
-spec resolve_new_task(Name :: task_name(),
                       OtherTasks :: list(),
                       TaskMap :: task_map(),
                       ResolvedTasks :: sets:set(),
                       Path :: sets:set(),
                       ResultAggr :: task_list()) ->
                          {UpdateResolvedTasks :: sets:set(), Result :: task_list()}.
resolve_new_task(Name, OtherTasks, TaskMap, ResolvedTasks, Path, Result) ->
    Task = maps:get(Name, TaskMap),
    Required = maps:get(?TASK_REQUIRES_KEY, Task, []),
    % Resolve dependencies first
    {NewResolvedTasks, NewResult} =
        resolve_task_order(Required,
                           TaskMap,
                           ResolvedTasks,
                           sets:add_element(Name, Path),
                           Result),
    % Resolve the current task and move on to the other tasks
    resolve_task_order(OtherTasks,
                       TaskMap,
                       sets:add_element(Name, NewResolvedTasks),
                       Path,
                       [clear_task_params(Task) | NewResult]).

%% @doc Clears unnecessary parameters from a task.
%% This is used to ensure only relevant fields are returned.
-spec clear_task_params(task()) -> task().
clear_task_params(Task) ->
    maps:with([?TASK_NAME_KEY, ?TASK_COMMAND_KEY], Task).
