%%%-------------------------------------------------------------------
%%% @author strilkov
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. May 2024 12:31
%%%-------------------------------------------------------------------
-module(task_manager_sup).
-author("strilkov").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
init([]) ->
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = #{
        strategy => one_for_one,
        intensity => MaxRestarts,
        period => MaxSecondsBetweenRestarts
    },

    {ok, {SupFlags, childspecs_from_env()}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

childspecs_from_env() ->
    {ok, Tasks} = application:get_env(task_manager, tasks),
    lists:map(
        fun({M, Time}) ->
            #{
                id => M,
                start => {'task', start_link, [M, Time]},
                restart => permanent,
                shutdown => 2000,
                type => worker,
                modules => ['task']
            }
        end,
        Tasks
    ).
