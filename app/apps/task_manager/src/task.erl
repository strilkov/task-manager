-module(task).
-author("strilkov").

-behaviour(gen_server).
-callback do_job() -> term().

%% API
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([interval_to_first_run/1]).

start_link(M, Time) ->
    gen_server:start_link({local, M}, ?MODULE, [M, Time], []).

init([M, Time]) ->
    Timer = erlang:send_after(interval_to_first_run(Time), self(), tick),
    io:format("Start module: ~w~n", [M]),
    io:format("Start time: ~w~n", [Time]),
    io:format("Interval to first run: ~w~n", [interval_to_first_run(Time)]),
    {ok, [M, Time, Timer]}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, [M, Time, OldTimer]) ->
    erlang:cancel_timer(OldTimer),
    Timer = erlang:send_after(erlang:convert_time_unit(24*60*60 , second, millisecond), self(), tick),
    M:do_job(),
    {noreply, [M, Time, Timer]};
handle_info(now, [M, Time, Timer]) ->
    M:do_job(),
    {noreply, [M, Time, Timer]};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

interval_to_first_run({H, Min}) ->
    Now = calendar:local_time(),
    NowSec = calendar:datetime_to_gregorian_seconds(Now),
    DoAt = calendar:datetime_to_gregorian_seconds(first_run_date_time(Now, {H, Min})),
    erlang:convert_time_unit(DoAt - NowSec, second, millisecond).

first_run_date_time({{Y, M, D}, {H, _Min, _S}}, {H1, Min1}) when H1 > H ->
    {{Y, M, D}, {H1, Min1, 0}};
first_run_date_time({{Y, M, D}, {H, Min, _S}}, {H1, Min1}) when H1 == H, Min1 >= Min ->
    {{Y, M, D}, {H1, Min1, 0}};
first_run_date_time({{Y, M, D}, {H, Min, S}}, {H1, Min1}) when H > H1; H == H1, Min > Min1 ->
    {Date2, _} = rich_calendar:next_day_start({{Y, M, D}, {H, Min, S}}),
    {Date2, {H1, Min1, 0}}.
