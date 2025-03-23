%%%-------------------------------------------------------------------
%%% @author strilkov
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(task_manager_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  set_default_env(),
  set_prod_env(),

  {ok, NotificationType} = application:get_env(task_manager, notification),
  {ok, Targets} = application:get_env(task_manager, targets),
  io:format("Priv dir: ~s~n", [code:priv_dir(task_manager)]),
  io:format("Notification type: ~s~n", [NotificationType]),
  io:format("Targets count: ~w~n", [length(Targets)]),

  task_manager_sup:start_link().

stop(_State) ->
  ok.

set_default_env() ->
  Priv = code:priv_dir(task_manager),
  {ok, Data} = file:consult(Priv ++ "/default.conf"),
  lists:foreach(fun config_entry_handler/1, Data).

set_prod_env() ->
  Priv = code:priv_dir(task_manager),
  case file:consult(Priv ++ "/prod.conf") of
    {ok, Data} -> lists:foreach(fun config_entry_handler/1, Data);
    {error, _} -> skip
  end.

config_entry_handler({K, V}) ->
  application:set_env(task_manager, K, V).