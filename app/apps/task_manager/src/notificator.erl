-module(notificator).

%% API
-export([notify/1]).

notify(Text) ->
    case application:get_env(task_manager, notification) of
        {ok, slack} -> notificator_slack:notify(Text);
        _ -> notificator_console:notify(Text)
    end.
