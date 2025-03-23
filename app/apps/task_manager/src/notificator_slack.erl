-module(notificator_slack).
-author("strilkov").

%% API
-export([notify/1]).

notify(Text) ->
    {ok, SlackUrl} = application:get_env(task_manager, slack_url),
    httpc:request(
        post,
        {SlackUrl, [], "application/json", "{'text': '" ++ Text ++ "'}"},
        [],
        []
    ).
