-module(notificator_console).
-author("strilkov").

%% API
-export([notify/1]).

notify(Text) ->
  io:format("~s~n", [Text]).
