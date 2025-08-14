-module(site_status_check_task).

-behaviour(task).
-export([do_job/0]).

do_job() ->
    {ok, Targets} = application:get_env(task_manager, ?MODULE),
    Res = lists:map(fun with_error_handler/1, Targets),
    Lines = lists:map(fun to_log_line/1, Res),
    Line = lists:flatten(Lines),
    notificator:notify(Line),
    ok.

with_error_handler(Url) ->
    try handle_target(Url) of
        Result -> Result
    catch
        error:Reason -> {log, Url, error, Reason}
    end. 

handle_target(Url) ->
    {ok, {{_Version, Status, _ReasonPhrase}, _Headers, _Body}} = httpc:request(Url),
    case Status of
        200 ->
            {ok, Url, 200};
        _ ->
            {log, Url, Status}
    end.

to_log_line(Entry) ->
    case Entry of
        {ok, _, _} -> "";
        {log, Url, Status} -> io_lib:format(":red_circle: ~p : ~p~n", [Status, Url]);
        {log, Url, error, _Reason} -> ":red_circle: some error " ++ Url ++ "\n"
    end.

