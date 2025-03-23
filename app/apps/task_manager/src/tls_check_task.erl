-module(tls_check_task).
-include_lib("public_key/include/public_key.hrl").
-behaviour(task).
-export([do_job/0]).

do_job() ->
    {ok, Targets} = application:get_env(task_manager, targets),
    Res = lists:map(fun error_handler/1, Targets),
    Lines = lists:map(fun to_log_line/1, Res),
    Line = lists:flatten(Lines),
    notificator:notify(Line),
    ok.

to_log_line(Entry) ->
    case Entry of
        {ok, _} -> "";
        {log, Hostname, expired} -> ":red_circle: " ++ Hostname ++ ": cert is expired.\n";
        {log, Hostname, soon_expire} -> ":large_yellow_circle: " ++ Hostname ++ ": cert will expire soon.\n";
        {log, Hostname, error, _Reason} -> ":red_circle: " ++ Hostname ++ ": an error occurred.\n"
    end.

error_handler(Hostname) ->
    try handle_target(Hostname) of
        Result -> Result
    catch
        error:Reason -> {log, Hostname, error, Reason}
    end. 

handle_target(Hostname) ->
    Cert = fetch_cert(Hostname),
    Validity = extract_validity(Cert),
    io:fwrite("~s -> ~w~n", [Hostname, Validity]),
    Diff = validity_to_diff(Validity),
    case Diff of
        {Days, _} when Days < 0 ->
            {log, Hostname, expired};
        {Days, _} when Days < 4 ->
            {log, Hostname, soon_expire};
        _ ->
            {ok, Hostname}
    end.

fetch_cert(Hostname) ->
    {ok, Sock} = ssl:connect(Hostname, 443, [{cacerts, public_key:cacerts_get()}, {verify, verify_none}], 5000),
    {ok, Cert} = ssl:peercert(Sock),
    ok = ssl:close(Sock),
    public_key:pkix_decode_cert(Cert, otp).

extract_validity(Cert) ->
    TBSCert = Cert#'OTPCertificate'.tbsCertificate,
    TBSCert#'OTPTBSCertificate'.validity.

utcTimeToDatetime(UtcTime) -> 
    Dt = datetimeToList(UtcTime, []),
    [Y, M, D, H, Min, S] = Dt,
    {{updateY(Y), M, D}, {H, Min, S}}.

updateY(Y) when Y < 70 ->
    2000 + Y;
updateY(Y) ->
    1900 + Y.

datetimeToList([A, B | Rest], Res) ->
    Int = list_to_integer([A, B]),
    datetimeToList(Rest, [Int] ++ Res);

datetimeToList([_], Res) ->
    lists:reverse(Res).

validity_to_diff({'Validity',{utcTime, _From},{utcTime, To}}) ->
    % Example of input date: 250611235925Z.
    % Format "YYMMDDHHMMSSZ".
    io:fwrite("To ~p~n", [utcTimeToDatetime(To)]),
    io:fwrite("Now ~p~n", [calendar:universal_time()]),
    SecTo = calendar:datetime_to_gregorian_seconds(utcTimeToDatetime(To)),
    SecNow = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    Diff = calendar:seconds_to_daystime(SecTo - SecNow),
    io:fwrite("Diff ~p~n", [Diff]),
    Diff.
