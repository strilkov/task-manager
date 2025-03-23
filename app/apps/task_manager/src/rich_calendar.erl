-module(rich_calendar).

-export([next_day_start/1]).

next_day_start({{Y, M, D}, {H, Min, S}}) ->
    case D == calendar:last_day_of_the_month(Y, M) of
        false ->
            {{Y, M, D + 1}, {H, Min, S}};
        true ->
            case M < 12 of
                true -> {{Y, M + 1, 1}, {H, Min, S}};
                false -> {{Y + 1, 1, 1}, {H, Min, S}}
            end
    end.
