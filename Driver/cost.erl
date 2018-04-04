-module(cost).
-export([calculate1/4]).

calculate1(ElevatorDirection,ElevatorDestination,OrderFloor,OrderDirection)->
    case OrderDirection =:= cab of
        true ->
            -5;
        false ->
            case ElevatorDirection of
                up ->
                    ElevatorDestination-OrderFloor;
                down ->
                    OrderFloor-ElevatorDestination;
                open ->               %%%%%%%%%%%Yeeeah not quite sure about these cases. Bad cases, should be eliminated. Bad code.
                    0;
                stop ->
                    0
            end
    end.