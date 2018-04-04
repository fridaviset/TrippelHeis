-module(elev).
-export([start/0]).
-define(NumberOfFloors,4).

start() ->
    Self=self(),
    DriverPid = spawn(fun() -> driver:start(Self) end),
    SchedulePid = spawn(fun() -> schedule:start(Self) end),
    Destination=2,
    receive 
        {initial_floor,InitialFloor} ->
            Direction=get_initial_direction(InitialFloor,Destination)
    end,
    DriverPid ! {set_motor_direction, Direction},
    go_on(DriverPid, Direction, Destination,InitialFloor, SchedulePid).

go_on(DriverPid, Direction, Destination, PreviousFloor, SchedulePid) ->
    timer:sleep(50),
    receive
        {floor_reached, between_floors}->
            go_on(DriverPid,Direction,Destination,PreviousFloor,SchedulePid);
        {floor_reached, Floor} ->
            NewDestination = get_next_destination(Floor, Destination, Direction, SchedulePid),
            NewDirection = get_next_direction(Floor, Direction, NewDestination),
            DriverPid ! {set_motor_direction, NewDirection},
            go_on(DriverPid,NewDirection,NewDestination,Floor, SchedulePid);
        

        %{new_order,cab,OrderFloor} ->
        %    SchedulePid ! {new_order,cab,OrderFloor},
        %    NewDestination=OrderFloor,
        %    NewDirection = get_initial_direction(PreviousFloor, NewDestination),
        %    DriverPid ! {set_motor_direction, NewDirection},
        %    go_on(DriverPid,NewDirection,NewDestination,PreviousFloor, SchedulePid);
        {new_order, OrderDirection, OrderFloor} ->
            SchedulePid ! {new_order, OrderDirection, OrderFloor},
            go_on(DriverPid,Direction,Destination,PreviousFloor,SchedulePid)
    end.

get_initial_direction(Floor,Destination)->
    case Floor of
        Destination ->
            stop;
        between_floors ->
            up;
        Floor when Floor < Destination ->
            up;
        Floor when Floor > Destination ->
            down
    end.

get_next_direction(Floor,Direction, Destination) ->
    case Floor of
        Floor when Destination =:= -1 ->
            stop;
        Destination ->
            stop;
        between_floors ->
            Direction;
        Floor when Floor < Destination ->
            up;
        Floor when Floor > Destination ->
            down
    end.

get_next_destination(Floor,Destination,Direction,SchedulePid)->
    case Destination of
        Floor -> 
            io:format("keeping same destination: ~b~n",[Destination]),
            Destination;
        _ ->
            SchedulePid ! {get_next_destination, Floor, Direction},
            receive
                {next_destination, OrderFloor}->
                    io:format("next destination is: ~b~n", [OrderFloor]),
                    OrderFloor;
                {wait_for_next_order}->
                    -1
            end
    end.

