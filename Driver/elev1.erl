-module(elev1).
-export([start/0]).
-define(NumberOfFloors,4).

start() ->

    %Spawning schedule and driver
    Self=self(),
    DriverPid = spawn(fun() -> driver:start(Self) end),
    SchedulePid = spawn(fun() -> schedule:start(Self) end),

    InitialFloor=drive_up_to_closest_floor(DriverPid), %Todo: rename inital_floor inside this thing to initial_state cause this is bad
    Direction=up,
    idle(DriverPid,Direction,InitialFloor,SchedulePid).

go_on(DriverPid, Direction, Destination, PreviousFloor, SchedulePid) ->
    timer:sleep(50),
    receive
        {floor_reached, between_floors}-> %Why is this signal sendt in the first place?
            go_on(DriverPid,Direction,Destination,PreviousFloor,SchedulePid);
        {floor_reached, Floor} ->
            hit_floor(DriverPid,Direction,Destination,Floor,SchedulePid);
        {new_order, OrderDirection, OrderFloor} -> %Why do I not have a separate process for this stuff? Because the Driver only sends this to one pid... That can be fixed!
            SchedulePid ! {new_order, OrderDirection, OrderFloor},
            go_on(DriverPid,Direction,Destination,PreviousFloor,SchedulePid)
    end.

hit_floor(DriverPid,Direction,Destination,Floor,SchedulePid)->
    case Destination =:= Floor of
        true -> 
            DriverPid ! {set_motor_direction, stop},
            clean_order(Direction,Floor,SchedulePid),
            open(DriverPid,Direction,Destination,Floor,SchedulePid);
        false ->
            SchedulePid ! {is_order_in_schedule,Direction,Floor},
            io:format("fsm to schedule: Is this in schedule: "),
            print_order1(Direction,Floor),
            receive
                {order_is_in_schedule,Direction,Floor}->
                    io:format("Yes and I will open doors now~n"),
                    clean_order(Direction,Floor,SchedulePid),
                    DriverPid ! {set_motor_direction, stop},
                    open(DriverPid,Direction,Destination,Floor,SchedulePid); %Here, the information about wheather this is a stop-by or reaching destination is kept.
                {order_is_not_in_schedule, Direction, Floor}->
                    io:format("Nope~n"),
                    go_on(DriverPid,Direction,Destination,Floor,SchedulePid)
            end
    end.


idle(DriverPid, Direction, Floor, SchedulePid)->
    timer:sleep(50),
    NewDestination = get_next_destination(SchedulePid, Direction, Floor), %The receive-loop here is just not transparent for shit
    NewDirection = get_next_direction(Floor, NewDestination),
    case NewDestination of
        -1->
            receive
                {new_order, OrderDirection, OrderFloor}-> %Again, I really, really need a separate process for this stuff. THIS IS A FAILURE AND IT MUST BE ATTENDED TO
                    SchedulePid ! {new_order, OrderDirection, OrderFloor}
            end,
            idle(DriverPid, Direction, Floor, SchedulePid);
        Floor->
            open(DriverPid, NewDirection, NewDestination, Floor, SchedulePid);
        _->
            DriverPid ! {set_motor_direction, NewDirection},
            go_on(DriverPid, NewDirection, NewDestination, Floor, SchedulePid)
        end.

open(DriverPid,Direction,Destination,Floor,SchedulePid)-> %when you are just open, you preserve the direction you came from
    ProcessPid=self(),
    TimerPid=spawn(fun()-> door_open_timer(2000,ProcessPid) end),
    receive Message ->
        case Message of  
            {new_order, OrderDirection, OrderFloor} -> %Why do I not have a separate process for this stuff? Because the Driver only sends this to one pid... That can be fixed!
                SchedulePid ! {new_order, OrderDirection, OrderFloor},
                open(DriverPid,Direction,Destination, Floor,SchedulePid);
            {wait_time_over} ->
                io:format("wait_time is over wihu~n"),
                case Destination =:= Floor of
                        true -> 
                            idle(DriverPid, Direction, Floor, SchedulePid);
                        false ->
                            DriverPid ! {set_motor_direction, Direction},
                            go_on(DriverPid, Direction, Destination, Floor, SchedulePid) %This looks dangerous, why no flying elevator?
                end
                        
        end
    end.


door_open_timer(WaitTime, Listener)->
    timer:sleep(WaitTime),
    Listener ! {wait_time_over}.

drive_up_to_closest_floor(DriverPid)->
    receive {initial_floor, InitialFloor}->
        case InitialFloor of
            between_floors ->
                DriverPid ! {set_motor_direction, up},
                receive {floor_reached, Floor} ->
                    DriverPid ! {set_motor_direction, stop},
                    Floor
                end;
            FloorNumber ->
                FloorNumber
        end
    end.

get_next_direction(Floor, Destination) ->
    case Floor of
        Floor when Destination =:= -1 ->
            stop;
        Destination ->
            open;
        Floor when Floor < Destination ->
            up;
        Floor when Floor > Destination ->
            down
    end.


%%%%THis one should be part of the schedule module, this is confusing AF
get_next_destination(SchedulePid, FromDirection, FromFloor)->
    SchedulePid ! {get_next_destination, FromDirection, FromFloor},
    receive
        {next_destination, NextDestination}->
            io:format("Next destination is: ~b~n",[NextDestination]),
            NextDestination
    end.

clean_order(Direction, Floor, SchedulePid)->
    SchedulePid ! {remove_order, cab, Floor},
    case Direction of
        up->
            SchedulePid ! {remove_order, hall_up, Floor};
        down->
            SchedulePid ! {remove_order, hall_down, Floor};
        stop->
            ok
    end.


print_order1(Direction, Floor) ->
    case Direction of
        stop -> io:format("cab, ~b~n ", [Floor]);
        up -> io:format("hall up, ~b~n ", [Floor]);
        down -> io:format("hall down, ~b~n ", [Floor])
    end.