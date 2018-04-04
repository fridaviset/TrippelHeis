-module(schedule).
-export([start/1]).
-record(order, {floor, direction}).
-record(schedule, {orders = [],destination_floor=0,destination_direction=up}).

start(Listener)->
    loop(Listener,#schedule{orders=[]}).

loop(Listener, Schedule)->
    timer:sleep(20),
    receive
        {new_order, OrderDir, OrderFloor} ->
            NewOrder=#order{floor = OrderFloor, direction = OrderDir},
            NewSchedule=add_order_to_schedule(NewOrder, Schedule),
            print_schedule(NewSchedule),
            loop(Listener, NewSchedule);

        {get_next_destination, FromDirection, FromFloor} ->
            case Schedule#schedule.orders of
                []->
                    Listener ! {next_destination, -1},
                    loop(Listener, Schedule);
                _->
                    NewSchedule=get_next_schedule(Schedule, FromDirection, FromFloor), %Direction preserving should be a part of this function
                    print_schedule(NewSchedule),
                    Listener ! {next_destination, NewSchedule#schedule.destination_floor},
                    loop(Listener, NewSchedule) %Notice that the queue removes an order the moment it is put as a destination
            end;

        {remove_order, OrderDir, OrderFloor} ->
            ServedOrder=#order{floor = OrderFloor, direction = OrderDir},
            NewSchedule=remove_order_from_schedule(ServedOrder,Schedule),
            loop(Listener,NewSchedule);
        {is_order_in_schedule, Direction, Floor} ->
            case is_order_in_schedule(Direction, Floor, Schedule) of
                true ->
                    Listener ! {order_is_in_schedule, Direction, Floor},
                    loop(Listener, Schedule);
                false ->
                    Listener ! {order_is_not_in_schedule, Direction, Floor},
                    loop(Listener, Schedule)
            end
    end.

add_order_to_schedule(Order, Schedule)->
    print_order(Order#order.direction, Order#order.floor),
    case lists:member(Order, Schedule#schedule.orders) of 
	true ->
	    Schedule;
	false ->
        Schedule#schedule{orders=[Order|Schedule#schedule.orders]}
    end.

remove_order_from_schedule(Order, Schedule)->
    print_removed_order(Order#order.direction,Order#order.floor),
    Schedule#schedule{orders=lists:delete(Order, Schedule#schedule.orders)}.

is_order_in_schedule(Dir, OrderFloor, Schedule)->
    case Dir of
        up->
            OrderDir=hall_up;
        down->
            OrderDir=hall_down;
        stop->
            OrderDir=cab
    end,
    Order=#order{floor=OrderFloor, direction = OrderDir},
    CabOrder=#order{floor=OrderFloor, direction = cab},
    lists:member(Order,Schedule#schedule.orders) or lists:member(CabOrder,Schedule#schedule.orders).

get_direction_between_floors(FromFloor,ToFloor,OldDirection)->
    case FromFloor > ToFloor of
        true -> 
            down;
        false ->
            case FromFloor < ToFloor of
                true ->
                    up;
                false ->
                    OldDirection
            end
    end.


get_next_schedule(Schedule, FromDirection, FromFloor)->
    case Schedule#schedule.orders of
        [] ->
            Schedule;
        Orders ->
            NextOrder=pick_next_optimal_order(Schedule, FromDirection, FromFloor),
            NewOrderList=lists:delete(NextOrder, Orders),
            NewDirection=get_direction_between_floors(NextOrder#order.direction,FromFloor,FromDirection),
            #schedule{orders=NewOrderList,destination_direction=NewDirection,destination_floor=NextOrder#order.floor}
    end.

pick_next_optimal_order(Schedule, FromDirection, FromFloor)->
    [H|T]=Schedule#schedule.orders,
    InitialCost=cost:calculate1(FromDirection, FromFloor, H#order.floor,H#order.direction),
    return_optimal_value(T,InitialCost,H,Schedule).
return_optimal_value(Orders, OptimalCost, OptimalOrder, Schedule)->
    case Orders =:= [] of
        true ->
            OptimalOrder;
        false ->
            [H|T]=Orders,
            New_optimal_cost=cost:calculate1(Schedule#schedule.destination_direction,Schedule#schedule.destination_floor ,H#order.floor,H#order.direction),
            case New_optimal_cost>=OptimalCost of
                true->
                    return_optimal_value(T, OptimalCost, OptimalOrder, Schedule);
                false->
                    return_optimal_value(T, New_optimal_cost, H, Schedule)
            end
    end.

%Printers

print_order(Direction, Floor) ->
    case Direction of
        cab -> io:format("queue got cab order at floor number: ~b~n", [Floor]);
        hall_up -> io:format("queue got hall up order at floor number: ~b~n", [Floor]);
        hall_down -> io:format("queue got hall down order at floor number: ~b~n", [Floor])
    end.

print_order1(Direction, Floor) ->
    case Direction of
        cab -> io:format("cab, ~b, ", [Floor]);
        hall_up -> io:format("hall up, ~b, ", [Floor]);
        hall_down -> io:format("hall down, ~b, ", [Floor])
    end.

print_removed_order(Direction, Floor)->
    case Direction of
        cab -> io:format("queue removed cab order at floor number: ~b~n", [Floor]);
        hall_up -> io:format("queue removed hall up order at floor number: ~b~n", [Floor]);
        hall_down -> io:format("queue removed hall down order at floor number: ~b~n", [Floor])
    end.

print_schedule(Schedule)->
    io:format("queue is: "),
    print_list(Schedule#schedule.orders),
    io:format("~n").

print_list(List)->
    case List of
        [] ->
            ok;
        [Head|Tail]->
            print_order1(Head#order.direction,Head#order.floor),
            print_list(Tail)
    end.