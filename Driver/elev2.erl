-module(elev2).
-export([start/0]).


start() ->
    
    DriverManagerPID = spawn(fun() -> driver_manager_init() end),
    driver:start(DriverManagerPID, ElevatorType),

    FsmManagerPID = spawn(fun() -> fsm_manager_init() end),
    FsmPID = elev_fsm:start(FsmManagerPID),
    register(fsm, FsmPID),
    
    ScheduleManagerPID = spawn(fun() -> schedule_manager() end),
    SchedulePID = schedule:start(ScheduleManagerPID),
    register(schedule, SchedulePID),
    
    FsmManagerPID ! init_completed,
    DriverManagerPID ! init_completed,
    ScheduleManagerPID ! init_completed.



fsm_manager_init() ->
    receive init_completed ->
	    ok
    end,
    fsm_manager().
fsm_manager() ->
    receive
	{init, started} ->
	    elev_driver:set_motor_direction(up);
	{init, completed} ->
	    ok;
	{direction, request, Caller} ->
	    Direction = schedule:get_next_direction(schedule),
	    Caller ! {direction, response, Direction};
	{motor, up} ->
	    elev_driver:set_motor_direction(up),
	    schedule:floor_left(schedule, up);
	{motor, down} ->
	    elev_driver:set_motor_direction(down),
	    schedule:floor_left(schedule, down);
	{motor, stop} ->
	    elev_driver:set_motor_direction(stop);
	{doors, open} ->
	    schedule:stopped_at_floor(schedule),
	    elev_driver:set_door_open_lamp(on);
	{doors, close} ->
	    elev_driver:set_door_open_lamp(off)
    end,
    fsm_manager().

driver_manager_init() ->
    receive init_completed ->
	    ok
    end,
    driver_manager().

driver_manager() ->
    receive
	{new_order, Direction, Floor} ->
	    order_distributer:add_order(Floor, Direction);
	{floor_reached, Floor} ->
	    elev_driver:set_floor_indicator(Floor),
	    elev_fsm:event_floor_reached(fsm),
	    schedule:floor_reached(schedule, Floor)
    end,
    driver_manager().

schedule_manager() ->			    
    receive
	{order_served, Floor, Direction} ->
	    order_distributer:remove_order(Floor, Direction)
    end,
    schedule_manager().