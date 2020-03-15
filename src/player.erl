-module(player).
-behaviour(gen_server).

-export([start_link/0, 
	register_player/1,
	register_player/2, 
	add_points/2,
	cheer_player/2,
	return_player/2, 
	play/1,
	finish_game/1]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
         terminate/2, code_change/3]).

-record(player, {name,points}).

%%%============================================================
%%% Client API

start_link() ->
    	gen_server:start_link(?MODULE, [], []).

%% Synchronous call

register_player(_) ->
	io:format("You need the PID: register_player(Pid, Name)~n").

register_player(Pid, Name) ->
   	gen_server:call(Pid, {register, Name}).

play(Pid) -> 
	gen_server:call(Pid, play).	

add_points(Pid,Points) ->
	gen_server:call(Pid,{add,Points}).

cheer_player(Pid,Points) ->
	gen_server:call(Pid,{cheer,Points}).

%% This call is asynchronous
return_player(Pid, Player = #player{}) ->
    gen_server:cast(Pid, {return, Player}).

%% Synchronous call
finish_game(Pid) ->
    gen_server:call(Pid, terminate).

%%%============================================================
%%% Server functions

init([]) ->
	process_flag(trap_exit, true), 
	{ok, []}.

handle_call({register, Name}, _From, _Player) ->
	NewPlayer = make_player(Name),
	{reply,NewPlayer,NewPlayer};


handle_call({add,Points},_From,Player) ->
	NewPoints = Points + Player#player.points,
	NewPlayer = Player#player{points=NewPoints},
	{reply,NewPlayer,NewPlayer};	

handle_call({cheer,Points},_From,Player) ->
	if Points > 10 -> [io:format("Amazing ~p! ~p~n",[Player#player.name,X])
							 || X <- ["Wow!","Wow!","Wow!"]];
	   true -> [io:format("~p! ~p~n",[X,Player#player.name]) || X <- ["Bof!","Bof!","Bof!"]]
	end, 
	{reply,bof,Player};

handle_call(terminate, _From, Player) ->
    	{stop, normal, ok, Player}.

%% JUST FOR THE EXAMPLE (NOT USING YET)
handle_cast({return, Player = #player{}}, Player) ->
    	{noreply, Player}.

handle_info(Msg, Player) ->
    	io:format("Unexpected message: ~p~n",[Msg]),
    	{noreply, Player}.

terminate(normal, Player) ->
    	io:format("~p was set free.~n",[Player#player.name]),
    	ok.

code_change(_OldVsn, State, _Extra) ->
    	%% No change planned. The function is there for the behaviour,
    	%% but will not be used. Only a version on the next
    	{ok, State}. 

%%%============================================================
%%% Private functions

make_player(Name) -> 
	#player{name=Name,points=0}.
