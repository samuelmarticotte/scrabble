-module(play).

-export([start_game/0]).

-record(player, {name,points}).
	
%%%============================================================
%%% The game starts here
start_game() ->
	%Register players

	Register = fun(Name, {Players,Pids}) -> 
		{ok,Pid} = play:start_link(),
		Player = player:register_player(Pid,Name),
		{[Player|Players],[Pid|Pids]}
	end,
	%Show all players
	{Players,PPids} = lists:foldl(Register, {[],[]},
					{["Dennis","Alexia","Igor","Sam"]}),
		
	play_message(),
	%Start turns (loop)
	take_turns(Players,Players,PPids,PPids).


%%%============================================================
%%% All player take turns, then a new round starts
take_turns(AllPlayers,[],Pids,[]) ->
	take_turns(AllPlayers,AllPlayers,Pids,Pids);

take_turns(AllPlayers,Players,AllPids,Pids) ->
	[CurrentPlayer | NextPlayers] = Players,
	[CurrentPid | NextPids] = Pids,
	take_turn(CurrentPlayer,CurrentPid),
	take_turns(AllPlayers,NextPlayers,AllPids,NextPids).

%%% Defines how a single turn takes place
take_turn(Player,Pid) ->
	io:format("~nTurn of ~p is starting~n",[Player#player.name]),
	Input = io:get_line("Enter score: "),
	{Points, _} = string:to_integer(Input),
	NewPlayer = player:add_points(Pid,Points),
	player:cheer_player(Pid,Points),
	io:format("~p has scored ~p points so far. Good Job!~n",
		[NewPlayer#player.name, NewPlayer#player.points]).



%%%============================================================
play_message() ->
	io:format("Game is now starting").
