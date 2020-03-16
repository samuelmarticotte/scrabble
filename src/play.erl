-module(play).

-export([game/0]).

-record(player, {name,points}).
	
%%%============================================================
%%% The game starts here
game() ->
	io:format("--------------------~n"),
	io:format("Game is now starting~n"),
	io:format("--------------------~n"),
	
	%Register players
	Register = fun(Name) -> 
		{ok,Pid} = player:start_link(),
		Player = player:register_player(Pid,Name),
		{Player,Pid}
	end,
	%Show all players and get started!
	Players = [Register(N) || N <- ["Dennis","Alexia","Igor","Sam"]],
		
	%Start turns (loop)
	take_turns(Players).



%%%%============================================================
%%%% All player take turns, then a new round starts
take_turns(PlayInfo) ->
	[take_turn(Ply,Pid) || {Ply,Pid} <- PlayInfo],
	take_turns(PlayInfo).


%%%%============================================================
check_winner(Player,Pid)->
	Points = player:get_points(Pid),
	if 	Points > 500 -> end_game(Player);
		true -> ok
	end.	

%%%%============================================================
%%% Defines how a single turn takes place
take_turn(Player,Pid) ->
	io:format("~nTurn of ~p is starting~n",[Player#player.name]),
	Input = io:get_line("Enter score: "),
	{Points, _} = string:to_integer(Input),
	NewPlayer = player:add_points(Pid,Points),
	player:cheer_player(Pid,Points),
	io:format("~p has scored ~p points so far. Good Job!~n",
		[NewPlayer#player.name, NewPlayer#player.points]),
	check_winner(Player,Pid).

 end_game(Player) ->
	io:format("~n~p has won the game!!!~n",[Player#player.name]),
	io:get_line("Press enter to quit..."),
	halt(0).





