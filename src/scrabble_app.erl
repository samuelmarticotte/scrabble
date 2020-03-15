%%%-------------------------------------------------------------------
%% @doc scrabble public API
%% @end
%%%-------------------------------------------------------------------

-module(scrabble_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    scrabble_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
