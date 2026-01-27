-module(game_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->

    % Konfiguracja ws
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ws", ws_handler, []}
        ]}
    ]),

    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
        env => #{dispatch => Dispatch}
    }),

    % Uruchamianie gry
    game_sup:start_link().

stop(_State) ->
    ok.