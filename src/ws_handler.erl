-module(ws_handler).
-behaviour(cowboy_websocket).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_init(State) ->
    io:format("~n[POLACZENIE] Nowy gracz: ~p~n", [self()]),
    game_engine:player_join(self()),
    {ok, State}.

websocket_handle({text, Msg}, State) ->
    Data = jsx:decode(Msg, [return_maps]),

    case maps:get(<<"type">>, Data) of
        <<"move">> ->
            Dir = maps:get(<<"dir">>, Data),
            game_engine:player_move(self(), Dir),
            {ok, State};
            
        _ ->
            {ok, State}
    end;

websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info({send_json, Map}, State) ->
    Json = jsx:encode(Map),
    {reply, {text, Json}, State};

websocket_info(kill_socket, State) ->
    {stop, normal, State};

websocket_info(_Info, State) ->
    {ok, State}.

terminate(Reason, _Req, _State) ->
    io:format("~n[ROZLACZENIE] PID: ~p. Powod: ~p~n", [self(), Reason]),
    game_engine:remove_player(self()),
    ok.