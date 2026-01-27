-module(ws_handler).
-behaviour(cowboy_websocket).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_init(State) ->
    io:format("~n[POLACZENIE] Nowy gracz dolaczyl! PID procesu: ~p~n", [self()]),
    
    {ok, State}.

websocket_handle({text, Msg}, State) ->
    io:format("Gracz (~p) przyslal: ~p~n", [self(), Msg]),
    {reply, {text, <<"Odebrano!">>}, State};
websocket_handle(_Data, State) ->
    {ok, State}.

terminate(Reason, _Req, _State) ->
    io:format("~n[ROZLACZENIE] Gracz (PID: ~p) opuscil serwer.~n", [self()]),
    io:format("Powod rozlaczenia: ~p~n", [Reason]),
    ok.

websocket_info(_Info, State) ->
    {ok, State}.