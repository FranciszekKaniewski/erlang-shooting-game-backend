    -module(game_engine).
    -behaviour(gen_server).

    %% API
    -export([
        start_link/0,
        player_join/1,
        player_move/2,
        remove_player/1,
        player_shoot/1
    ]).

    -export([
        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2
    ]).

    %% Stałe gry
    -define(MAP_SIZE, 100).
    -define(TICK_RATE, 100).

    -record(state, {
        players = #{},
        attacks = []
    }).


    init([]) ->
        rand:seed(exrop),

        %% Uruchomienie pętli gry
        erlang:send_after(?TICK_RATE, self(), tick),
        {ok, #state{}}.

    %api
    start_link() ->
        gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

    player_join(Pid) ->
        gen_server:cast(?MODULE, {join, Pid}).

    player_move(Pid, Dir) ->
        gen_server:cast(?MODULE, {move, Pid, Dir}).

    remove_player(Pid) ->
        gen_server:cast(?MODULE, {leave, Pid}).
    player_shoot(Pid) -> 
        gen_server:cast(?MODULE, {shoot, Pid}).
    %

    % Join
    handle_cast({join, Pid}, State) ->
        erlang:monitor(process, Pid),
        PlayerId = list_to_binary(pid_to_list(Pid)),

        NewPlayer = #{
            id => PlayerId,
            x => rand:uniform(?MAP_SIZE) - 1,
            y => rand:uniform(?MAP_SIZE) - 1,
            dir => <<"right">>,
            status => <<"alive">>,
            score => 0
        },

        NewPlayers = maps:put(Pid, NewPlayer, State#state.players),
        io:format("Game: Do areny dolacza gracz ~p~n", [Pid]),

        Pid ! {send_json, #{
            type => <<"init">>,
            self_id => PlayerId,
            map_size => ?MAP_SIZE
        }},

        {noreply, State#state{players = NewPlayers}};

    % Move
    handle_cast({move, Pid, Dir}, State) ->
        Players = State#state.players,
        case maps:find(Pid, Players) of
            {ok, #{status := <<"alive">>} = Player} ->
                {DX, DY} = dir_to_delta(Dir),
                
                %% Logika pozycji
                NX = max(0, min(?MAP_SIZE - 1, maps:get(x, Player) + DX)),
                NY = max(0, min(?MAP_SIZE - 1, maps:get(y, Player) + DY)),

                NewPlayer = Player#{
                    x => NX, 
                    y => NY, 
                    dir => Dir
                },
                
                NewPlayers = maps:put(Pid, NewPlayer, Players),
                {noreply, State#state{players = NewPlayers}};
            _ ->
                {noreply, State}
        end;

    % Leave
    handle_cast({leave, Pid}, State) ->
        NewPlayers = maps:remove(Pid, State#state.players),
        {noreply, State#state{players = NewPlayers}};
    %Shoot
    handle_cast({shoot, Pid}, State) ->
        Players = State#state.players,

        case maps:find(Pid, Players) of
            {ok, #{status := <<"alive">>} = Shooter} ->
                {Area, View} = attackArea(Shooter),

                {Score, NewPlayers} = maps:fold(
                    fun(TargetPid, TargetP, {Score, GraczeAcc}) ->
                        case isShooted(TargetP, Area) of
                            true when TargetPid =/= Pid ->

                                TargetPid ! {send_json, #{
                                    type => <<"death">>,
                                    killer_id => maps:get(id, Shooter),
                                    final_score => score(TargetP)
                                }},
                                
                                TargetPid ! kill_socket,

                                {Score + 1, maps:remove(TargetPid, GraczeAcc)};
                            _ ->
                                {Score, GraczeAcc}
                        end
                    end,
                    {0, Players},
                    Players
                ),

                NewShooter = case maps:find(Pid, NewPlayers) of
                    {ok, A} -> 
                        NewPlayers#{Pid => A#{score => score(A) + Score}};
                    _ -> 
                        NewPlayers
                end,

                NewAttacks = [View | State#state.attacks],

                {noreply, State#state{players = NewShooter, attacks = NewAttacks}};
            _ ->
                {noreply, State}
        end;

    handle_cast(_Msg, State) ->
        {noreply, State}.

    attackArea(Player) ->
        X = x(Player),
        Y = y(Player),
        Dir = dir(Player),

        case Dir of
            <<"up">> ->
                Area = {X, Y - 5, 1, 5},
                View = #{x => X, y => Y - 5, w => 1, h => 5, color => <<"yellow">>};
            <<"down">> ->
                Area = {X, Y + 1, 1, 5},
                View = #{x => X, y => Y + 1, w => 1, h => 5, color => <<"yellow">>};
            <<"left">> ->
                Area = {X - 5, Y, 5, 1},
                View = #{x => X - 5, y => Y, w => 5, h => 1, color => <<"yellow">>};
            <<"right">> ->
                Area = {X + 1, Y, 5, 1},
                View = #{x => X + 1, y => Y, w => 5, h => 1, color => <<"yellow">>}
        end,
        {Area, View}.

    isShooted(Player, {AreaX, AreaY, Width, Height}) ->
        PX = x(Player),
        PY = y(Player),
        PX >= AreaX andalso PX < AreaX + Width andalso
        PY >= AreaY andalso PY < AreaY + Height.

    x(Player) -> maps:get(x, Player).
    y(Player) -> maps:get(y, Player).
    dir(Player) -> maps:get(dir, Player).
    score(Player) -> maps:get(score, Player).
    %Moves
    dir_to_delta(<<"up">>) -> {0, -1};
    dir_to_delta(<<"down">>) -> {0, 1};
    dir_to_delta(<<"left">>) -> {-1, 0};
    dir_to_delta(<<"right">>) -> {1, 0};
    dir_to_delta(_) -> {0, 0}.

    handle_call(_Req, _From, State) -> 
        {reply, {error, unknown_request}, State}.

    handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
        NowiGracze = maps:remove(Pid, State#state.players),
        io:format("Gracz ~p opuszcza arene~n", [Pid]),
        {noreply, State#state{players = NowiGracze}};

    % Pętla gry
    handle_info(tick, State) ->
        Players = [P || {_Pid, P} <- maps:to_list(State#state.players)],

        Msg = #{
            type => <<"state">>,
            players => Players,
            attacks => State#state.attacks
        },

        % Rozglaszanie stanu do wszystkich graczy
        [Pid ! {send_json, Msg} || Pid <- maps:keys(State#state.players)],

        erlang:send_after(?TICK_RATE, self(), tick),
        {noreply, State#state{attacks = []}}.
    %