%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc Handles the scores and ETS table of a certain group.
%%% @end
%%%-------------------------------------------------------------------
-module(group_pool).

-behaviour(gen_server).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0, tab/1, add/2, add_score/3, get_score/2, del/2]).
-export_types([]).

-type score() :: float().
-record(pr, {           % Table record with runing ids and scores
    sr  :: {score(), term()}  % Score and process reference(id)
}).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-define(init_score, 0.0).
-define(tab_id(Id), {maps:get(Id, State, not_found), Id}).
-define(TAB_CONFIGUTATION, [
    public,           % Multiple agents have to register in it
    ordered_set,      % The pool must be a set (no repeated values)
    % {read_concurrency, true}, % Concurrent read optimisation
    {keypos, #pr.sr}  % The key of the record must be the Ref
]).


%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server with a link (for supervisors).
%% @end
%%--------------------------------------------------------------------
-spec start_link() ->
  {ok, Pid::pid()} | ignore | {error, Reason::term()}.
start_link() ->
  gen_server:start_link(?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Mounts the ets table and sends it to the server.
%% @end
%%--------------------------------------------------------------------
-spec tab(Server::pid()) -> ets:tid().
tab(Server) -> 
    gen_server:call(Server, tab).

%%--------------------------------------------------------------------
%% @doc Adds an agent-ref to a group pool.
%% @end
%%--------------------------------------------------------------------
-spec add(Server::pid(), Ref::term()) ->
    ok | {error, Reason::term()}.
add(Server, Ref) ->
    gen_server:call(Server, {add, Ref}).

%%--------------------------------------------------------------------
%% @doc Adds a score to a registered id.
%% @end
%%--------------------------------------------------------------------
-spec add_score(Server::pid(), Ref::term(), Score::score()) -> 
    ok.
add_score(Server, Ref, Score) when is_float(Score) ->
    gen_server:cast(Server, {add_score, Ref, Score}).

%%--------------------------------------------------------------------
%% @doc Returns the score of a registered id.
%% @end
%%--------------------------------------------------------------------
-spec get_score(Server::pid(), Ref::term()) -> 
    ok | {error, Reason::term()}.
get_score(Server, Ref) ->
    gen_server:call(Server, {get_score, Ref}).

%%--------------------------------------------------------------------
%% @doc Removes an agent-ref from the group pool.
%% @end
%%--------------------------------------------------------------------
-spec del(Server::pid(), Ref::term()) -> 
      ok | {error, Reason::term}.
del(Server, Ref) ->
    gen_server:call(Server, {del, Ref}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Initializes the server
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #{}} | {ok, State :: #{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
    put(pool, ets:new(group_pool, ?TAB_CONFIGUTATION)),
    ?LOG_INFO(#{what=>"Group pool started", id=>self()}),
    {ok, #{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #{}) ->
  {reply, Reply :: term(), NewState :: #{}} |
  {reply, Reply :: term(), NewState :: #{}, timeout() | hibernate} |
  {noreply, NewState :: #{}} |
  {noreply, NewState :: #{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #{}} |
  {stop, Reason :: term(), NewState :: #{}}).
handle_call({add, Ref}, _From, State) ->
    case add_to_pool({?init_score, Ref}) of 
        {ok, Score} -> {reply, ok, State#{Ref=>Score}};
        Error       -> {reply, {error, Error}, State}
    end;
handle_call({del, Ref}, _From, State) ->
    case del_in_pool(?tab_id(Ref)) of 
        ok    -> {reply, ok, maps:remove(Ref, State)};
        Error -> {reply, {error, Error}, State}
    end;
handle_call({get_score, Ref}, _From, State) ->
    case ?tab_id(Ref) of 
        {Score, Ref} -> {reply, Score, State};
        not_found    -> {reply, {error, not_found}, State}
    end;
handle_call(tab, _From, State) ->
    {reply, get(pool), State};
handle_call(Request, From, State) ->
    ?LOG_WARNING(#{what=>"Unexpected call message", id=>self(), 
                   details=>#{request=>Request, from=>From}}),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #{}) ->
  {noreply, NewState :: #{}} |
  {noreply, NewState :: #{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #{}}).
handle_cast({add_score, Ref, Score}, State) ->
    case score_in_pool(?tab_id(Ref), Score) of 
        {ok, NewScore} -> {noreply, State#{Ref=>NewScore}};
        not_found      -> ?LOG_NOTICE(#{what=>"Score request on non registered", 
                                        id=>self(), details=>#{ref=>Ref}}),
                          {noreply, State}
    end;
handle_cast(Request, State) ->
    ?LOG_WARNING(#{what=>"Unexpected cast message", id=>self(), 
                   details=>#{request=>Request}}),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling all non call/cast messages
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #{}) ->
  {noreply, NewState :: #{}} |
  {noreply, NewState :: #{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #{}}).
handle_info({'ETS-TRANSFER', Tab, _, 'group_pool'}, State) -> 
    ?LOG_INFO(#{what=>"Tab transfer received", id=>self(), 
                details=>#{tab=>Tab}}),
    put(pool, Tab),
    {noreply, State};
handle_info(Info, State) ->
    ?LOG_WARNING(#{what=>"Unexpected info message", id=>self(), 
                   details=>#{info=>Info}}),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #{}) -> term()).
terminate(Reason, _State) ->
    ?LOG_INFO(#{what=>"Terminating group server", id=>self(), 
                details=>#{reason=>Reason}}),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #{},
                  Extra :: term()) ->
    {ok, NewState :: #{}} | {error, Reason :: term()}).
code_change(OldVsn, State, Extra) ->
    ?LOG_WARNING(#{what=>"Unexpected code change", id=>self(), 
                   details=>#{oldvsn=>OldVsn, extra=>Extra}}),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% Adds a pool record (pr) to the pool -------------------------------
add_to_pool({Score, _}=SP) -> 
    true = ets:insert(get(pool), #pr{sr=SP}),
    {ok, Score}.

% Removes a pool record (pr) from the pool --------------------------
del_in_pool(SP) ->
    true = ets:delete(get(pool), SP),
    ok.

% Adds the score to a pool record (pr) in the pool ------------------
score_in_pool({Score, Ref}=SP, Extra) -> 
    case ets:lookup(get(pool), SP) of
        []      -> not_found;
        [#pr{}] -> NewScore = Score+Extra,
                   add_to_pool({NewScore, Ref}),
                   del_in_pool(SP),
                   {ok, NewScore}
    end.


%%====================================================================
%% Eunit white box tests
%%====================================================================

% --------------------------------------------------------------------
% TESTS DESCRIPTIONS -------------------------------------------------
make_group_pool_test_() ->
    [{"Correct start and handle of group scores",
      {setup, local, fun pool_up/0, fun pool_down/1,
       fun(Pool) -> {inorder,
        [{"Correct member addition", ?_test(test_add(Pool))},
         {"Correct member scoring",  ?_test(test_add_score(Pool))},
         {"Correct score reply",     ?_test(test_get_score(Pool))},
         {"Correct member delete",   ?_test(test_del(Pool))}]}
       end
      }}].


% --------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS -------------------------------------------
-define(err_msg(Err), io_lib:format("Server start failed with ~p", [Err])).
pool_up() -> 
    case start_link() of 
        {ok, Server} -> {Server, tab(Server)};
        Error        -> error(?err_msg(Error))
    end.

pool_down({Server, _Tab}) -> 
    exit(Server, normal).


% --------------------------------------------------------------------
% ACTUAL TESTS -------------------------------------------------------

% Adds a Ref to the group -------------------------------------------
test_add({Server, Tab}) -> 
    ?assertEqual(ok, add(Server, fake_pid(1))),
    Id = {0.0, fake_pid(1)},
    ?assertMatch([#pr{sr=Id}], ets:lookup(Tab, Id)).

% Adds a score to a Ref in the group --------------------------------
test_add_score({Server, Tab}) ->
    ok = add(Server, fake_pid(2)),
    ?assertEqual(ok, add_score(Server, fake_pid(2), 100.0)),
    Id = {100.0, fake_pid(2)},
    timer:sleep(10),
    ?assertMatch([#pr{sr=Id}], ets:lookup(Tab, Id)).

% Gets the score of a Ref in the group ------------------------------
test_get_score({Server, _Tab}) ->
    ok = add(Server, fake_pid(4)),
    ?assertEqual(ok, add_score(Server, fake_pid(4), 100.0)),
    ?assertMatch(100.0, get_score(Server, fake_pid(4))).

% Deletes a Ref from the group --------------------------------------
test_del({Server, Tab}) -> 
    ok = add(Server, fake_pid(3)),
    ?assertEqual(ok, del(Server, fake_pid(3))),
    ?assertMatch([], ets:lookup(Tab, {0.0, fake_pid(3)})).


% --------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS ------------------------------------------

% Creates a mock Ref ------------------------------------------------
fake_pid(N) -> 
    c:pid(0, N, 0).

