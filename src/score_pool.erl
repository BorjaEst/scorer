%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(score_pool).
-behaviour(gen_server).

%% API
-export([start_link/1, stop/1, add_score/3, get_score/2]).
-export([top/2, bottom/2, to_list/1]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
    table :: atom()
}).

-define(TAB_CONFIGUTATION, [
    {type, ordered_set},          % Ordered by score
    {attributes, [score_id, id]}  % Table fields
]).
-define(INIT_SCORE, 0.0).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(Name :: atom()) ->
    {ok, Pid :: pid(), term()} | ignore | {error, Reason :: term()}.
start_link(Name) when is_atom(Name) ->
    gen_server:start_link(?MODULE, [Name], []);
start_link(Name) ->
    error({badtype, Name}).

%%--------------------------------------------------------------------
%% @doc Stops the server
%% @end
%%--------------------------------------------------------------------
-spec stop(ServerRef :: pid()) -> ok.
stop(ServerRef) ->
    gen_server:stop(ServerRef).

%%--------------------------------------------------------------------
%% @doc Adds a score to an specific id.
%% @end
%%--------------------------------------------------------------------
-spec add_score(ServerRef, To, Points) -> ok when 
    ServerRef :: pid(),
    To        :: term(),
    Points    :: float().
add_score(ServerRef, To, Points) ->
    gen_server:cast(ServerRef, {add_score, To, Points}).

%%--------------------------------------------------------------------
%% @doc Gets the score of an specific id.
%% @end
%%--------------------------------------------------------------------
-spec get_score(Tab, Of) -> Points when 
    Tab    :: atom(),
    Of     :: term(),
    Points :: float().
get_score(Tab, Of) ->
    case mnesia:dirty_index_read(Tab, Of, id) of 
        [{Tab, {Score, Of}, Of}] -> Score;
        []                       -> error({badarg, Of})
    end.

%%--------------------------------------------------------------------
%% @doc Returns the top N of an score pool in a format {Score, Id}.
%% Ordered from highst to lowest.
%% @end
%%--------------------------------------------------------------------
-spec top(Tab :: atom(), N :: integer()) -> 
    {atomic, [{Score :: float(), Id :: term()}]}.
top(Tab, N) -> 
    Top_Transaction = fun() -> last_n(Tab, mnesia:last(Tab), N) end,
    mnesia:transaction(Top_Transaction).

last_n(_Tab, '$end_of_table',_N) -> [];
last_n( Tab, Key,  N) when N > 0 -> 
    [Key|last_n(Tab, mnesia:prev(Tab,Key), N-1)];
last_n(_Tabl,_Key,_N)            -> [].

%%--------------------------------------------------------------------
%% @doc Returns the N with the lowest score in a format {Score, Id}.
%% Ordered from lowest to highest.
%% @end
%%--------------------------------------------------------------------
-spec bottom(Tab ::atom(), N :: integer()) -> 
    {atomic, [{Score :: float(), Id :: term()}]}.
bottom(Tab, N) -> 
    Bot_Transaction = fun() -> first_n(Tab, mnesia:first(Tab), N) end,
    mnesia:transaction(Bot_Transaction).

first_n(_Tab, '$end_of_table',_N) -> [];
first_n( Tab, Key, N) when N > 0 ->
    [Key|first_n(Tab, mnesia:next(Tab,Key), N-1)];
first_n(_Tab,_Key,_N)            -> [].

%%--------------------------------------------------------------------
%% @doc Returns a list of all table elements in a format {Score, Id}.
%% Ordered from lowest to highest.
%% @endd
%%--------------------------------------------------------------------
-spec to_list(Tab :: atom()) -> 
    {atomic, [{Score :: float(), Id :: term()}]}.
to_list(Tab) -> 
    mnesia:transaction(fun() -> mnesia:all_keys(Tab) end).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([Name]) ->
    {atomic, ok} = mnesia:create_table(Name, ?TAB_CONFIGUTATION),
    {atomic, ok} = mnesia:add_table_index(Name, id), 
    {ok, #state{table = Name}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
        State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(table, _From, State) ->
    {reply, State#state.table, State};
handle_call(Request, _From, _State) ->
    {stop, {unknown_call, Request}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({add_score, To, Points}, State) ->
    do_add_points(State#state.table, To, Points),
    {noreply, State};
handle_cast(Request,_State) ->
    {stop, {unknown_cast, Request}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
        State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
        Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% Adds the specified points to the ref ------------------------------
do_add_points(Tab, Id, Points) -> 
    case mnesia:dirty_index_read(Tab, Id, id) of 
        [{Tab, {OldScore, Id}, Id}] -> 
            NewScore = Points + OldScore,
            Update = update_score(Tab, {OldScore, Id}, NewScore),
            {atomic, ok} = mnesia:transaction(Update);
        [] -> 
            mnesia:dirty_write(Tab, {Tab, {Points, Id}, Id})
    end.

% Transaction: Update score -----------------------------------------
update_score(Tab, {OldScore, Id}, NewScore) -> 
    fun() -> 
        ok = mnesia:delete({Tab, {OldScore, Id}}),
        ok = mnesia:write({Tab, {NewScore, Id}, Id})
    end.


%%====================================================================
%% Eunit white box tests
%%====================================================================

% --------------------------------------------------------------------
% TESTS DESCRIPTIONS -------------------------------------------------

% --------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS -------------------------------------------

% --------------------------------------------------------------------
% ACTUAL TESTS -------------------------------------------------------

% --------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS ------------------------------------------

