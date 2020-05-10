%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(score_pool).
-compile([export_all, nowarn_export_all]). %% TODO: To delete after buil

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export_type([pool/0]).

-type pool() :: term().

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
    table :: ets:tid()
}).

-define(TAB_CONFIGUTATION, [
    protected,    % Anyone can look but only the owner can write
    ordered_set   % Ordered by score
]).
-define(INIT_SCORE, 0.0).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() ->
    {ok, Pid :: pid(), term()} | ignore | {error, Reason :: term()}.
start_link() ->
    case gen_server:start_link(?MODULE, [], []) of 
        {ok, Pid} -> {ok, Pid, gen_server:call(Pid, tid)};
        Other     -> Other
    end.

%%--------------------------------------------------------------------
%% @doc Adds a score to an specific id.
%% @end
%%--------------------------------------------------------------------
-spec add_score(Pool, To, Points) -> ok when 
    Pool   :: scorer:pool(),
    To     :: pid(),
    Points :: float().
add_score({_,ServerRef,_}, To, Points) ->
    gen_server:cast(ServerRef, {add_score, To, Points}).

%%--------------------------------------------------------------------
%% @doc Returns the top N of an score pool in a format {Id, Score}.
%% @end
%%--------------------------------------------------------------------
-spec top(pool(), N :: integer()) -> 
    [{Score :: float(), Agent_Id :: agent:id()}].
top(Pool, N) -> last_n(Pool, ets:last(Pool), N).

last_n(_Pool, '$end_of_table',_N)       -> [];
last_n( Pool,      ScoreAgent, N) when N > 0 ->
    [ScoreAgent|last_n(Pool, ets:prev(Pool,ScoreAgent), N-1)];
last_n(_Pool,     _ScoreAgent,_N)       -> [].

%%--------------------------------------------------------------------
%% @doc Returns the N agents with the lowest score.
%% @end
%%--------------------------------------------------------------------
-spec bottom(pool(), N :: integer()) -> 
    [{Score :: float(), Agent_Id :: agent:id()}].
bottom(Pool, N) -> first_n(Pool, ets:first(Pool), N).

first_n(_Pool, '$end_of_table',_N)            -> [];
first_n( Pool,      ScoreAgent, N) when N > 0 ->
    [ScoreAgent|first_n(Pool,ets:next(Pool,ScoreAgent), N-1)];
first_n(_Pool,     _ScoreAgent,_N)            -> [].


%%--------------------------------------------------------------------
%% @doc Returns the N agents with the lowest score.
%% @end
%%--------------------------------------------------------------------
-spec to_list(pool()) -> 
    [{Score :: float(), Agent_Id :: agent:id()}].
to_list(Pool) -> lists:reverse(ets:tab2list(Pool)).


















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
init([]) ->
    {ok, #state{
        table = ets:new(no_named, ?TAB_CONFIGUTATION)
    }}.

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

handle_call(tid, _From, State) ->
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

handle_cast({add_score, To, Score}, State) ->

    io:format("Adding ~p points to ~p in pool ~p ~n", [Score, To, self()]),

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

