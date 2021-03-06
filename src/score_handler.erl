%%%-------------------------------------------------------------------
%%% @author BorjaEst
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(score_handler).
-behaviour(gen_event).

%% API
-export([create_table/2, add_score/3]).

%% gen_event callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_event/2, handle_call/2, handle_info/2]).

-record(state, {
    table :: atom(),
    best  :: {Score :: float(), Id :: term()}
}).
-define(TABLE, State#state.table).
-define( BEST, State#state.best ).

-define(TAB_CONFIGUTATION, [
    {type, ordered_set},          % Ordered by score
    {attributes, [score_id, id]}  % Table fields
]).
-define(INIT_SCORE, 0.0).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Adds an event handler
%% @end
%%--------------------------------------------------------------------
-spec create_table(ScoreMgr :: pid(), Name :: atom()) -> 
    ok | {'EXIT', Reason :: term()} | term().
create_table(ScoreMgr, Name) ->
    gen_event:add_handler(ScoreMgr, ?MODULE, [Name]).

%%--------------------------------------------------------------------
%% @doc Adds a score to an specific id.
%% @end
%%--------------------------------------------------------------------
-spec add_score(ScoreMgr, Id, Points) -> ok when 
    ScoreMgr :: pid(),
    Id       :: term(),
    Points   :: float().
add_score(ScoreMgr, Id, Points) ->
    gen_event:notify(ScoreMgr, {add_score, Id, Points}).


%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%% @end
%%--------------------------------------------------------------------
-spec(init(InitArgs :: term()) ->
    {ok, State :: #state{}} |
    {ok, State :: #state{}, hibernate} |
    {error, Reason :: term()}).
init([Name]) ->
    {atomic, ok} = mnesia:create_table(Name, ?TAB_CONFIGUTATION),
    {atomic, ok} = mnesia:add_table_index(Name, id),
    {ok, #state{table = Name}}.

%%--------------------------------------------------------------------
%% @private
%% @doc Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%% @end
%%--------------------------------------------------------------------
-spec(handle_event(Event :: term(), State :: #state{}) ->
    {ok, NewState :: #state{}} |
    {ok, NewState :: #state{}, hibernate} |
    {swap_handler, Args1 :: term(), NewState :: #state{},
        Handler2 :: (atom() | {atom(), Id :: term()}), Args2 :: term()} |
    remove_handler).
handle_event({add_score, Id, Points}, State) ->
    Score = do_add_points(?TABLE, Id, Points),
    Best  = get_best({Score, Id}, ?BEST),
    {ok, State#state{best = Best}};
handle_event(_Event, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), State :: #state{}) ->
    {ok, Reply :: term(), NewState :: #state{}} |
    {ok, Reply :: term(), NewState :: #state{}, hibernate} |
    {swap_handler, Reply :: term(), Args1 :: term(), NewState :: #state{},
        Handler2 :: (atom() | {atom(), Id :: term()}), Args2 :: term()} |
    {remove_handler, Reply :: term()}).
handle_call(Request, State) ->
    error({unknown_call, Request}),
    {ok, undefined, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: term(), State :: #state{}) ->
    {ok, NewState :: #state{}} |
    {ok, NewState :: #state{}, hibernate} |
    {swap_handler, Args1 :: term(), NewState :: #state{},
        Handler2 :: (atom() | {atom(), Id :: term()}), Args2 :: term()} |
    remove_handler).
handle_info(Info, State) ->
    error({unknown_info, Info}),
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Args :: (term() | {stop, Reason :: term()} | stop |
        remove_handler | {error, {'EXIT', Reason :: term()}} |
    {error, term()}), State :: term()) -> term()).
terminate(_Arg, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
        Extra :: term()) ->
    {ok, NewState :: #state{}}).
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
            Update = update_score(Tab, Id, OldScore, NewScore),
            {atomic, ok} = mnesia:transaction(Update);
        [] -> 
            NewScore = Points,
            mnesia:dirty_write({Tab, {NewScore, Id}, Id})
    end,
    NewScore.

% Transaction: Update score -----------------------------------------
update_score(Tab, Id, OldScore, NewScore) -> 
    fun() -> 
        ok = mnesia:delete({Tab, {OldScore, Id}}),
        ok = mnesia:write({Tab, {NewScore, Id}, Id})
    end.

% Compares two scores and returns de best ---------------------------
get_best(Applicant, Best) when Best >= Applicant -> Best;
get_best(Applicant, Best) when Best <  Applicant -> 
    {Score, Id} = Applicant,
    event_handler:new_champion(self(), Id, Score),
    Applicant.


%%====================================================================
%% Eunit white box tests
%%====================================================================

% -------------------------------------------------------------------
% TESTS DESCRIPTIONS ------------------------------------------------

% -------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS ------------------------------------------

% -------------------------------------------------------------------
% ACTUAL TESTS ------------------------------------------------------

% -------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS -----------------------------------------





