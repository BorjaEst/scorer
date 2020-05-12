%%%-------------------------------------------------------------------
%%% @author BorjaEst
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(score_handler).
-behaviour(gen_event).

%% API
-export([subscribe/2, add_score/3]).

%% gen_event callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_event/2, handle_call/2, handle_info/2]).

-record(state, {
    pool :: pid()
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Adds an event handler
%% @end
%%--------------------------------------------------------------------
-spec subscribe(EventMgrRef :: pid(), pid()) -> 
    ok | {'EXIT', Reason :: term()} | term().
subscribe(EventMgrRef, Pid) ->
    gen_event:add_sup_handler(EventMgrRef, ?MODULE, [Pid]).

%%--------------------------------------------------------------------
%% @doc Adds a score in an specific group
%% @end
%%--------------------------------------------------------------------
-spec add_score(EventMgrRef, Id, Points) -> ok when 
    EventMgrRef :: pid(), 
    Id          :: term(),
    Points      :: float().
add_score(EventMgrRef, Id, Points) ->
    gen_event:notify(EventMgrRef, {add_score, Id, Points}).


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
init([Pid]) ->
    {ok, #state{pool = Pid}}.

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
handle_event({add_score, To, Points}, State) ->
    score_pool:add_score(State#state.pool, To, Points),
    {ok, State};
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
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

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
handle_info(_Info, State) ->
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





