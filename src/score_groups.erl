%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(score_groups).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([start/0, join/2, subscribed/1]).

-define(TAB, score_groups).
-define(TAB_CONFIGUTATION, [
    named_table,  % The table should be known to everyone
    public,       % Anyone can subscribe
    bag           % Subscribe should not need an update
]).
-define(INIT_SCORE, 0.0).


%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the groups pool ets table.
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok.
start() -> 
    ?TAB = ets:new(?TAB, ?TAB_CONFIGUTATION),
    ok.

%%--------------------------------------------------------------------
%% @doc Creates a new gen_event for a group which the new handlers.
%% @end
%%--------------------------------------------------------------------
-spec join(scorer:group(), Who :: pid()) -> ok.
join(Group, Who) -> 
    true = ets:insert(?TAB, {Who, Group}),
    ok.

%%--------------------------------------------------------------------
%% @doc Creates a new gen_event for a group which the new handlers.
%% @end
%%--------------------------------------------------------------------
-spec subscribed(Who :: pid()) -> [scorer:group()].
subscribed(Who) -> 
    [Group || {_,Group} <- ets:lookup(?TAB, Who)].


%%====================================================================
%% Internal functions
%%====================================================================


%%====================================================================
%% Eunit white box tests
%%====================================================================

% -------------------------------------------------------------------
% TESTS DESCRIPTIONS ------------------------------------------------
join_and_check_is_subscribed_test_() ->
    {"A process joins a group and checks it is subscribed -------",
      {setup, local, fun start_table/0, fun nothing/1,
       {inorder, [
           {"Call for joining group 'a'--------------------------", 
            ?_assert(ok == join(a, self()))},
           {"Check subscriptio to group 'a'----------------------",
            ?_assert(lists:member(a, subscribed(self())))}
       ]}}}.

% -------------------------------------------------------------------
% SPECIFIC SETUP FUNCTIONS ------------------------------------------
start_table() -> ok = start().
nothing(_)    -> ok.

% -------------------------------------------------------------------
% ACTUAL TESTS ------------------------------------------------------

% -------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS -----------------------------------------

