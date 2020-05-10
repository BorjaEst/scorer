%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(scorer).
-author("borja").

%% API
-export([new_group/0, new_pool/1, add_score/2]).
-export_types([]).

-type group() :: term().
-type pool()  :: term().


%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new gen_event for a group which the new handlers.
%% @end
%%--------------------------------------------------------------------
-spec new_group() -> {ok, group()} .
new_group() -> scorer_sup:start_group().

%%--------------------------------------------------------------------
%% @doc Creates a new score table suscribed to the defined groups.
%% @end
%%--------------------------------------------------------------------
-spec new_pool([group()]) -> pool().
new_pool(Groups) -> 
    {ok, Pool} = scorer_sup:start_pool(),
    [ok = score_handler:subscribe(G, Pool) || G <- Groups],
    {ok, Pool}.


%%--------------------------------------------------------------------
%% @doc Creates a new score table suscribed to the defined groups.
%% @end
%%--------------------------------------------------------------------
-spec add_score([group()], Points :: float()) -> ok.
add_score(Groups, Points) -> 
    [ok = score_handler:add_score(G, Points) || G <- Groups],
    ok. 



% join_group(Group) -> The Pid joins a group so when scores, notifies the score servers
% add_score(Points) -> Scores X Points, forwarded to score server which will forward to handlers (tables)


%%====================================================================
%% Internal functions
%%====================================================================


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

