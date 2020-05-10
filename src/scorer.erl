%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(scorer).
-author("borja").

%% API
-export([new_group/0, new_pool/1, join/1]). 
-export([add_score/1, add_score/2, get_score/2]).
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
%% @doc The process joins the specified group.
%% @end
%%--------------------------------------------------------------------
-spec join(group()) -> ok.
join(Group) -> 
    score_groups:join(Group, self()).

%%--------------------------------------------------------------------
%% @doc Creates a new score table suscribed to the defined groups.
%% @end
%%--------------------------------------------------------------------
-spec add_score(Points :: float()) -> ok.
add_score(Points) -> 
    Subscribed_Groups = score_groups:subscribed(self()),
    [ok = add_score(G, Points) || G <- Subscribed_Groups],
    ok.

-spec add_score(group(), Points :: float()) -> ok.
add_score(Group, Points) -> 
    score_handler:add_score(Group, Points). 

%%--------------------------------------------------------------------
%% @doc Creates a new score table suscribed to the defined groups.
%% @end
%%--------------------------------------------------------------------
-spec get_score(pool(), Of :: pid()) -> Score :: float().
get_score(Pool, Pid) -> 
    score_pool:get_score(Pool, Pid).


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

