%%%-------------------------------------------------------------------
%%% @author borja
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(scorer).
-author("borja").

%% API
-export([new_group/0, remove_group/1]). 
-export([new_pool/1, remove_pool/1, add_score/3, get_score/2]).
-export_types([]).

-type group() :: {pid(), group}.
-type pool()  :: {pid(), ets:tid(), pool}.


%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new gen_event for a group to run the new handlers.
%% @end
%%--------------------------------------------------------------------
-spec new_group() -> {ok, group()} .
new_group() -> 
    {ok, EventMgrRef} = gen_event:start_link(),
    {ok, {EventMgrRef, group}}.

%%--------------------------------------------------------------------
%% @doc Removes a group.
%% @end
%%--------------------------------------------------------------------
-spec remove_group(group()) -> true.
remove_group({EventMgrRef, group}) -> 
    exit(EventMgrRef, normal).

%%--------------------------------------------------------------------
%% @doc Creates a new score table suscribed to the defined groups.
%% @end
%%--------------------------------------------------------------------
-spec new_pool([group()]) -> pool().
new_pool(Groups) -> 
    {ok, ServerRef, Tid} = score_pool:start_link(),
    [ok = score_handler:subscribe(EventMgrRef, ServerRef) 
        || {EventMgrRef,group} <- Groups],
    {ok, {ServerRef, Tid, pool}}.

%%--------------------------------------------------------------------
%% @doc Removes a pool.
%% @end
%%--------------------------------------------------------------------
-spec remove_pool(pool()) -> true.
remove_pool({ServerRef, _Tid, pool}) -> 
    unlink(ServerRef),  % Table not deleted if exit(SerRef, normal) 
    exit(ServerRef, terminate). 
    % exit(ServerRef, normal).

%%--------------------------------------------------------------------
%% @doc Add the points to the pid in a group.
%% @end
%%--------------------------------------------------------------------
-spec add_score(group(), Id :: term(), Points :: float()) -> ok.
add_score({EventMgrRef, group}, Id, Points) -> 
    score_handler:add_score(EventMgrRef, Id, Points). 

%%--------------------------------------------------------------------
%% @doc Gets the pid score in a pool.
%% @end
%%--------------------------------------------------------------------
-spec get_score(pool(), Of :: term()) -> Score :: float().
get_score({_ServerRef, Tid, pool}, Id) -> 
    score_pool:get_score(Tid, Id).


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

