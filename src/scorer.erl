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
-export([new_pool/2, remove_pool/1, add_score/3, get_score/2]).
-export([top/2, bottom/2, to_list/1]).
-export_types([]).

-type group() :: {pid(), group}.
-type pool()  :: {pid(), atom(), pool}.


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
-spec remove_group(group()) -> ok.
remove_group({EventMgrRef, group}) -> 
    try gen_event:stop(EventMgrRef) of 
          ok          -> ok
    catch exit:noproc -> ok
    end.

%%--------------------------------------------------------------------
%% @doc Creates a new score table suscribed to the defined groups.
%% @end
%%--------------------------------------------------------------------
-spec new_pool(Name :: atom(), [group()]) -> pool().
new_pool(Name, Groups) -> 
    {ok, ServerRef} = score_pool:start_link(Name),
    [ok = score_handler:subscribe(EventMgrRef, ServerRef) 
        || {EventMgrRef,group} <- Groups],
    {ok, {ServerRef, Name, pool}}.

%%--------------------------------------------------------------------
%% @doc Removes a pool.
%% @end
%%--------------------------------------------------------------------
-spec remove_pool(pool()) -> ok | {error, Reason :: term()}.
remove_pool({ServerRef, Tab, pool}) -> 
    try score_pool:stop(ServerRef) of 
          ok          -> ok
    catch exit:noproc -> ok
    end,
    case mnesia:delete_table(Tab) of 
        {atomic, ok}            -> ok;
        {aborted,{no_exists,_}} -> ok;
        {aborted, Reason}       -> {error, Reason}
    end.

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
get_score({_ServerRef, Tab, pool}, Id) -> 
    score_pool:get_score(Tab, Id).

%%--------------------------------------------------------------------
%% @doc Returns the top N of an score pool in a format {Score, Id}.
%% Ordered from highst to lowest.
%% @end
%%--------------------------------------------------------------------
-spec top(Pool :: pool(), N :: integer()) -> 
    [{Score :: float(), Id :: term()}].
top({_ServerRef, Tab, pool}, N) -> 
    {atomic, TopN} = score_pool:top(Tab, N),
    TopN.

%%--------------------------------------------------------------------
%% @doc Returns the N with the lowest score in a format {Score, Id}.
%% Ordered from lowest to highest.
%% @end
%%--------------------------------------------------------------------
-spec bottom(Pool :: pool(), N :: integer()) -> 
    [{Score :: float(), Id :: term()}].
bottom({_ServerRef, Tab, pool}, N) -> 
    {atomic, BottomN} = score_pool:bottom(Tab, N),
    BottomN.

%%--------------------------------------------------------------------
%% @doc Returns a list of all table elements in a format {Score, Id}.
%% Ordered from lowest to highest.
%% @end
%%--------------------------------------------------------------------
-spec to_list(Pool :: pool()) -> 
    [{Score :: float(), Id :: term()}].
to_list({_ServerRef, Tab, pool}) -> 
    {atomic, List} = score_pool:to_list(Tab),
    List.

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

