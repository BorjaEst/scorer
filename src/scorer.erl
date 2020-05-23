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
-export([top/2, bottom/2, to_list/1, subscribe/1]).
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
    {ok, GroupMgr} = gen_event:start_link(),
    {ok, {GroupMgr, group}}.

%%--------------------------------------------------------------------
%% @doc Removes a group.
%% @end
%%--------------------------------------------------------------------
-spec remove_group(group()) -> ok.
remove_group({GroupMgr, group}) -> 
    try gen_event:stop(GroupMgr) of 
          ok          -> ok
    catch exit:noproc -> ok
    end.

%%--------------------------------------------------------------------
%% @doc Creates a new score table suscribed to the defined groups.
%% @end
%%--------------------------------------------------------------------
-spec new_pool(Name :: atom(), [group()]) -> pool().
new_pool(Name, Groups) -> 
    {ok, ScoreMgr} = gen_event:start_link(),
    ok = score_handler:create_table(ScoreMgr, Name),
    [ok = group_handler:subscribe(X, ScoreMgr) || {X,group} <- Groups],
    {ok, {ScoreMgr, Name, pool}}.

%%--------------------------------------------------------------------
%% @doc Removes a pool.
%% @end
%%--------------------------------------------------------------------
-spec remove_pool(pool()) -> ok | {error, Reason :: term()}.
remove_pool({ScoreMgr, Tab, pool}) -> 
    try gen_event:stop(ScoreMgr) of 
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
add_score(_, _, 0.0) -> ok;
add_score({ScoreMgr, group}, Id, Points) -> 
    group_handler:add_score(ScoreMgr, Id, Points). 

%%--------------------------------------------------------------------
%% @doc Gets the pid score in a pool.
%% @end
%%--------------------------------------------------------------------
-spec get_score(pool(), Of :: term()) -> Score :: float().
get_score({_ScoreMgr, Tab, pool}, Id) -> 
    case mnesia:dirty_index_read(Tab, Id, id) of 
        [{Tab, {Score, Id}, Id}] -> Score;
        []                       -> error({badarg, Id})
    end.

%%--------------------------------------------------------------------
%% @doc Returns the top N of an score pool in a format {Score, Id}.
%% Ordered from highst to lowest.
%% @end
%%--------------------------------------------------------------------
-spec top(Pool :: pool(), N :: integer()) -> 
    [{Score :: float(), Id :: term()}].
top({_ScoreMgr, Tab, pool}, N) -> 
    Top_Transaction = fun() -> last_n(Tab, mnesia:last(Tab), N) end,
    {atomic, TopN} = mnesia:transaction(Top_Transaction),
    TopN.

%%--------------------------------------------------------------------
%% @doc Returns the N with the lowest score in a format {Score, Id}.
%% Ordered from lowest to highest.
%% @end
%%--------------------------------------------------------------------
-spec bottom(Pool :: pool(), N :: integer()) -> 
    [{Score :: float(), Id :: term()}].
bottom({_ScoreMgr, Tab, pool}, N) -> 
    Bot_Transaction = fun() -> first_n(Tab, mnesia:first(Tab), N) end,
    {atomic, BottomN} = mnesia:transaction(Bot_Transaction),
    BottomN.

%%--------------------------------------------------------------------
%% @doc Returns a list of all table elements in a format {Score, Id}.
%% Ordered from lowest to highest.
%% @end
%%--------------------------------------------------------------------
-spec to_list(Pool :: pool()) -> 
    [{Score :: float(), Id :: term()}].
to_list({_ScoreMgr, Tab, pool}) -> 
    List_Transaction = fun() -> mnesia:all_keys(Tab) end,
    {atomic, List} = mnesia:transaction(List_Transaction),
    List.

%%--------------------------------------------------------------------
%% @doc Subscribes to the pool events. The subscriber will receive the
%% following messages in these contexts:
%%   - {new_best, Pool, {Id, Score}} -> When new best score.
%% @end
%%--------------------------------------------------------------------
-spec subscribe(Pool :: pool()) -> ok.
subscribe({ScoreMgr, _Tab, pool} = Pool) -> 
    event_handler:subscribe(ScoreMgr, Pool, self()).


%%====================================================================
%% Internal functions
%%====================================================================

% Reads the last n elements from a table ----------------------------
last_n(_Tab, '$end_of_table',_N) -> [];
last_n( Tab, Key,  N) when N > 0 -> 
    [Key|last_n(Tab, mnesia:prev(Tab,Key), N-1)];
last_n(_Tabl,_Key,_N)            -> [].

% Reads the first n elements from a table ---------------------------
first_n(_Tab, '$end_of_table',_N) -> [];
first_n( Tab, Key, N) when N > 0 ->
    [Key|first_n(Tab, mnesia:next(Tab,Key), N-1)];
first_n(_Tab,_Key,_N)            -> [].


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

