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
-export([new_pool/2, remove_pool/1, add_score/2, get_score/2]).
-export([top/2, bottom/2, to_list/1, subscribe/1]).
-export_types([score/0]).

-type score() :: float().
-record(group, {sup::pid(), mgr::pid(), srv::pid(), tab::ets:tid()}).
-type group() :: #group{}.
-record(pool, {mgr::pid(), tab::atom()}).
-type pool()  :: #pool{}.


%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new gen_event for a group to run the new handlers.
%% @end
%%--------------------------------------------------------------------
-spec new_group() -> group().
new_group() -> 
    case group_sup:start_link() of 
        {ok, Supervisor} -> 
            GroupMgr = group_sup:start_eventmng(Supervisor),
            GroupSrv = group_sup:start_groupsrv(Supervisor),
            GroupTab = group_pool:tab(GroupSrv),
            #group{sup = Supervisor, mgr = GroupMgr, 
                   srv = GroupSrv,   tab = GroupTab};
        {error, Reason}  -> 
            error(Reason)
    end.

%%--------------------------------------------------------------------
%% @doc Removes a group.
%% @end
%%--------------------------------------------------------------------
-spec remove_group(group()) -> ok.
remove_group(#group{sup=Supervisor}) ->
    exit(Supervisor, normal),
    ok.

%%--------------------------------------------------------------------
%% @doc Creates a new score table suscribed to the defined groups.
%% @end
%%--------------------------------------------------------------------
-spec new_pool(Name::atom(), [group()]) -> pool().
new_pool(Name, Groups) -> 
    {ok, PoolMgr} = gen_event:start_link(),
    ok = score_handler:create_table(PoolMgr, Name),
    [ok = group_handler:subscribe(G#group.mgr, PoolMgr) || G <- Groups],
    #pool{mgr=PoolMgr, tab=Name}.

%%--------------------------------------------------------------------
%% @doc Removes a pool.
%% @end
%%--------------------------------------------------------------------
-spec remove_pool(pool()) -> ok | {error, Reason::term()}.
remove_pool(#pool{}=P) -> 
    try gen_event:stop(P#pool.mgr) of 
          ok          -> ok
    catch exit:noproc -> ok
    end,
    case mnesia:delete_table(P#pool.tab) of 
        {atomic, ok}            -> ok;
        {aborted,{no_exists,_}} -> ok;
        {aborted, Reason}       -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Add the points to the pid in a group.
%% @end
%%--------------------------------------------------------------------
-spec add_score(group(), {Id, score()}) -> Id when Id::term().
add_score(         _, {Id,   0.0}) -> Id;
add_score(#group{}=G, {Id, Score}) -> 
    group_pool:add_score(G#group.srv, Score),
    group_handler:add_score(G#group.mgr, Id, Score),
    Id. 

%%--------------------------------------------------------------------
%% @doc Gets the pid score in a pool.
%% @end
%%--------------------------------------------------------------------
-spec get_score(group()|pool(), Id) -> Id when Id::term().
get_score(#group{}=G, Id) -> 
    case group_pool:get_score(G#group.srv, Id) of
        Score when is_float(Score) -> Score;
        {error, not_found}         -> error({badarg, Id})
    end;
get_score(#pool{}=P, Id) -> 
    case mnesia:dirty_index_read(P#pool.tab, Id, id) of 
        [{_, {Score, Id}, Id}] -> Score;
        []                     -> error({badarg, Id})
    end.

%%--------------------------------------------------------------------
%% @doc Returns the top N of an score pool in a format {Score, Id}.
%% Ordered from highst to lowest.
%% @end
%%--------------------------------------------------------------------
-spec top(group()|pool(), N::integer()) -> [{score(),Id::term()}].
top(#group{}=G, N) -> 
    last_n(ets, G#group.tab, N);
top( #pool{}=P, N) -> 
    Transaction = fun() -> last_n(mnesia, P#pool.tab, N) end,
    {atomic, TopN} = mnesia:transaction(Transaction),
    TopN.

%%--------------------------------------------------------------------
%% @doc Returns the N with the lowest score in a format {Score, Id}.
%% Ordered from lowest to highest.
%% @end
%%--------------------------------------------------------------------
-spec bottom(group()|pool(), N::integer()) -> [{score(),Id::term()}].
bottom(#group{}=G, N) -> 
    first_n(ets, G#group.tab, N);
bottom( #pool{}=P, N) -> 
    Transaction = fun() -> first_n(mnesia, P#pool.tab, N) end,
    {atomic, BottomN} = mnesia:transaction(Transaction),
    BottomN.

%%--------------------------------------------------------------------
%% @doc Returns a list of all table elements in a format {Score, Id}.
%% Ordered from lowest to highest.
%% @end
%%--------------------------------------------------------------------
-spec to_list(group()|pool()) -> [{score(),Id::term()}].
to_list(#group{}=G) -> 
    ets:tab2list(G#group.tab);
to_list( #pool{}=P) -> 
    List_Transaction = fun() -> mnesia:all_keys(P#pool.tab) end,
    {atomic, List} = mnesia:transaction(List_Transaction),
    List.

%%--------------------------------------------------------------------
%% @doc Subscribes to the pool events. The subscriber will receive the
%% following messages in these contexts:
%%   - {new_best, Pool, {Id, Score}} -> When new best score.
%% @end
%%--------------------------------------------------------------------
-spec subscribe(pool()) -> ok.
subscribe(#pool{}=P) -> 
    event_handler:subscribe(P#pool.mgr, P#pool.tab, self()).


%%====================================================================
%% Internal functions
%%====================================================================

% Reads the last n elements from a table ----------------------------
first_n(mnesia, Tab, N) -> get_n(Tab, mnesia:first(Tab), N, fun mnesia:next/2); 
first_n(   ets, Tab, N) -> get_n(Tab,    ets:first(Tab), N, fun    ets:next/2).

% Reads the last n elements from a table ----------------------------
last_n(mnesia, Tab, N) -> get_n(Tab, mnesia:last(Tab), N, fun mnesia:prev/2); 
last_n(   ets, Tab, N) -> get_n(Tab,    ets:last(Tab), N, fun    ets:prev/2).

% Goes over ETS/Mnesia table using the function F -------------------
get_n(_T, K,_N,_F) when K=='$end_of_table' -> [];
get_n( T, K, N, F) when N > 0              -> [K|get_n(T,F(T,K),N-1, F)];
get_n(_T,_K,_N,_F)                         -> [].


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

