%%%-------------------------------------------------------------------
%% @doc scorer top level supervisor.
%% @end
%%%-------------------------------------------------------------------
-module(scorer_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([start_group/0, stop_group/1]).
-export([ start_pool/0,  stop_pool/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

-define(SPECS_SCORE_SERVER(Id), #{
    id       => Id,
    start    => {gen_event, start_link, []},
    restart  => permanent,
    shutdown => 500,
    modules  => [gen_event]
}).

-define(SPECS_SCORE_POOL(Id), #{
    id       => Id,
    start    => {score_pool, start_link, []},
    restart  => permanent,
    shutdown => 500,
    modules  => [gen_server]
}).


%%====================================================================
%% API functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the supervisor.
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc Starts a score server (group) under the supervisor. 
%% @end
%%--------------------------------------------------------------------
start_group() ->
    Id = make_ref(),
    case supervisor:start_child(?SERVER, ?SPECS_SCORE_SERVER(Id)) of 
        {ok, Pid} -> {ok, {Id, Pid}};
        Other     -> Other
    end.

%%--------------------------------------------------------------------
%% @doc Starts a score server (group) under the supervisor. 
%% @end
%%--------------------------------------------------------------------
stop_group({Id, _}) ->
    supervisor:terminate_child(?SERVER, Id).

%%--------------------------------------------------------------------
%% @doc Starts a score server (group) under the supervisor. 
%% @end
%%--------------------------------------------------------------------
start_pool() ->
    Id = make_ref(),
    case supervisor:start_child(?SERVER, ?SPECS_SCORE_POOL(Id)) of 
        {ok, Pid, Tid} -> {ok, {Id, Pid, Tid}};
        Other          -> Other
    end.

%%--------------------------------------------------------------------
%% @doc Starts a score server (group) under the supervisor. 
%% @end
%%--------------------------------------------------------------------
stop_pool({Id, _}) ->
    supervisor:terminate_child(?SERVER, Id).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.


%%====================================================================
%% Internal functions
%%====================================================================

