%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
-module(group_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_eventmng/1, start_groupsrv/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SPECS_EVENT_MNG, #{
    id       => {event_mng, make_ref()},
    start    => {gen_event, start_link, []},
    restart  => permanent,
    shutdown => 500,
    modules  => [gen_event]
 }).
-define(SPECS_GROUP_SRV, #{
    id       => {group_pool, make_ref()},
    start    => {group_pool, start_link, []},
    restart  => permanent,
    shutdown => 500,
    modules  => [gen_server, group_pool]
 }).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the supervisor.
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
start_link() ->
    supervisor:start_link(?MODULE, []).

%%--------------------------------------------------------------------
%% @doc Starts the neural network cortex
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
start_eventmng(Supervisor) ->
    {ok, Pid} = supervisor:start_child(Supervisor, ?SPECS_EVENT_MNG),
    Pid.

%%--------------------------------------------------------------------
%% @doc Starts the neural network cortex
%% @end
%%--------------------------------------------------------------------
% TODO: To make description and specs
start_groupsrv(Supervisor) ->
    {ok, Pid} = supervisor:start_child(Supervisor, ?SPECS_GROUP_SRV),
    Pid.


%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{strategy  => one_for_all, %% All down if one down
                 intensity => 0,   %% Restart is not allowed
                 period    => 10}, %% Any as intensity = 0
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
