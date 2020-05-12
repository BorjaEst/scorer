%%%-------------------------------------------------------------------
%%% Author  :
%%% Description :
%%% Created :
%%%-------------------------------------------------------------------
-module(scorer_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").

-define(HEAD, [$- || _ <-lists:seq(1,80)] ++ "\n").
-define(HEAD(Text), ct:log(?LOW_IMPORTANCE, ?HEAD ++ "~p", [Text])).
-define(END,  [$_ || _ <-lists:seq(1,80)]).
-define(END(V), ct:log(?LOW_IMPORTANCE, "~p~n" ++ ?END, [V]), V).

-define(INFO(A,B),    ct:log(?LOW_IMPORTANCE,    "~p: ~p",   [A,B])).
-define(ERROR(Error), ct:pal( ?HI_IMPORTANCE, "Error: ~p", [Error])).


%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {seconds, 8}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ok = application:start(scorer),
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok = application:stop(scorer),
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    G_a  = single_group(),
    G_b  = single_group(),
    G_ab = single_group(),
    P_a  = create_pool([G_a, G_ab]),
    P_b  = create_pool([G_b, G_ab]),
    [{g_a, G_a}, {g_b, G_b}, {g_ab, G_ab},
     {p_a, P_a}, {p_b, P_b} | Config].

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, Config) ->
    ok = delete_group(?config( g_a, Config)),
    ok = delete_group(?config( g_b, Config)),
    ok = delete_group(?config(g_ab, Config)),
    ok = delete_pool(?config(p_a, Config)),
    ok = delete_pool(?config(p_b, Config)), 
    ok.

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------
all() ->
    [
        add_score_updates_score_in_simple_group,
        add_score_updates_in_combined_group,
        remove_group_in_combined_group,
        remove_pool_in_combined_group
    ].

%%--------------------------------------------------------------------
%% Function: TestCase() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
my_test_case_example() ->
    [].

%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%%--------------------------------------------------------------------
my_test_case_example(_Config) ->
    ok.

% --------------------------------------------------------------------
% TESTS --------------------------------------------------------------

% -------------------------------------------------------------------
add_score_updates_score_in_simple_group(Config) -> 
    ok = scorer:add_score(?config(g_a, Config), self(), 100.0),
    timer:sleep(10),
    true = 100.0 == scorer:get_score(?config(p_a, Config), self()).


% -------------------------------------------------------------------
add_score_updates_in_combined_group(Config) -> 
    ok = scorer:add_score(?config(g_ab, Config), self(), 100.0),
    timer:sleep(10),
    true = 100.0 == scorer:get_score(?config(p_a, Config), self()),
    true = 100.0 == scorer:get_score(?config(p_b, Config), self()).

% -------------------------------------------------------------------
remove_group_in_combined_group(Config) -> 
    ok = scorer:remove_group(?config(g_ab, Config)),
    timer:sleep(10),
    ok = scorer:add_score(?config(g_a, Config), self(), 100.0),
    try scorer:get_score(?config(p_a, Config), self()) of 
          _                -> error(test_failed)
    catch error:{badarg,_} -> ok
    end.

% -------------------------------------------------------------------
remove_pool_in_combined_group(Config) -> 
    ok = scorer:remove_pool(?config(p_a, Config)),
    ok = scorer:add_score(?config(g_ab, Config), self(), 100.0),
    timer:sleep(10),
    true = 100.0 == scorer:get_score(?config(p_b, Config), self()),
    try scorer:get_score(?config(p_a, Config), self()) of 
          _            -> error(test_failed)
    catch error:badarg -> ok
    end.
    

% --------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS ------------------------------------------

% Creates a single group --------------------------------------------
single_group() -> 
    {ok, Group} = scorer:new_group(),
    ?INFO("Group: ", Group),
    Group.

% Greates a pool attached to the indicated groups -------------------
create_pool(Groups) -> 
    {ok, Pool} = scorer:new_pool(Groups),
    ?INFO("{Pool, Groups}: ", {Pool, Groups}),
    Pool.

% Terminates a group ------------------------------------------------
delete_group(Group) -> 
    ok = scorer:remove_group(Group),
    ?INFO("Removed group: ", Group),
    ok.

% Terminates a pool -------------------------------------------------
delete_pool(Pool) -> 
    ok = scorer:remove_pool(Pool),
    ?INFO("Removed pool: ", Pool),
    ok.


% --------------------------------------------------------------------
% RESULTS CONSOLE PRINT ----------------------------------------------

