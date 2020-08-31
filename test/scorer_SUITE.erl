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

-define(N_PARALLEL_IDS, 5).

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
    application:start(mnesia),
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    application:stop(mnesia),
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
    P_a  = create_pool(p_a, [G_a, G_ab]),
    P_b  = create_pool(p_b, [G_b, G_ab]),
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
    [
        {score_tests, [sequence], 
          [
            add_score_updates_score_in_simple_group,
            add_score_updates_in_combined_group,
            remove_group_in_combined_group,
            remove_pool_in_combined_group,
            group_is_automatically_down_when_process_down,
            pool_is_automatically_down_when_process_down,
            get_top_3_from_highest_to_lowest_order,
            get_bottom_3_from_lowest_to_highest_order,
            get_a_list_of_all_scores_from_lowest_to_highest_order,
            subscription_to_pool_receives_event_messages,
            parallel_scoring
          ]
        }
    ].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------
all() ->
    [
        {group, score_tests}
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
    scorer:add_score(?config(g_a, Config), {self(), 100.0}),
    timer:sleep(10),
    true = 100.0 == scorer:get_score(?config(g_a, Config), self()),
    true = 100.0 == scorer:get_score(?config(p_a, Config), self()).

% -------------------------------------------------------------------
add_score_updates_in_combined_group(Config) -> 
    scorer:add_score(?config(g_ab, Config), {self(), 100.0}),
    timer:sleep(10),
    true = 100.0 == scorer:get_score(?config(p_a, Config), self()),
    true = 100.0 == scorer:get_score(?config(p_b, Config), self()).

% -------------------------------------------------------------------
remove_group_in_combined_group(Config) -> 
    scorer:remove_group(?config(g_ab, Config)),
    timer:sleep(10),
    scorer:add_score(?config(g_a, Config), {self(), 100.0}),
    try scorer:get_score(?config(p_a, Config), self()) of 
          _                -> error(test_failed)
    catch error:{badarg,_} -> ok
    end.

% -------------------------------------------------------------------
remove_pool_in_combined_group(Config) -> 
    scorer:remove_pool(?config(p_a, Config)),
    scorer:add_score(?config(g_ab, Config), {self(), 100.0}),
    timer:sleep(10),
    true = 100.0 == scorer:get_score(?config(p_b, Config), self()),
    try scorer:get_score(?config(p_a, Config), self()) of 
          _                            -> error(test_failed)
    catch exit:{aborted,{no_exists,_}} -> ok
    end.

% -------------------------------------------------------------------
group_is_automatically_down_when_process_down(_Config) -> 
    Parent = self(),
    Fun = fun() -> Parent ! single_group(), receive _ -> ok end end,
    Child = spawn(Fun),
    Group = receive Msg -> Msg end,
    Pool  = create_pool(test1, [Group]),
    exit(Child, exception_raise),
    timer:sleep(10),
    scorer:add_score(Group, {self(), 100.0}),
    try scorer:get_score(Pool, self()) of 
          _                -> error(test_failed)
    catch error:{badarg,_} -> ok
    end. 

% -------------------------------------------------------------------
pool_is_automatically_down_when_process_down(_Config) -> 
    Group  = single_group(),
    Parent = self(),
    Fun = fun() -> Parent ! create_pool(test2, [Group]), receive _ -> ok end end,
    Child = spawn(Fun),
    Pool = receive Msg -> Msg end,
    exit(Child, exception_raise),
    timer:sleep(10),
    scorer:add_score(Group, {self(), 100.0}),
    try scorer:get_score(Pool, self()) of 
          _                -> error(test_failed)
    catch error:{badarg,_} -> ok
    end. 

% -------------------------------------------------------------------
get_top_3_from_highest_to_lowest_order(Config) -> 
    [rand_score(?config(g_a, Config)) || _ <- lists:seq(1, 40)],
    timer:sleep(10),
    Top3 = scorer:top(?config(p_a, Config), 3),
    3 = length(Top3),
    check_order_highest_to_lowest(Top3),
    ok.

% -------------------------------------------------------------------
get_bottom_3_from_lowest_to_highest_order(Config) -> 
    [rand_score(?config(g_a, Config)) || _ <- lists:seq(1, 40)],
    timer:sleep(10),
    Bottom3 = scorer:bottom(?config(p_a, Config), 3),
    3 = length(Bottom3),
    check_order_lowest_to_higher(Bottom3),
    ok.

% -------------------------------------------------------------------
get_a_list_of_all_scores_from_lowest_to_highest_order(Config) -> 
    [rand_score(?config(g_a, Config)) || _ <- lists:seq(1, 40)],
    timer:sleep(10),
    List = scorer:to_list(?config(p_a, Config)),
    ?N_PARALLEL_IDS = length(List),
    check_order_lowest_to_higher(List),
    ok.

% -------------------------------------------------------------------
subscription_to_pool_receives_event_messages(Config) -> 
    Pool = ?config(p_a, Config),
    scorer:subscribe(Pool),
    scorer:add_score(?config(g_a, Config), {a1, 1.0}),
    scorer:add_score(?config(g_a, Config), {a2, 2.0}),
    scorer:add_score(?config(g_a, Config), {a3, 1.0}),
    scorer:add_score(?config(g_a, Config), {a4, 3.0}),
    scorer:add_score(?config(g_a, Config), {a1, 1.0}),
    scorer:add_score(?config(g_a, Config), {a2, 2.0}),
    timer:sleep(10),
    Inbox = read_inbox(),
    ?INFO("Inbox: ", Inbox),
    [{new_best,p_a,{a1,1.0}},
     {new_best,p_a,{a2,2.0}},
     {new_best,p_a,{a4,3.0}},
     {new_best,p_a,{a2,4.0}}] = Inbox,
    ok.

% -------------------------------------------------------------------
parallel_scoring(Config) -> 
    Score_In_A = fun() -> rand_score(?config(g_a, Config)) end,
    Score_In_B = fun() -> rand_score(?config(g_a, Config)) end,
    [spawn(Score_In_A) || _ <- lists:seq(1, 40)],
    [spawn(Score_In_B) || _ <- lists:seq(1, 40)],
    ok.

% --------------------------------------------------------------------
% SPECIFIC HELPER FUNCTIONS ------------------------------------------

% Creates a single group --------------------------------------------
single_group() -> 
    Group = scorer:new_group(),
    ?INFO("Group: ", Group),
    Group.

% Greates a pool attached to the indicated groups -------------------
create_pool(Name, Groups) -> 
    Pool = scorer:new_pool(Name, Groups),
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

% Checks the list is in desc order ----------------------------------
check_order_highest_to_lowest(List) -> 
    ?INFO("Check desc order in: ", List),
    Reversed = lists:reverse(List),
    Reversed = lists:sort(List).

% Checks the list is in ascendent order -----------------------------
check_order_lowest_to_higher(List) -> 
    ?INFO("Check ascendent order in: ", List),
    List = lists:sort(List).

% Random scoring ----------------------------------------------------
rand_score(Group) -> 
    Id = rand:uniform(?N_PARALLEL_IDS),
    ?INFO("Scoring 1.0 points in {Group,For}: ", {Group,Id}),
    scorer:add_score(Group, {Id, 1.0}).


% --------------------------------------------------------------------
% RESULTS CONSOLE PRINT ----------------------------------------------

% Reads all the messages in the inbox -------------------------------
read_inbox() -> 
    receive X -> [X|read_inbox()]
    after   0 -> []
    end.

