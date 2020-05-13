scorer
=====

## Dependencies
 - [mnesia](https://erlang.org/doc/man/mnesia.html)

> For correct run, check the mnesia application is running in the background.

## Installation
Create your own project with rebar3.
 ```sh
 $ rebar3 new app yourapp
 ```

Then in your project path find rebar.config file and add enn as dependency under the deps key:
```erlang
{deps, 
    [
        {enn, {git, "https://github.com/BorjaEst/scorer.git", {tag, "<version>"}}}
    ]}.
```

Then using compile command, rebar3 will fetch the defined dependencies and compile them as well for your application.
```sh
$ rebar3 compile
```

At the end for making a release you first need to create your release structure and then making a release with following commands.
```sh
$ rebar3 new release yourrel
$ rebar3 release
```

>You can find more information about dependencies in [rebar3 - dependencies](https://www.rebar3.org/docs/dependencies). 


## Usage
Load the app using your prefered method. For example in the project folder executing  rebar3 shell:
```sh
$ rebar3 shell
===> Booted mnesia
1>
``` 

The application is divided into 2 elements:
 - **groups**: Gen events where the pools can subscribe to receive the score notifications.
 - **pools**: Are a list of {scores, id}, they are subscribed to 1 or more groups from where receive the score updates.

All user functions are defined inside the module [src/scorer](./src/scorer.erl), however here is an example:


### Example
Suppose have 2 groups (g_a, g_b), 3 pools (p_a, p_b, p_ab) and the following relations (target):

- Points to **g_a**, will be reflected in **p_a** and **p_ab**
- Points to **g_b**, will be reflected in **p_b** and **p_ab**

To create the groups run:
```erl
1> {ok, G_a } = scorer:new_group().
{ok,{<0.148.0>,group}}
2> {ok, G_b } = scorer:new_group().
{ok,{<0.150.0>,group}}
```

After the groups are created, you can create the pools indicating the groups they belong to:
```erl
3> {ok, P_a } = scorer:new_pool(p_a,  [G_a]).
{ok,{<0.154.0>,p_a,pool}}
4> {ok, P_b } = scorer:new_pool(p_b,  [G_b]).
{ok,{<0.158.0>,p_b,pool}}
5> {ok, P_ab} = scorer:new_pool(p_ab, [G_a, G_b]).
{ok,{<0.160.0>,p_ab,pool}}
```

Then if an id let's call *john*, scores in the group **g_a** the score will be reflected in the pools **p_a** and **p_ab** but not in **p_b**:

```erl
6> scorer:add_score(G_a, john, 100.0).
ok
7> scorer:get_score(P_a, john).
100.0
8> scorer:get_score(P_ab, john).
100.0
9> catch scorer:get_score(P_b, john).
{'EXIT',{{badarg,john},
         [{score_pool,get_score,2,
                      [{file,"/home/borja/Projects/scorer/src/score_pool.erl"},
                       {line,74}]},
          {erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,684}]},
          {erl_eval,expr,5,[{file,"erl_eval.erl"},{line,437}]},
          {shell,exprs,7,[{file,"shell.erl"},{line,686}]},
          {shell,eval_exprs,7,[{file,"shell.erl"},{line,642}]},
          {shell,eval_loop,3,[{file,"shell.erl"},{line,627}]}]}}
```

Now imagine that a lot of users are scoring in **g_a** and **g_b**. You can get the top, bottom or a list using the functions `top/2`, `bottom/2` and `to_list/1`:

Let's add some random scores to some numeric users:
```erl 
10> [scorer:add_score(G_a, 10 + rand:uniform(9), 1.0) || _ <- lists:seq(1, 9)].
[ok,ok,ok,ok,ok,ok,ok,ok,ok]
11> [scorer:add_score(G_b, 20 + rand:uniform(9), 1.0) || _ <- lists:seq(1, 9)].
[ok,ok,ok,ok,ok,ok,ok,ok,ok]
```

After that, we can get the best and the worse ones from the specified pool:
```erl
12> Best3_A    = scorer:top(P_a, 3).
[{100.0,john},{2.0,17},{2.0,14}]
13> Worst3_A   = scorer:bottom(P_a, 3).
[{1.0,11},{1.0,13},{1.0,19}]
```

If we query the pool **p_ab** we will get the scores of all the groups together:
```erl
14> Best3_All  = scorer:top(P_ab, 3).
[{100.0,john},{3.0,24},{2.0,29}]
15> Worst3_All = scorer:bottom(P_ab, 3).
[{1.0,11},{1.0,13},{1.0,19}]
```

Also it is possible to get the complete list sorted from lowest score to highest:
```erl
16> All = scorer:to_list(P_ab).
[{1.0,11}, {1.0,13}, {1.0,19}, {1.0,23}, {1.0,27}, {2.0,12}, {2.0,14}, {2.0,17}, {2.0,28}, {2.0,29}, {3.0,24}, {100.0,john}]
```


## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.


### Improvement ideas and requests
The main idea behind is to provide a flexible score system for other applications (for example evolutionary algorithms).

It is running behing mnesia to provide an automatic management of second index and transactions. However similar results might be achieved using 2 ets tables. Feel free tro try and discuss.

## License
This software is under [GPL-3.0](https://www.gnu.org/licenses/gpl-3.0.en.html) license.


