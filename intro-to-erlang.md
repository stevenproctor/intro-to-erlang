# Intro to Erlang

---

## About Me

- Founder and Organizer of the Dallas/Fort Worth Erlang User Group
- Functional Geekery (http://www.functionalgeekery.com/)
- Planet Erlang (http://www.planeterlang.com/)

---

## Background


- Created by Joe Armstrong, Robert Virding, and Mike Williams
- Born in Ericsson for telecom switches in 1986
- Open Sourced in 1998

---

# Code Time!

---

## The Erlang Shell

```
$ erl
Erlang/OTP 17 [erts-6.2.1] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V6.2.1  (abort with ^G)
1>
```

---

## The Erlang Shell
### Quitting

```
$ erl
Erlang/OTP 17 [erts-6.2.1] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V6.2.1  (abort with ^G)
11>
User switch command
 --> ?
  c [nn]            - connect to job
  i [nn]            - interrupt job
  k [nn]            - kill job
  j                 - list all jobs
  s [shell]         - start local shell
  r [node [shell]]  - start remote shell
  q                 - quit erlang
  ? | h             - this message
 -->
```

---

## The Erlang Shell
### Quitting

```
$ erl
Erlang/OTP 17 [erts-6.2.1] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V6.2.1  (abort with ^G)
1> q().
ok
2> $
```

---

## Small Syntax

- Predefined Data Types a.k.a Terms
- Pattern Matching
- Variables (but not really)
- Function Clauses
- Modules

---

## Data Types

- Immutable
- Limited Set

---

## Data Types

<div style="column-count: 2; -moz-column-count: 2">
<ul>
<li>Number</li>
<li>Atom</li>
<li>Bit strings and Binaries</li>
<li>Reference</li>
<li>Function Identifier</li>
</ul>
<ul>
<li>Port Identifier</li>
<li>Pid</li>
<li>List</li>
<li>Tuple</li>
<li>Map</li>
</ul>
</div>

---

## Where are my...

### Booleans?

Atoms. [frag=1]

- [frag=2] `true`
- [frag=3] `false`

---

## Where are my...

### Strings?

Lists of Integers. [frag=1]

```erlang
[72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100].
% "Hello World"
```
[frag=2]

---

## Where are my...

### Custom Data Types

Tagged/Named Tuples. [frag=1]

```erlang
{muppet, "Kermit", "frog"}.
% {muppet,"Kermit","frog"}
{muppet, "Fozzie", "bear"}.
% {muppet,"Fozzie","bear"}
```
[frag=2]

---

## Where are my...

### Custom Data Types (cont)

Records [frag=1]

```erlang
-record(muppet, {name, type}).
```

[frag=2]

```erlang
Rolf = #muppet{name="Rolf", type="Dog"}.
% #muppet{name = "Rolf",type = "Dog"}
```
[frag=3]

```erlang
tuple_to_list(Rolf).
% [muppet,"Rolf","Dog"]
```
[frag=4]

---

## Variables

- Not Really... [frag=1]
- Can only be bound once [frag=2]
- Start with a capital letter [frag=3]

---

## Variables

```erlang
> Q.
* 1: variable 'Q' is unbound
> Q = 42.
42
> Q.
42
```

---

## Pattern Matching

```erlang
1> 13 = 13.
```
[frag=1]

```erlang
13
```
[frag=2]

```erlang
2> a = a.
```
[frag=3]

```erlang
a
```
[frag=4]

```erlang
3> A = 13.
```
[frag=5]

```erlang
13
```
[frag=6]

```erlang
4> 13 = A.
```
[frag=7]

```erlang
13
```
[frag=8]

---

## Pattern Matching

```erlang
5> A = 15.
```
[frag=1]

```erlang
** exception error: no match of right hand
     side value 15
```
[frag=2]

```erlang
6> List = [1, 2, 3].
```
[frag=3]

```erlang
[1,2,3]
```
[frag=4]

---

## Pattern Matching

```erlang
7> [First, y, Third] = [x, y, z].
```
[frag=1]

```erlang
[x,y,z]
```
[frag=2]

```erlang
8> First.
```
[frag=3]

```erlang
x
```
[frag=4]

```erlang
9> Third.
```
[frag=5]

```erlang
z
```
[frag=6]

---

## Pattern Matching

```erlang
10> SomeList = [Foo, Bar, Baz] = [a, b, c].
```
[frag=1]

```erlang
[a,b,c]
```
[frag=2]

```erlang
11> SomeList.
```
[frag=3]

```erlang
[a,b,c]
```
[frag=4]

---

## Pattern Matching

```erlang
12> Foo.
```
[frag=1]

```erlang
a
```
[frag=2]

```erlang
13> Bar.
```
[frag=3]

```erlang
b
```
[frag=4]

```erlang
14> Baz.
```
[frag=5]

```erlang
c
```
[frag=6]


---

## Pattern Matching

```erlang
15> [Head | Rest] = [1, 2, 3, 4, 5].
```
[frag=1]

```erlang
[1,2,3,4,5]
```
[frag=2]

```erlang
16> Head.
```
[frag=3]

```erlang
1
```
[frag=4]

```erlang
17> Rest.
```
[frag=5]

```erlang
[2,3,4,5]
```
[frag=6]


---

## Modules

- Used for name-spacing/organization [frag=1]
- Level of code reuse [frag=2]
- Must match with filename [frag=3]
- Declaration is first line of file [frag=4]

---

## Modules

```erlang
-module(markov_chain).
```

---

## Exports

- Preprocessor directive to declare API of a module [frag=1]
- List of function identifiers [frag=2]
- Multiple export declarations are allowed [frag=3]


---

## Exports

```erlang
-export([sum/1]).
```

---

## Exports

```erlang
%% API
-export([start_link/1,
         add_following_word/2,
         pick_next_word_after/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
```

---

## Functions

- Identified by name and arity (number of arguments)
- Allows for multiple function clauses
  - Uses pattern matching to match the clause
  - Order is important, first clause to match wins

---

my_list.erl

```erlang
-module(my_list).

-export([sum/1]).

sum(List) ->
    sum(List, 0).

sum([], Sum) ->
    Sum;
sum([Head | Rest], Sum) ->
    sum(Rest, Sum + Head).
```

---

## Markov Chain

- https://en.wikipedia.org/wiki/Markov_chain
- Random state transitions based off current state and probabilities of next state.
- Similar to iPhone’s predictive typing

---

## Markov Chain

Priming the Markov Chain

- [frag=1] Parse out words
- [frag=2] Iterate over words, and create an association between a word and the word following it

---

## Markov Chain

Generating the Markov Chain

- [frag=1] Given a word, we want to pick from the next word by probability of occurance
- [frag=2] Keep picking words until we reach the number of words to generate

---

# Code Time!

- Function to add a word to list of following words
- Function to pick the next word
  - Pick a random word from a list of words

---

### src/markov_generator.erl

```
tokenize(Text) ->
    string:tokens(Text, " \t\n").
```

---

### src/markov_generator.erl

```
parse_text(Text) ->
    [FirstWord | Words] = tokenize(Text),
    load_words(FirstWord, Words).
```

---

### src/markov_generator.erl

```
load_words(_Word, []) ->
    ok;
load_words(Word, [Following | Words]) ->
    markov_word:add_following_word(Word, Following),
    load_words(Following, Words).
```

---

### src/markov_generator.erl

```
add_word_to_list(Words, Word) ->
    [Word | Words].
```

---

```
pick_next_word(Words) ->
    pick_random(Words).
```

---

```
pick_random(List) ->
    Length = length(List),
    Index = random:uniform(Length),
    lists:nth(Index, List).
```

---

# OTP

---

## Why OTP?

OTP is a set of libraries for applying lessons learned in building distributed, asynchronous, concurrent applications with high availability

---

## But, First

---

## Actor Model

- [frag=1] No Shared State
- [frag=2] Message Passing
  - [frag=3] Asynchronous Communication

---

## Actor Model

Implemented via Erlang Processes [frag=1]

---

## Processes

- Cheap
- Isolated
- Message Passing
- Links and Monitors
- Garbage Collection

---

## Processes

### Cheap

Processes are not

- Platform Process
- Threads

---

## Processes

### Cheap

Processes are

- Green threads
- Managed by the Erlang Runtime
- Small
- Quick to create

---

## Processes

### Cheap

> Processes are not threads.  Processes are cheap.  Ommmmm.
>
> -- <cite>Martin J. Logan</cite>
---

## Processes

### Isolated Processes

- No shared state
- The only state they have access to is their own

---

## Processes

### Message Passing

- Mailboxes
- Messages
  - Must provide all the information a process would need
    - Includes "Return addresses"
- No View of outside world (mostly)
  - Process registry
- Asynchronous

---

## Processes

### Links and Monitors

- Monitors
  - Monitor status of another process
- Links
  - Process is dependent on another process

---

## Processes

### Garbage Collection

Processes live on their own, and don't share state, so easy to reclaim memory when process is no longer being used.

---

## gen_server

- [frag=1] Generic Server
- [frag=2] The base behavior other OTP behaviors are built on
- [frag=3] Takes care of the different concerns you would have to write yourself
  - [frag=4] Handles maintaining state in your processes
  - [frag=5] Allows for synchronous communication on top of asynchronous message passing

---

## gen_server

Implement a behavior and expected set of callbacks

```erlang
-behavior(gen_server).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
```
[frag=1]

---

## gen_server

### init/1

```erlang
init(Args) -> {ok, State} |
              {ok, State, Timeout} |
              ignore |
              {stop, Reason}
```

---

## gen_server

### handle_call/3

```erlang
handle_call(Request, From, State) ->
                            {reply, Reply, State} |
                            {reply, Reply, State, Timeout} |
                            {noreply, State} |
                            {noreply, State, Timeout} |
                            {stop, Reason, Reply, State} |
                            {stop, Reason, State}
```

---

## gen_server

### handle_cast/2

```erlang
handle_cast(Msg, State) -> {noreply, State} |
                           {noreply, State, Timeout} |
                           {stop, Reason, State}
```

---

## gen_server

### handle_info/2

```erlang
handle_info(Info, State) -> {noreply, State} |
                            {noreply, State, Timeout} |
                            {stop, Reason, State}
```

---

## gen_server

### terminate/2

```erlang
terminate(Reason, State) -> void()
```

---

## gen_server

### code_change/3

```erlang
code_change(OldVsn, State, Extra) -> {ok, NewState}
```

---

## gen_server

### Calling a gen_server Synchronously

```
call(ServerRef, Request) -> Reply
call(ServerRef, Request, Timeout) -> Reply
```

---

## gen_server

### Calling a gen_server Asynchronously

```
cast(ServerRef, Request) -> ok
```

---

## gen_server

### Creating a API for you gen_server

- Why?
  - [frag=1] How would your client know your ServerRef?
  - [frag=2] What if you change your Request format?
  - [frag=3] All consumers would need to know format

---

## gen_server

### Creating a API for you gen_server

```
-export([add_following_word/1]).


add_following_word(Word, FollowingWord) ->
    WordPid = find_process_for_word(Word),
    gen_server:call(WordPid, {add_following_word, FollowingWord}).
```

---

# Code Time!

- Generate a reference for a process for a word
- Start a process for a word seen
- Add following word to state of process

---

## Generate a reference for a word

### src/markov_word.erl

```
find_process_for_word(Word) ->
    WordKey = get_registered_name_for_word(Word),
    case whereis(WordKey) of
        undefined -> register_word(WordKey);
        Pid when is_pid(Pid) -> Pid
    end.
```

---

### src/markov_word.erl

```
add_following_word(Word, FollowingWord) ->
    WordPid = find_process_for_word(Word),
    gen_server:call(WordPid, {add_following_word, FollowingWord}).
```

---

### src/markov_word.erl

```
pick_next_word_after(Word) ->
    WordPid = find_process_for_word(Word),
    gen_server:call(WordPid, {pick_next_word}).
```

---

### src/markov_word.erl

```
handle_call({add_following_word, Word}, _From, #state{following_words=FollowingWords}) ->
        NewState = #state{following_words=add_word_to_list(FollowingWords, Word)},
        {reply, ok, NewState};
handle_call({pick_next_word}, _From, State=#state{following_words=FollowingWords}) ->
        Reply = pick_next_word(FollowingWords),
        {reply, Reply, State}.
```

---

## Starting a process for a word
```
start_link(WordKey) when is_atom(WordKey) ->
    gen_server:start_link({local, WordKey}, ?MODULE, [], []).
```

---

#Let it Crash!

- Supervisors

---

## supervisor

- Monitors child processes
- Can supervise other supervisor processes
- Handles restarting of child processes
  - Independent or cascading
- What happens if a child process dies

---

## supervisor


```erlang
-behaviour(supervisor).

%% Supervisor callbacks
-export([init/1]).
```

---

## supervisor

### init

```erlang
init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
              ignore |
              {error, Reason}
```

---

## supervisor

### init - example

```erlang
init([]) ->
        RestartStrategy = simple_one_for_one,
        MaxRestarts = 1000,
        MaxSecondsBetweenRestarts = 3600,

        SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

        Restart = permanent,
        Shutdown = 2000,
        Type = worker,

        Child = {markov_word, {markov_word, start_link, []},
                          Restart, Shutdown, Type, [markov_word]},

        {ok, {SupFlags, [Child]}}.
```

---

## supervisor

### restart strategies

- one_for_one
- one_for_all
- rest_for_one
- simple_one_for_one

---

# Code Time!

- Start a process for a word seen
- Add following word to state of process

---

### src/markov_word.erl

```
register_word(Word) ->
    {ok, Pid} = markov_word_sup:start_child(Word),
    Pid.
```

---

# Questions?

---

# Resources

- Designing for Scalability with Erlang/OTP
  - By Francesco Cesarini, Steve Vinoski
- Erlang and OTP in Action
  - By Martin Logan, Eric Merritt, and Richard Carlsson

---

# Contact me

- @stevenproctor on Twitter
- steven.proctor@gmail.com
- http://www.proctor-it.com/

---

## Challenges

- Limit to 140 characters for a tweet
  - Limit to characters or words (tagged tuple)

---

## Challenges

- Read text in given a file name

---

## Challenges

- Add in ability to clear primed text
  - List of process ids to stop nicely?
    - Another Process
    - ETS tables
    - mnesia
  - Supervisor?

---

## Challenges

- Go out there and have fun!!!

---
