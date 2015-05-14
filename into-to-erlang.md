# Intro to Erlang

---

## About Me

- Founder and Organizer of the Dallas/Fort Worth Erlang User Group
- Functional Geekery (http://www.functionalgeekery.com/)
- Planet Erlang (http://www.planeterlang.com/)
- http://www.proctor-it.com/
- @stevenproctor on Twitter

---

## Background


- Created by Joe Armstrong, Robert Virding, and Mike Williams
- Born in Ericsson for telecom switches in 1986
- Open Sourced in 1998

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

Named Tuples. [frag=1]

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

# Exercise Time!

- Function to prime the Markov Chain
- Function to generate a string of N words

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

# Code Time!

- Start a process for a word seen
- Add following word to state of process


---

#Let it Crash!

- Supervisors

---
-

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

## Challenges

- Limit to 140 characters for a tweet

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
