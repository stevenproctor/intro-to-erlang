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
<li>Tuple</li>
<li>Map</li>
<li>List</li>
</ul>
</div>

---

## Where are my...

<ul>
<li class="fragment">Booleans?</li>
<li class="fragment">Strings?</li>
<li class="fragment">Custom Data Types?</li>
</ul>

---

## Variables

<ul>
<li class="fragment">Not Really...</li>
<li class="fragment">Can only be bound once</li>
<li class="fragment">Start with a capital letter</li>
</ul>

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

<pre>
<span class="fragment">
1> 13 = 13.
13
</span>
<span class="fragment">
2> a = a.
a
</span>
<span class="fragment">
3> A = 13.
13
</span>
<span class="fragment">
4> 13 = A.
13
</span>
</pre>

---

## Pattern Matching

<pre>
<span class="fragment">
5> A = 15.
** exception error: no match of right hand side value 15
</span>
<span class="fragment">
6> List = [1, 2, 3].
[1,2,3]
</span>
</pre>

---

## Pattern Matching

<pre>
<span class="fragment">
7> [First, y, Third] = [x, y, z].
</span>
<span class="fragment">
[x,y,z]
</span>
<span class="fragment">
8> First.
</span>
<span class="fragment">
x
</span>
<span class="fragment">
9> Third.
</span>
<span class="fragment">
z
</span>
</pre>

---

## Pattern Matching

<pre>
<span class="fragment">
10> SomeList = [Foo, Bar, Baz] = [a, b, c].
</span>
<span class="fragment">
[a,b,c]
</span>
<span class="fragment">
11> SomeList.
</span>
<span class="fragment">
[a,b,c]
</span>
</pre>

---

## Pattern Matching

<pre>
<span class="fragment">
12> Foo.
</span>
<span class="fragment">
a
</span>
<span class="fragment">
13> Bar.
</span>
<span class="fragment">
b
</span>
<span class="fragment">
14> Baz.
</span>
<span class="fragment">
c
</span>
</pre>

---

## Pattern Matching

<pre>
<span class="fragment">
15> [Head | Rest] = [1, 2, 3, 4, 5].
</span>
<span class="fragment">
[1,2,3,4,5]
</span>
<span class="fragment">
16> Head.
</span>
<span class="fragment">
1
</span>
<span class="fragment">
17> Rest.
</span>
<span class="fragment">
[2,3,4,5]
</span>
</pre>

---

## Modules

<ul>
<li class="fragment">Used for name-spacing/organization</li>
<li class="fragment">Level of code reuse</li>
<li class="fragment">Must match with filename</li>
<li class="fragment">Declaration is first line of file</li>
</ul>

---

## Modules

```erlang
-module(markov_chain).
```

---

## Exports

<ul>
<li class="fragment">Preprocessor directive to declare API of a module</li>
<ul>
<li class="fragment">List of function identifiers</li>
</ul>
<li class="fragment">Multiple export declarations are allowed</li>
</ul>

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

### OTP



---

## gen_server

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
