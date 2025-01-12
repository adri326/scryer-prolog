# Changing and inspecting how predicates get compiled

Prolog makes meta-programming quite easy, as you can treat predicates as terms,
which you can then transform through other predicates.

Today we'll see how this is done in Scryer Prolog, and how you can inspect the result of these transformations when they go wrong.

## Goal and term expansion

Some libraries like `dcgs` modify predicates before they become further compiled into WAM instructions.
Scryer prolog (and multiple other distributions of prolog) provide two places in which these transformations may occur:
- **Term expansion** transform whole terms, like `id(X) --> X` into `id(X, S, R) :- phrase(X, S, R)`,
  before they get further processed.
- **Goal expansion** happens while each individual goal of a predicate gets processed; they allow for things like `X #>= Y` to be transformed down into simpler goals.

You can define your own term expansion with `term_expansion/2` and `goal_expansion/2`:
the first argument will be transformed into the second argument.

`term_expansion/2` can unify the second argument with a list of terms, in case you want to generate multiple predicates from a single term.

- [Term expansion on SICStus Prolog](https://sicstus.sics.se/sicstus/docs/3.12.8/html/sicstus/Term-and-Goal-Expansion.html)
- [Program transformation on SWI-Prolog](https://www.swi-prolog.org/pldoc/man?section=progtransform)

### Inspecting how a predicate was transformed

You can look at a given predicate's fully transformed form using the `listing/1` predicate from `library(format)`.
To do so, the predicate to inspect must be marked as `dynamic/1`, so that `listing/1` can access its contents.

In the file defining the predicate you are interested in, add `:- dynamic(pred/arity).` like this:

```prolog
:- use_module(library(dcgs)).

% Must be added for `listing/1` to work.
% Note that dcgs have arity n+2.
:- dynamic(spaces/2).

spaces --> [].
spaces --> space, !, spaces.
```

Then, use `listing/1` to inspect the predicate:

```prolog
?- use_module(library(format)).
?- listing(spaces/2).

spaces(A,B) :-
   A=B.
spaces(A,B) :-
   space(A,C),
   !,
   C=D,
   spaces(D,B).
```

### Inspecting term expansion

You can inspect term expansion by directly calling `term_expansion/2`, although this will only match one step of term expansion, and will fail if there are no term expansion rules matching the input.

`expand_term/2` will call `term_expansion/2` until it cannot, and return that last value.
If no rules match, then it will simply return the input value.

```prolog
?- term_expansion((id(X) --> X), Exp).
   Exp = id(X, S, R) :- phrase(X, S, R).
?- term_expansion((eq(X, Y) :- X = Y), Exp).
   false.

?- expand_term((id(X) --> X), Exp).
   Exp = id(X, S, R) :- phrase(X, S, R).
?- expand_term((eq(X, Y) :- X = Y), Exp).
   Exp = (eq(X, Y) :- X = Y).
```

### Making your own term expansion

Here's how you can use term expansion to implement a primitive form of `';'/2`:

```prolog
:- op(100, xfy, or).
term_expansion((Header :- (Lhs or Rhs)), [(Header :- Lhs), (Header :- Rhs)]).

% Only needed for listing/1
:- dynamic(is_odd/1).

is_odd(X) :- (X = 1) or (X = 3) or (X > 1, X2 is X - 2, is_odd(X2)).
```

```prolog
?- use_module(library(format)).
?- listing(is_odd/1).

is_odd(A) :-
   A=1.
is_odd(A) :-
   A>1,
   B is A-2,
   is_odd(B).
is_odd(A) :-
   A=3.
```

### Inspecting goal expansion

Like `term_expansion/2`, you can inspect a single step of goal expansion by directly calling `goal_expansion/2`.

`loader:goal_expansion/3` can be used to repeatedly apply all `goal_expansion/2` defined in a given module, although it currently doesn't recursively expand subgoals.

```prolog
?- use_module(library(clpz)).
   true
?- goal_expansion(X #>= Y, Exp).
   Exp = ... some long expression ...
?- goal_expansion(X = Y, Exp).
   false.

?- loader:goal_expansion(X #>= Y, user, Exp).
   Exp = ... some long expression ...
?- loader:goal_expansion(X = Y, user, Exp).
   Exp = (X = Y).
```

### Making your own goal expansion

Let's re-implement `once(Pred)` using goal expansion.
As a recap, `once(Pred)` can be defined as `once(P) :- call(P), !`

```prolog
:- use_module(library(format)).
goal_expansion(my_once(Pred), (Pred, !)).

% Only needed for listing/1
:- dynamic(is_odd/1).

is_odd(X) :- my_once(X = 1); my_once(X = 3); X > 1, X2 is X - 2, is_odd(X2).
```

This gives the following:

```prolog
?- listing(is_odd/1).

is_odd(A) :-
   (  A=1,
      !
   ;  A=3,
      !
   ;  A>1,
      B is A-2,
      is_odd(B)
   ).
```

## Inspecting the WAM instructions

After all term and goal expansion rules are applied, the predicate is then compiled into instructions for the Warren Abstract Machine (also known as the WAM).
You can learn more about these instructions in [Flying Rolls #1](https://github.com/mthom/scryer-prolog/wiki/Flying-Roll-%231:-instructions.rs-and-the-Dispatch-Loop).

The following predicates can be combined together to inspect the WAM instructions making up a compiled predicate:

- `wam_instructions/2`, from `library(diag)`, to get a list of instructions
- `portray_clause/1`, from `library(format)`, to print each instruction
- `maplist/2`, from `library(lists)`, to iterate over the list of instructions

```prolog
?- use_module(library(diag)), use_module(library(format)), use_module(library(lists)).
?- wam_instructions(spaces/2, Instr), maplist(portray_clause, Instr).

dynamic_else(1,inf,2).
execute(=,2).
dynamic_else(1,inf,fail(0)).
allocate(4).
get_variable(y(1),2).
get_level(y(2)).
put_variable(y(3),2).
call(space,2).
cut(y(2)).
put_value(y(3),1).
put_variable(y(4),2).
call(=,2).
put_unsafe_value(4,1).
put_value(y(1),2).
deallocate.
execute(spaces,2).
```
