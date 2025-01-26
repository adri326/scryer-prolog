fibonacci(N, X) :- fibonacci(N, X, _).

fibonacci(N, X, Y) :-
    N > 0,
    N1 is N - 1,
    fibonacci(N1, W, X),
    Y is W + X.
fibonacci(0, 1, 1).

fibonacci_prod(N, X) :-
    N > 0,
    N1 is N - 1,
    fibonacci_prod(N1, X1),
    fibonacci(N, Y),
    X is X1 * Y.
fibonacci_prod(0, 1).

log2(N, X) :-
    N < 2 -> X = 0
    ; N2 is N >> 1, log2(N2, X1), X is X1 + 1.

fibonacci_prod_log2(N, X) :-
    fibonacci_prod(N, Y),
    log2(Y, X).
