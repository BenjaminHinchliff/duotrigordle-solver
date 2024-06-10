:- [wordlist].

remove_first(_, [], []) :- !.
remove_first(X, [X | Tail], Tail) :- !.
remove_first(X, [Y | Tail], [Y | NewTail]) :- X \= Y, remove_first(X, Tail, NewTail).

count_frequency(List, Counts) :-
    msort(List, Sorted),
    clumped(Sorted, Counts).

% Score the fully correct letters of a given word
% G - guessed word
% T - true word
% R - string of remaining letters
% S - final score
compute_remaining([], [], []).
compute_remaining([G|Gs], [T|Ts], Rs) :-
  G == T,
  !,
  compute_remaining(Gs, Ts, Rs).
compute_remaining([_|Gs], [T|Ts], [R|Rs]) :-
  R = T,
  compute_remaining(Gs, Ts, Rs).

% Score the correct letters of a given word
% G - guessed word
% T - true word
% R - string of remaining letters
% S - final score
score([], [], _, []).
score([G|Gs], [T|Ts], R, [S|Ss]) :-
  (   G == T ->
      ( R = Rn,
        S = f)
    ; member(G, R) ->
      ( remove_first(G, R, Rn),
        S = p
      )
    ; ( R = Rn,
        S = i)
  ),
  score(Gs, Ts, Rn, Ss).
score(G, T, S) :-
  atom_chars(G, Gc),
  atom_chars(T, Tc),
  compute_remaining(Gc, Tc, R),
  score(Gc, Tc, R, Sc),
  atom_chars(S, Sc).

entropy(G, E) :-
  findall(S, (words(T), score(G, T, S)), Ss),
  length(Ss, L),
  count_frequency(Ss, Pairs),
  pairs_values(Pairs, Cs),
  maplist({L}/[C, R] >> (R is C / L), Cs, Ps),
  foldl([P, A, R] >> (R is A + (P * log(P))), Ps, 0, NE),
  E is -NE.

all_possible(Gs, Ks, W) :-
  maplist({W}/[G, K] >> score(G, W, K), Gs, Ks).

all_words_all_possible(Gs, Ks, Ws) :- findall(W, (words(W), all_possible(Gs, Ks, W)), Ws).

words_max_entropy(Ws, ME) :-
  concurrent_maplist([W, E-W] >> entropy(W, E), Ws, Es),
  keysort(Es, SEs),
  reverse(SEs, [_-ME|_]).

solved(Ks) :- last(Ks, L), L == 'fffff'.

best_word_given(Gs, Kss, BW) :-
  exclude(solved, Kss, UNKss),
  concurrent_maplist({Gs}/[Ks, L-Ws] >>
      (all_words_all_possible(Gs, Ks, Ws), length(Ws, L)),
  UNKss,
  Wss),
  keysort(Wss, [_-Ws|_]),
  words_max_entropy(Ws, BW).
