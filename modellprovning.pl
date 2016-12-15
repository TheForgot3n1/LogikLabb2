% For SICStus, uncomment line below: (needed for member/2)
%:- use_module(library(lists)).
% Load model, initial state and formula from file.
verify(Input) :-
see(Input), read(T), read(L), read(S), read(F), seen,
check(T, L, S, [], F).
% check(T, L, S, U, F)
%     T - The transitions in form of adjacency lists
%     L - The labeling
%     S - Current state
%     U - Currently recorded states
%     F - CTL Formula to check.
%
% Should evaluate to true iff the sequent below is valid.
%
% (T,L), S  |-    F
%             U
% (Läses F är sann i tillståndet S med tanke på slingdetektionen U)
% To execute: consult('your_file.pl'). verify('input.txt').
% Literals

notMember(X, L) :-
  not(member(X, L)).

check(_,[],_,_,_).
check(_, L, S, _, F) :-
  member([S, X], L),
  member(F, X).

check(_, L, S, _, neg(F)) :-
  member([S, X], L),
  notMember(F, X).

check(T, L, S, _, and(F,G)) :-
  check(T, L, S, [], F),
  check(T, L, S, [], G).

check(T, L, S, _, or(F,G)) :-
  (check(T, L, S, [], F);
  check(T, L, S, [], G)).

% AX
% enklaste: s1 -> s2 -> s3 -> (ingen)
%           s1 -> s4 -> s5 -> (ingen)
check(T, L, S, _, ax(F)) :-
  member([S, AL], T),
  check_all(T, L, AL, [], F).

check(T, L, S, _, ex(F)) :-
  member([S, AL], T),
  check_exists(T, L, AL, [], F).

check(_, _, S, U, ag(_)) :-
  member(S, U).

check(T, L, S, U, ag(F)) :-
  notMember(S, U),
  check(T, L, S, [], F),
  member([S, AL], T),
  check_all(T, L, AL, [S|U], ag(F)).

check(T, L, S, U, af(F)) :-
  notMember(S, U),
  (check(T, L, S, [], F);
  (member([S, AL], T),
  check_all(T, L, AL, [S|U], af(F)))).

check(_, _, S, U, eg(_)) :-
  member(S, U).

check(T, L, S, U, eg(F)) :-
  notMember(S, U),
  check(T, L, S, U, F),
  member([S, AL], T),
  check_exists(T, L, AL, [S|U], eg(F)).

check(T, L, S, U, ef(F)) :-
  notMember(S, U),
  (check(T, L, S, [], F);
  (member([S, AL], T),
  check_exists(T, L, AL, [S|U], ef(F)))).

check_all(_,_,[],_,_).
check_all(T, L, [H|Tail], U, F) :-
  check(T, L, H, U, F),
  check_all(T, L, Tail, U, F).

check_exists(_,_,[],_,_) :- fail.
check_exists(T,L,[H|Tail],U,F) :-
  (check(T, L, H, U, F);
  check_exists(T, L, Tail, U, F)).
