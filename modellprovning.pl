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
% check_if_right_state(S,[H|T]) :-
%  (member(S, H); check_if_right_state(S,T))

check(_,[],_,_,_).

check(_, [H|R], S, [], X) :-
  ((member(S,H), check_ext1(H, X)); check(_, R, S, [], X)), !.
check_ext1([H|R], X) :-
  flatten(R,T),
  member(X,T).

check(_, [H|R], S, [], neg(X)) :-
  ((member(S,H), check_ext1(H, X)), check_ext1(H, X)); check(_, R, S, [], neg(X))),
  check_ext2(_, H, S, [], X).
check_ext2(_, [H|R], S, [], X) :-
  flatten(R,T),
  \+ member(X,T).

check(T, [H|R], S, [], and(F,G)) :-
  ((member(S,H), check_ext1(H, X)); check(T, R, S, [], and(F,G))),
  check_ext3(T, H, [], and(F,G)).
check_ext3(T, [H|R], [], and(F,G)) :-
  flatten(R,T),
  member(F,T),
  member(G,T).

check(_, L, S, [], or(F,G)) :-
  ((member(S,H), check_ext1(H, X)); check(_, R, S, [], and(F,G))),
  check_ext4(_, H, [], and(F,G)).
check_ext4(_, [H|R], [], and(F,G)) :-
  flatten(R,T),
  (member(F,T); member(G,T)).

% AX
% enklaste: s1 -> s2 -> s3 -> (ingen)
%           s1 -> s4 -> s5 -> (ingen)
check(Adj, [H|T], S, [], X) :-
  findAdjacents(S,Adj,Targets),
  check_AXHelper(Adj,[H|T],S,U,X,Targets).

check_AXHelper(Adj, L, S, U, X, Targets) :-
  \+ member(S,U),
  check(_,L,S, [], X),
  append(S,U,UAppend),
  check(Adj,)
  check(Adj, [H|T], Next, UAppend, X).
  check_AXHelper(Adj, [H|T], S, UAppend, X) :-

findAdjacents(S, [H1|T1], Targets) :-
  (member(S,H1), flatten(T1,Flat), permutation(Flat, Targets));
  findAdjacents(S,T1,Targets).


% EX
% AG
% EG
% EF
% AF

% States are s0, s1 and s2
% Adjacency lists of LTS
% im = [[s0, [s1, s2]],[s1, [s0, s2]],[s2, [s2]]].
% Labeling of LTS
% lt = [[s0, [p,q]],[s1, [q,r]],[s2, [r]]].
