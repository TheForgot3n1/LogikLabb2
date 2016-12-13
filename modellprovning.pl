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

check(_,[],_,_,_).

check(_, [H|R], S, [], X) :-
  ((member(S,H), check_ext1(H, X)); check(_, R, S, [], X)), !.
check_ext1([H|R], X) :-
  flatten(R,T),
  member(X,T).

check(_, [H|R], S, [], neg(X)) :-
  ((member(S,H), check_ext1(H, X)), check_ext2(H, X)); check(_, R, S, [], neg(X))).
check_ext2(_, [H|R], S, [], X) :-
  flatten(R,T),
  \+ member(X,T).

check(T, [H|R], S, [], and(F,G)) :-
  ((member(S,H), check_ext3(H, X)); check(T, R, S, [], and(F,G))).
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
check_AX([],_,_,_,_)

check_AX([H1|T1], [H2|T2], S, _, AX(F))
  check_AXHelper(H1,H2,F),
  check_AX(T1,[H2|T2],S,U,F).

check_AXHelper(AdjState,[H|T],F) :-
  AdjState = H -> member(F,T),
  check_AXHelper(AdjState,T,F).


% EX
check_EX([],_,_,_,_)

check_EX([H1|T1], [H2|T2], S, _, EX(F))
  (check_EXHelper(H1,H2,F); check_EX(T1,[H2|T2],S,U,F)).

check_EXHelper(AdjState,[H|T],F) :-
  AdjState = H -> member(F,T),
  check_AXHelper(AdjState,T,F).


% AG
check_AG([H1|T1], [H2|T2], S, _, AG(F))
  check_AGHelper(H1,H2,F),
  check_AG(T1,[H2|T2],S,U,F).

check_AGHelper(AdjState,[H|T],F) :-
  AdjState = H -> member(F,T),
  check_AGHelper(AdjState,T,F).

% EG
% EF
% AF

% States are s0, s1 and s2
% Adjacency lists of LTS
% im = [[s0, [s1, s2]],[s1, [s0, s2]],[s2, [s2]]].
% Labeling of LTS
% lt = [[s0, [p,q]],[s1, [q,r]],[s2, [r]]].
