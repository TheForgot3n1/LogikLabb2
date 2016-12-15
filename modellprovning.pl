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

find_and_get_list([H|L],AL,S) :-
  member(S,H) -> find_and_get_list(H,AL),
  find_and_get_list(L,AL,S).

find_and_get_list([_|T],AL) :-
  AL = T.

check_ext([_|T], X) :-
  flatten(T, R),
  member(X, R).

check(_,[],_,_,_) :- fail.

check(_, [H|R], S, [], F) :-
  ((member(S,H), check_ext(H, F)); check(_, R, S, [], F)), !.
  /*
check_ext1([_|R], X) :-
  flatten(R,T),
  member(X,T).
*/
check(_, [H|R], S, [], neg(F)) :-
  (member(S,H), \+ check_ext(H, F));
  check(_, R, S, [], neg(F)).
  /*
check_ext2(_, [H|R], S, [], X) :-
  flatten(R,T),
  \+ member(X,T).
*/
check(T, [H|R], S, [], and(F,G)) :-
  ((member(S,H), check_ext(H, F), check_ext(H, G));
  check(T, R, S, [], and(F,G))).
  /*
check_ext3(T, [H|R], [], and(F,G)) :-
  flatten(R,T),
  member(F,T),
  member(G,T).
*/
check(_, [H|R], S, [], or(F,G)) :-
  (member(S,H), (check_ext(H, F); check_ext(H, G)));
  check(_, R, S, [], and(F,G)).
  /*
check_ext4(_, [H|R], [], and(F,G)) :-
  flatten(R,T),
  (member(F,T); member(G,T)).
*/
% AX
% enklaste: s1 -> s2 -> s3 -> (ingen)
%           s1 -> s4 -> s5 -> (ingen)
check(T, L, S, _, ax(F)) :-
  member([S, AL], T),
  check_AX(AL, L, S, _, F).

check(T, L, S, _, ex(F)) :-
  member([S, AL], T),
  check_EX(AL, L, S, _, F).

check(_, _, S, U, ag(_)) :-
  member(S, U).

check(T, L, S, U, ag(F)) :-
  check(T, L, S, [], F),
  member([S, AL], T),
  check_all(T, L, AL, [S|U], ag(F)).

check(T, L, S, U, af(F)) :-
  \+ member(S, U),
  check(T, L, S, [], F);
  (member([S, AL], T),
  check_all(T, L, AL, [S|U], af(F))).

check(T, L, S, U, eg(F)) :-
  check(T, L, S, U),
  member([S, AL], T),
  check_exists(T, L, AL, [S|U], eg(F)).

%check(T, L, S, _, af(F)) :-
%  find_and_get_list(T, AL, S),
%  fail.

check_AX([],_,_,_,_).

check_AX([H|T], L, _, _, F) :-
  check(_, L, H, _, F),
  check_AX(T, L, _, _, F).

check_EX([],_,_,_,_) :-
  fail.

check_EX([H|T], L, _, _, F) :-
  check(_, L, H, _, F);
  check_EX(T, L, _, _, F).

check_all(_,_,[],_,_).
check_all(T, L, [H|T], U, F) :-
  check(T, L, H, U, F),
  check_all(T, L, T, U, F).

check_exists(_,_,[],_,_).
check_exists(T, L, [H|T], U, F) :-
  check(T, L, H, U, F);
  check_exists(T, L, T, U, F).
























/*

check_AX(Adj, [H2|T2], S, _, F) :-
  find_and_get_list(Adj,AL,S),
  check_AXHelper1(AL,[H2|T2], S, _, F).

check_AXHelper1([H1|T1],[H2|T2], S, _, F).
  check_AXHelper2(H1,H2,F),
  check_AXHelper1(T1,[H2|T2],S,U,F).

check_AXHelper2(AdjState,[H|T],F) :-
  AdjState = H -> member(F,T),
  check_AXHelper(AdjState,T,F).


% EX
%check_EX([],_,_,_,_).

check_EX(Adj, [H2|T2], S, _, F) :-
  find_and_get_list(Adj,AL,S),
  check_EXHelper1(AL,[H2|T2], S, _, F).

check_EXHelper1([H1|T1],[H2|T2], S, _, F) :-
  (check_EXHelper2(H1,H2,F); check_EXHelper1(T1,[H2|T2],S,_,F)).

check_EXHelper2(AdjState,[H|T],F) :-
  AdjState = H -> member(F,T),
  check_AXHelper(AdjState,T,F).


% AG
check_AG(Adj,[H2|T2],S,U,F) :-
  find_and_get_list(Adj,AL,S),
  check_all_routes(AL,[H2|T2],S,U,F),
  check_AGHelper(AL,[H2|T2],S,U,AG(F)),
  check_all_routes([H1|T1],[H2|T2],S,U,F),
  check_AGHelper(AdjState,[H|T],F).

check_AGHelper(_, _, _, _, _).
% EG
% EF
% AF

% States are s0, s1 and s2
% Adjacency lists of LTS
% im = [[s0, [s1, s2]],[s1, [s0, s2]],[s2, [s2]]].
% Labeling of LTS
% lt = [[s0, [p,q]],[s1, [q,r]],[s2, [r]]].*/
