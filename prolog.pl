link(san_diego, seattle).
link(seattle, dallas).
link(dallas, new_york).
link(new_york, chicago).
link(new_york, seattle).
link(chicago, boston).
link(boston, san_diego).


path_2(X,Y):- link(X,Z), link(Z,Y).

path_3(A,B):- link(A,C),link(C,D),link(D,B).

path_N(A,B,N):- N>1,link(A,C),N2 is N-1,path_N(C,B,N2) .

path_N(A,B,N):- N = 1, link(A,B).

path_helper(A, B, Seen) :- link(A,B), not(member(B, Seen)).

path_helper(A, B, Seen) :- link(A,C), not(member(C, Seen)), path_helper(C, B,[C|Seen]).

path(A, B) :- path_helper(A, B, [A]).

zip([],[],[]).
%zip([H1|T1],[H2|T2],[H3|T3]):- H3 = [H1,H2], zip(T1,T2,T3).
zip([H1|T1],[H2|T2],[[H1,H2]|T3]):- zip(T1,T2,T3).

part([],_,[],[]).
part([H|T],P,R1,[H|T2]):- P < H, part(T,P,R1,T2).
part([H|T],P,[H|T1],R2):- P >= H, part(T,P,T1,R2).

qsort([],[]).
qsort([H|T],R):- part(T,H,L1,L2),qsort(L1,R1),qsort(L2,R2),append(R1,[H|R2],R).

remove_all(_,[],[]).
remove_all(X,[X|T],R):- remove_all(X,T,R).
remove_all(X,[H|T],[H|T1]):- remove_all(X,T,T1).

remove_first(_,[],[]).
remove_first(X,[X|T],T).
remove_first(X,[H|T],[H|T1]):- remove_first(X,T,T1).

prefix([],_).
prefix([H1|T1],[H1|T2]):- prefix(T1,T2).

segment([H1|T1],[H1|T2]):- prefix(T1,T2).
segment([H1|T1],[H2|T2]):- segment([H1|T1],T2).


sorted([]).
sorted([_]).
sorted([A,B|T]):- A =< B, sorted([B|T]).

sort1(L1,L2) :- permutation(L1,R), sorted(R), L2 = R, !.


split([], [], []).
split([X], [X], []).
split([X|T],[X|T2],[Y|T3]) :- split(T,[Y|T3], T2). % have more than one answers.

merge([],L,L).
merge(L,[],L).

merge([H1|T1],[H2|T2],[H1|T3]):- H1 =< H2, merge(T1,[H2|T2],T3).
merge([H1|T1],[H2|T2],[H2|T3]):- H2 =< H1, merge([H1|T1],T2,T3).


merge_sort([],[]).
merge_sort([A],[A]).
merge_sort([H1,H2|T],S) :- split([H1,H2|T],L1,L2),merge_sort(L1,R1),merge_sort(L2,R2), merge(R1,R2,S).


sat(var(X)) :- X = 1.
sat(not(var(X))) :- X = 0.

sat(and([])).
sat(and([X | Tail])) :- sat(X), sat(and(Tail)).

sat(or([])) :- fail.
%% Fill in the other case(s) for ‘‘or’’ here:
sat(or([X | Tail])) :- sat(X).
sat(or([_ | Tail])) :- sat(or(Tail)).

bool(X) :- X = 0.
bool(X) :- X = 1.
bools([]).
bools([X | Tail]) :- bool(X),bools(Tail).

allsat(A,F):- bools(A),sat(F).



len([],0).
len([_|T],N) :- len(T,Nt), N is Nt + 1.

isin(X,[X|_]).
isin(X,[_|T]):- isin(X,T).

isfoolist(L):- isfoolist_helper(L,[]).

isfoolist_helper([],_).
isfoolist_helper([X|T],Seen):- foo(X), not(isin(X,Seen)),isfoolist_helper(T,[X|Seen]).

foo(a).  
foo(b). 
foo(c).
foo(d).





