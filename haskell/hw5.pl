%-------------------------------------------------------------------------
%- Group members：Qiulin Zhang, Zhuoling Chen, Sangyeon Lee, Ingyu Woo
%- CS 381, Spring 2018
%- Homework 5 (Prolog)
%- Due：June 5, 2018，2pm
%-------------------------------------------------------------------------

/* Exercise 1 */

when(275,10).
when(261,12).
when(381,11).
when(398,12).
when(399,12).

where(275,owen102).
where(261,dear118).
where(381,cov216).
where(398,dear118).
where(399,cov216).

enroll(mary,275).
enroll(john,275).
enroll(mary,261).
enroll(john,381).
enroll(jim,399).

%- (a)
schedule(S,P,T) :- when(C,T), where(C,P), enroll(S,C).

%- (b)
usage(P,T) :- when(S,T), where(S,P).

%- (c)
conflict(X,Y) :- when(X,T), when(Y,T), X\=Y.

%- (d)
meet(A,B) :- enroll(A,X), enroll(B,X), A\=B.
meet(A,B) :- enroll(A,X), enroll(B,Y), when(X,M), when(Y,N), where(X,P), where(Y,P), A\=B, X\=Y, M=:=N+1.
meet(A,B) :- enroll(A,X), enroll(B,Y), when(X,M), when(Y,N), where(X,P), where(Y,P), A\=B, X\=Y, M=:=N-1.


/* Exercise 2 */

%- (a)
rdup([],[]).
rdup([X],[X]).
rdup([H|L],M) :- member(H,L), rdup(L,M).
rdup([H|L],[H|M]) :- not(member(H,L)), rdup(L,M).

%- (b)
flat([],[]) :- !.
flat(L,[L]).
flat([X|L],F) :- flat(X,L1), flat(L,L2), append(L1,L2,F).

%- (c)
numfinder(1, [X|XR], [X]) :- !.
numfinder(N, [X|XR], L) :- M is N-1, numfinder(M,XR,L).

project([],X,[]).
project([N|NR], X, L) :- numfinder(N, X, L1), project(NR, X, L2), append(L1, L2, L).
project([N|NR], X, L) :- project(NR, X, L2), append([], L2, L).
