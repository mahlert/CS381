/*Jaelyn Litzinger, Chris Perdriau, Tom Mahler, Connor Stettler*/

redefine_system_predicate(when).

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

/* part a of exercise 1 */
schedule(S, P, T) :- enroll(S, C), when(C, T), where(C, P).
/* part b of exercise 1 */
useage(R, T) :- when(X, T), where(X, R).
/*part c of exercise 1 */
conflict(C1, C2) :- where(C1, X), where(C2, X), when(C1, Y), when(C2, Y), C1\=C2.
/*part d of exercise 1 */
meet(S1, S2) :- enroll(S1,C), enroll(S2,C), S1\=S2.
meet(S1, S2) :- schedule(S1,P,T), K is T+1, schedule(S2,P,K),S1\=S2.


/* Exercise 2 */
/* 2a */
rdup([], M) :- M=[].
rdup([X,X|L], M) :- rdup([X|L], M).
rdup([X|L], [X|M]) :- rdup(L, M).

/* 2b */

flat([], M) :- M=[].
flat([[X]|L], M) :- flat([X|L],M).
flat([[]|L], M) :- flat([L],M).
flat([[X|Y]|L], M) :- flat([X,Y|L],M), Y\=[].
flat([X|L], [X|M]) :- flat(L, M).


/* 2c */

project(IL, BL, M) :- phelp(IL, BL, 1|M).

phelp(_, [], _|M) :- M=[].
phelp([], _, _|M) :- M=[].
phelp(I|IL, C|BL, I|M) :- K is I+1, append(C, M, NM), phelp(IL, BL, K|NM).
phelp(N|IL, _|BL, I|M) :- I\=N, K is I+1, phelp(N|IL, BL, K|M).


