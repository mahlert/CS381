
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
meet(A, B) :- enroll(A, X), enroll(B, X), A\=B; enroll(A, X), enroll(B, Y), when(X, T1), when(Y, T2), where(X, P), where(Y, P), A\=B, T1==T2+1;

/* Exercise 2 */


