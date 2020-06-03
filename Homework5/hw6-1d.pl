
/* Exercise 1 */

xxwhen(275,10).
xxwhen(261,12).
xxwhen(381,11).
xxwhen(398,12).
xxwhen(399,12).

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
schedule(S, P, T) :- enroll(S, C), xxwhen(C, T), where(C, P).
/* part b of exercise 1 */
useage(R, T) :- xxwhen(X, T), where(X, R).
/*part c of exercise 1 */
conflict(C1, C2) :- where(C1, X), where(C2, X), xxwhen(C1, Y), xxwhen(C2, Y), C1\=C2.
/*part d of exercise 1 */
meet(S1, S2) :- enroll(S1,C), enroll(S2,C), S1\=S2.
meet(S1, S2) :- schedule(S1,P,T), K is T+1, schedule(S2,P,K),S1\=S2.
 

/* Exercise 2 */
