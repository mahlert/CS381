/* 2a */
rdup([], M).
rdup([X,X|L], M) :- rdup([X|L], M).
rdup([X|L], [X|M]) :- rdup(L, M).

/* 2b */

flat([], M).
flat([[X]|L], M) :- flat([X|L],M).
flat([X|L], [X|M]) :- flat(L, M).

