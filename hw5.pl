
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

/* A */
schedule(S,P,T) :- enroll(S,Y), where(Y,P), when(Y,T).

/* B */
usage(P,T) :- where(Y,P), when(Y,T).

/* C */
conflict(X,Y) :- where(X,N1), when(X,N2), where(Y,N1), when(Y,N2), X\=Y.

/* D */
meet(A,B) :- schedule(A, P, T), schedule(B, P, T).
meet(A,B) :- schedule(A, P, T1), schedule(B, P, T2), (T1 + 1) =:= T2.


/* Exercise 2 */


