father(ruwee, padme).
father(anakin, luke).
father(anakin, leia).
father(han, ben).
%father(X,Y) :- male(X), parent(X,Y).

mother(jobal, padme).
mother(shmi, anakin).
mother(padme, luke).
mother(padme, leia).
mother(leia, ben).

alias(darthvader, anakin).
alias(kyloren, ben).
alias(X,Y) :- alias(Y,X).

parent(han, kyloren).
parent(X,Y) :- father(X, Y).
parent(X,Y) :- mother(X, Y).

childof(X, Y) :- parent(Y ,X).

son(X,Y) :- male(X), father(Y,X).

male(han).
male(luke).
