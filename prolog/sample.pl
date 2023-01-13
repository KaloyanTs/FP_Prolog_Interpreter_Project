edge(a,b).
edge(b,c).
edge(a,d).
edge(d,c).
edge(a,a).
edge(X,Y) :- edge(Y,X).

self(X) :- edge(X,X).