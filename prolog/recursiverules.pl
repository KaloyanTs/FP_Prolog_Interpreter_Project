% family
father(bob, alice).
mother(sue, james).
mother(sue, alice).
mother(ann, sue).

dad(X,Y) :- father(X,Y).
parent(X,Y) :- father(X,Y).
parent(X,Y) :- mother(X,Y).
child(X,Y) :- parent(Y,X).

ancestor(A,D) :- parent(A,D).
ancestor(A,D) :- parent(A,X), ancestor(X, D).

% graphs
% facts - list of edges in a directed graph
edge(a,b).
edge(a,c).
edge(b,d).
edge(c,d).
edge(c,f).
edge(d,e).
edge(f,g).
edge(g, h).
edge(i,j).

connected(Node1,Node2) :- edge(Node1,Node2).
connected(Node1,Node2) :- edge(Node1,NodeX), connected(NodeX,Node2).