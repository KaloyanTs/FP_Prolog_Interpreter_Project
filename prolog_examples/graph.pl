edge(a,b).
edge(a,c).
edge(b,d).
edge(c,b).
edge(c,f).
edge(d,e).
edge(f,g).
edge(g, h).
edge(f,j).

vertex(Node) :- edge(Node,NodeX).
vertex(Node) :- edge(NodeX,Node).

connected(Node,Node) :- vertex(Node).
connected(Node1,Node2) :- edge(Node1,Node2).
connected(Node1,Node2) :- edge(Node1,NodeX), connected(NodeX,Node2).