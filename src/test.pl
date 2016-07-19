p(X, Y):- q(X, Y).
p(X, Y):- r(X, Y).
q(X, Y):- s(X), !, t(Y).
r(c, d).
s(a).
s(b).
t(a).
t(b).