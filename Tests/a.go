a{
 a:[list[t],list[t]]=>list[t].

 a([],X) => X.
 a([E,..X],Y) => [E,..a(X,Y)].
}

