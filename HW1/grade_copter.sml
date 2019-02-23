fun auxOneEgg (n, foo) =
  if foo(n)=true then n-1 else auxOneEgg (n+1,foo);

fun auxTwoEggs (n, foo) =
  if foo(n)=true then auxOneEgg ((n div 2) +1, foo) else auxTwoEggs (2*n, foo);

fun gradeCopter (foo:int->bool) =
  auxTwoEggs(1, foo);
