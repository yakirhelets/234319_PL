% variable(Name, Factor).
% equation([variable(Name, Factor)]).

% del(X, [X|Xs], Xs).
% del(X, [Y|Ys], [Y|Zs]) :- del(X, Ys, Zs).
%
% insert(X, List, BiggerList) :- del(X, BiggerList, List).
%
% append([], Ys, Ys).
% append([X|Xs], Ys, []) :- append(Xs, Ys, Xs).
% append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).

% eq_add(Equation, Equation, AddEquation).
% eq_add(equation([]), equation([]), equation(List)):- equation(List).
% eq_add(equation([variable(N1, F1)|Xs]), equation([variable(N2, F2)|Ys]), equation(List)):-
%   append([variable(N1, F1)|Xs], [variable(N2, F2)|Ys], AList),
%   member(variable(N1,_),[variable(N2,_)|Ys]),
%   X is F1+F2,
%   del((variable(N1,_)), AList, BList),
%   del((variable(N1,_)), BList, CList),
%   append([variable(N1, X)], CList, List),
%   write('iteration done.'),
  % trace,
  % eq_add(equation(Xs), equation([variable(N2, F2)|Ys]), equation(List)).

  eq_add(Equation, Equation, AddEquation).
  % eq_add(equation([]), equation([]), equation(List)):-
  %   write('iteration end.'),
  %   equation(List).

  eq_add(equation([variable(N1, F1)|Xs]), equation([variable(N2, F2)|Ys]), equation(List)):-
    not(is_list(List)),
    % append(List, [], []),
    eq_add(equation([variable(N1, F1)|Xs]), equation([variable(N2, F2)|Ys]), equation([])).

  % eq_add(equation([variable(N1, F1)|Xs]), equation([]), equation(List)):-
  %   write('iteration 11.'),
  %   append([variable(N1, F1)], List, AList),
  %   % write(AList),
  %   eq_add(equation(Xs), equation([]), equation(AList)).
  %
  % eq_add(equation([]), equation([variable(N2, F2)|Ys]), equation(List)):-
  %   write('iteration 12.'),
  %   append([variable(N2, F2)], List, AList),
  %   % write(AList),
  %   eq_add(equation([]), equation(Ys), equation(AList)).

  eq_add(equation([variable(N1, F1)|Xs]), equation([variable(N2, F2)|Ys]), equation(List)):-
    % trace,
    % write('iteration 1.'),
    % write(N1),
    % write(N2),
    N1@>N2,
    % trace,
    append([variable(N2, F2)], List, AList),
    write(AList),
    % write('iteration 1 done.'),
    eq_add(equation([variable(N1, F1)|Xs]), equation(Ys), equation(AList)).

  eq_add(equation([variable(N1, F1)|Xs]), equation([variable(N2, F2)|Ys]), equation(List)):-
    % write('iteration 2.'),
    % write(N1),
    % write(N2),
    N1@<N2,
    % trace,
    append([variable(N1, F1)], List, AList),
    write(AList),
    % write('iteration 2 done.'),
    eq_add(equation(Xs), equation([variable(N2, F2)|Ys]), equation(AList)).

  eq_add(equation([variable(N1, F1)|Xs]), equation([variable(N2, F2)|Ys]), equation(List)):-
    % write('iteration 3.'),
    % write(N1),
    % write(N2),
    N1=@=N2,
    X is F1+F2,
    % trace,
    append([variable(N1, X)], List, AList),
    write(AList),
    % write('iteration 3 done.'),
    eq_add(equation(Xs), equation(Ys), equation(AList)).

% eq_add(equation([variable(bar, 1.0), variable(buzz, 2.0), variable(foo, 2.0)]),
% equation([variable(buzz, 5.0), variable(foo, 2.0)]), AddEquation).

% example:
% ?- eq_add(equation([variable(bar, 1.0), variable(foo, 2.0)]), equation([variable(bar, 2.0), variable(buzz, 5.0)]), AddEquation).
% AddEquation = equation([variable(“bar”, 3.0), variable(“buzz”, 5.0), variable(“foo”, 2.0)])

mul(Variable, Factor).
mul((variable(N, F)), factor) :-
  variable(N, F*factor).

iterate_mul([], Factor).
iterate_mul([H|T], factor) :-
  mul(H, factor),
  iterate_mul(T).

eq_multiply(Equation, Factor, MultipliedEquation).
eq_multiply(equation([]), factor, equation(List)) :- equation(List).
eq_multiply(equation(SomeList), factor, equation(List)) :-
  iterate_mul(SomeList, factor),
  % write(SomeList),
  trace,
  append(SomeList,[],List).
  % member(variable(N,F),[variable(N, F)|Xs]),
  % write(N),
  % X is F*factor,
  % write(N','X),
  % append([variable(N,X)],List,NList),
  % write(NList).
  % eq_multiply(equation(Xs), factor, mul(NList)).


% example:
% ?- eq_multiply(equation([variable("a", 2.0), variable("b", 1.0)]), 3.0, MultipliedEquation).
% MultipliedEquation = equation([variable(a, 6.0), variable(b, 3.0)])

eq_extract(Equation, Name, ExtractedEquation).

% example:
% ?- eq_extract(equation([variable(x, 2.0), variable(y, 4.0), variable(z, 8.0)]), x, Equation)
% Equation = equation([variable(y, -2.0), variable(z, -4.0)])

eq_substitute(Equation, Name, NameEquation, Result).

% example:
% ?- eq_substitute(equation([variable(x, 2.0), variable(y, 1.0)]), x, equation([variable(y, 1.0)]), Result)
% Result = equation([variable(y, 3.0)])

eqs_substitute(Equations, Name, NameEquation, Result).

eqs_solve_step(Equations, Name, Result).

% example:
% ?- eqs_solve_step(equations([equation([variable(x, 2.0), variable(y, 4.0)]), equation([variable(x, 7.0), variable(y, 3.0), variable(z, 5.0)])]), x, Result).
% Result = equations([equation([variable(“y”, -11.0), variable(“z”, 5.0)])]).

eqs_solve(Equations, Names, Result).

% example:
% ?- eqs_solve(equations([
%   equation([variable(“x”, 1.0), variable(“y”, 3.0), variable(“z”, 5.0), variable(“_”, 23.0)]),
%   equation([variable(“x”, 3.0), variable(“y”, 1.0), variable(“z”, 8.0), variable(“_”, 22.0)]),
%   equation([variable(“x”, 7.0), variable(“y”, 2.0), variable(“z”, 3.0), variable(“_”, 34.0)])]), [“x”, “y”], Partial),
% eqs_solve(Partial, [“x”], Result).
% Result = equations([equation([variable(“_”, 3.0), variable(“x”, 1.0)])]).
%
% ?- eqs_solve(equations([
%   equation([variable(“x”, 1.0), variable(“y”, 3.0), variable(“z”, 5.0), variable(“_”, 23.0)]),
%   equation([variable(“x”, 3.0), variable(“y”, 1.0), variable(“z”, 8.0), variable(“_”, 22.0)]),
%   equation([variable(“x”, 7.0), variable(“y”, 2.0), variable(“z”, 3.0), variable(“_”, 34.0)])]), [“x”, “y”], Partial),
% eqs_solve(Partial, [“y”], Result).
% Result = equations([equation([variable(“_”, 5.0), variable(“y”, 1.0)])]).

eqs_solutions(Equations, Names, Result).

% example:
% ?- eqs_solutions(equations([
%   equation([variable(“x”, 1.0), variable(“y”, 3.0), variable(“z”, 5.0), variable(“_”, 23.0)]),
%   equation([variable(“x”, 3.0), variable(“y”, 1.0), variable(“z”, 8.0), variable(“_”, 22.0)]),
%   equation([variable(“x”, 7.0), variable(“y”, 2.0), variable(“z”, 3.0), variable(“_”, 34.0)])]), [“x”, “y”], Result).
% Result = [variable(“x”, -3.0), variable(“y”, -5.0)].
