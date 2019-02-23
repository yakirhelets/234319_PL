use_module(library(clpfd)).

is_list([]).
is_list([_|Xs]) :- is_list(Xs).

member(X,[X|_]).
member(X,[_|Ys]) :- member(X,Ys).

prefix([],_).
prefix([X|Xs],[X|Ys]) :- prefix(Xs,Ys).

suffix(Xs,Xs).
suffix(Xs,[_|Ys]) :- suffix(Xs,Ys).

del(X,[X|Xs],Xs).
del(X,[Y|Ys],[Y|Zs]) :- del(X,Ys,Zs).

insert(X,List,BiggerList) :-
	del(X,BiggerList,List).

append([],Ys,Ys).
append([X|Xs],Ys,[X|Zs]) :- append(Xs,Ys,Zs).

sublist(Xs,Ys) :-
	append(_,Bs,Ys),
	append(Xs,_,Bs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%variable(Name, Factor).%
%equation([variable(Name, Factor)]).%

%%%%%%%%%%%%%%%%%%%%%%%%eq_add(Equation, Equation, AddEquation).%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%eq_add(Equation, Equation, AddEquation).%

eq_add(equation([]), equation([]), equation([])).
eq_add(Equation, equation([]), Equation).
eq_add(equation([]), Equation, Equation).
eq_add(equation([variable(Name, Factor1)|Xs]),equation([Y|Ys]),AddEquation) :-
	(member(variable(Name, Factor2),[Y|Ys]),
  del(variable(Name, Factor2), [Y|Ys], List),
	Factor3 is Factor1+Factor2,
	append([variable(Name, Factor3)],List,[Z|Zs]),
	eq_add(equation(Xs),equation([Z|Zs]),AddEquation)) ;
	(eq_add(equation(Xs),equation([variable(Name, Factor1),Y|Ys]),AddEquation)).


%%%%%%%%%%%%%%%%%%%%%%%eq_multiply(Equation, Factor, MultipliedEquation)%%%%%%%%%%%%%%%%%%%%%

eq_multiply(equation([]),_,equation([])).
eq_multiply(Equation,1.0,Equation).
eq_multiply(_,0.0,equation([])).
eq_multiply(equation([variable(Name,Factor)|Es1]),Mul,equation([variable(Name,Res_Factor)|Res_Es])):-
	Res_Factor is Mul*Factor,
	eq_multiply(equation(Es1),Mul,equation(Res_Es)).


%%%%%%%%%%%%%%%%%%%%%%%%eq_extract(Equation, Name, ExtractedEquation).%%%%%%%%%%%%%%%%%%%%%%%

eq_extract(equation([]), _, equation([])).
eq_extract(equation(Xs), "_", equation(Ys)):-
	eq_multiply(equation(Xs),1,equation(Ys)).
eq_extract(equation(Xs), Name, equation([variable("_", 0.0)])):-
	not(member(variable(Name,_),Xs)).
eq_extract(equation(Xs),Name,equation(Ys)):-
	member(variable(Name,Factor1),Xs),
	Factor2 is -1/Factor1,
	del(variable(Name,Factor1),Xs,NewXs),
	eq_multiply(equation(NewXs),Factor2,equation(Ys)).

%%%%%%%%%%%%%%%%%%%%%%%%eq_substitute(Equation, Name, NameEquation, Result).%%%%%%%%%%%%%%%%%%

eq_substitute(equation([]),_,_,equation([])).
eq_substitute(equation(Xs), Name,_,equation(Xs)):-
	not(member(variable(Name,_),Xs)).
eq_substitute(equation(Xs), Name, equation(Ys),equation(Zs)):-
	member(variable(Name,Factor),Xs),
	eq_multiply(equation(Ys),Factor,equation(Cs)),
	del(variable(Name,Factor),Xs,Ls),
	eq_add(equation(Ls),equation(Cs),equation(Zs)).


%%%%%%%%%%%%%%%%%%%%%%%%eqs_substitute(Equations, Name, NameEquation, Result).%%%%%%%%%%%%%%%

% eqs_substitute(Equations,_,equations([]),Result).
eqs_substitute(equations([]),_,_,equations([])).
% eqs_substitute(equations([]),_,_,equations([Z|Zs])).
eqs_substitute(equations([equation(X)|Xs]),Name,_,equations([X|Xs])):-
	write('d'),
	% write([X|Xs]),
	% trace,
	not(member(variable(Name,_),X)),
	qs_substitute(equations(Xs),Name,_,equations(Xs)).
eqs_substitute(equations([equation(X)|Xs]), Name, equation(Ys), equations([Z|Zs])):-
	% write('equation(Ys)'),
	% write([X|Xs]),
	member(variable(Name,_),X),
	% write('yes'),
	eq_substitute(equation(X), Name, equation(Ys), equation(Ts)),
	write(equation(Ts)),
	% trace,
	eqs_substitute(equations(Xs), Name, equation(Ys), equations([(equation(Ts)),Z|Zs])).

 % eqs_substitute(equations([equation([variable("x", 2.0)]), equation([variable("x", 7.0)])]), "x", equation([variable("y", 2.0)]), Result).
%%%%%%%%%%%%%%%%%%%%%%%%eqs_solve_step(Equations, Name, Result).%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eqs_solve_step(equations([]),_,equations([])).
% eqs_solve_step(equations([_|Xs]), Name,equations(Xs)):-
% 	not(member(variable(Name,_),Xs)).
eqs_solve_step(equations([(equation([L|Ls]))|Xs]), Name,equations([Z|Zs])):-
	write(Name),
	write(equation([L|Ls])),
	trace,
	member(variable(Name,_), [L|Ls]),
	% write(X),

	eq_extract(X, Name, T),
	write(T),
	eqs_substitute(X, Name, T, equations(Z|Zs)) ;
	% trace,
	write('next->'),
	eqs_solve_step(equations(Xs), Name, equations([Z|Zs])).


%%%%%%%%%%%%%%%%%%%%%%%%eqs_solve(Equations, Names, Result).%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%eqs_solutions(Equations, Names, Result).%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
