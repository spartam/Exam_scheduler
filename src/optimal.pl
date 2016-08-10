:- module(optimal, [find_optimal/1]).
:- use_module(cost,	[cost/2]).

%% findall(-S)
%% finds an optimal solution
find_optimal(S) :- 	findall([Cost, Solution], cost(Solution, Cost), SolutionCostList),
					sort(SolutionCostList, [[_ | [S]] | _]),!.
