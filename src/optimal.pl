:- use_module(cost,	[cost/2]).

find_optimal(S) :- 	findall([Cost, Solution], cost(Solution, Cost), SolutionCostList),
					sort(SolutionCostList, [[_ | [S]] | _]),!.
