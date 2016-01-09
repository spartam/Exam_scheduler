:- use_module(cost,	[cost/2]).

find_optimal(S) :- 	findall([Cost, Solution], cost(Solution, Cost), SolutionCostList),
					sort(SolutionCostList, [[C | [S]] | Tail]),!.
					%first(S, SortedSolutionCostList),!.

first(Head, [Head | Tail]).