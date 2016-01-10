:- use_module(cost, [sorted_cost_event_list/2]).
:- use_module(solution, 	[is_valid/1,
							 my_write/2]).

find_heuristically(schedule(EventArray)) :- findall(EID, has_exam(_, EID), EIDS),
											find_solution_heuristically(EventArray, EIDS),
											open("Failed_the_test.txt", append, Stream), my_write(Stream, EventArray), close(Stream),
											is_valid(schedule(EventArray)),!.

find_solution_heuristically([], []).
find_solution_heuristically([event(EID, RID, Day, Start) | OtherEvents], [EID | OtherEIDS]):- 	sorted_cost_event_list(EventList, EID),
																								member(event(EID, RID, Day, Start), EventList),
																								find_solution_heuristically(OtherEvents, OtherEIDS).