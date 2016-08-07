:- module(heuristic, 	[find_sorted_events/2,
						 find_heuristically/1,
						 replace/3]).
:- use_module(cost, [sorted_cost_event_list/2]).
:- use_module(solution2, 	[is_valid/1,
							 my_write/2,
							 is_event_conflict_free_with_list/2]).


find_sorted_events([], []).
find_sorted_events([CurrentEventSortedList|Rest], [EID|EIDS]) :- 	sorted_cost_event_list(CurrentEventSortedList, EID),
																	find_sorted_events(Rest, EIDS).


find_heuristically(schedule(Events)) :- findall(EID, exam(EID,_), EIDS),
										find_sorted_events(ELS, EIDS),
										find_heuristically(Events, ELS),
										open("Failed_the_test.txt", append, Stream), my_write(Stream, Events), close(Stream),
										is_valid(schedule(Events)), !.

find_heuristically([], []).
find_heuristically(Event, [[[_| Event] | _]]).
find_heuristically(Event, [[_ | Rest]]) :- find_heuristically(Event, [Rest]).
find_heuristically(Lst, ExamsOptions) :- 	length(ExamsOptions, L),
											L > 1,
											Y is round(L / 2),
											append(Exams1, Exams2, ExamsOptions),
											length(Exams1, Y),
											find_heuristically(Lst1, Exams1),
											find_heuristically(Lst2, Exams2),
											append(Lst1, Lst2, Lst),
											succesfull_merge(Lst).

succesfull_merge(Lst, X) :-	length(Lst, Y),
							Y < X / 2.
succesfull_merge([First|Others], _) :- is_event_conflict_free_with_list(First, Others),!.
succesfull_merge(Lst) :- length(Lst, X),
						 succesfull_merge(Lst, X),!.
