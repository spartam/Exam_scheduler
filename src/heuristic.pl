:- module(heuristic, 	[find_sorted_events/2,
						 find_heuristically/1]).
:- use_module(cost, [sorted_cost_event_list/2]).
:- use_module(solution, 	[is_valid/1,
							 my_write/2,
							 is_event_conflict_free_with_list/2]).

%% find_sorted_events(-SCL, +EIDS)
%% creates for each exam with EID in EIDS a list of valid events sorted on cost.
find_sorted_events([], []).
find_sorted_events([CurrentEventSortedList|Rest], [EID|EIDS]) :- 	sorted_cost_event_list(CurrentEventSortedList, EID),
																	find_sorted_events(Rest, EIDS).

%% find_heuristically(-S)
%% basically a merge sort.
%% we split the problem is sub problems and try to find a solution for the sub problem and merging the solutions of the subproblems to see if
%% it yields a solution for the higher subproblem
%% we only take the cost of the event themselves into account and don't look to the costs 2 events may have on eachother.
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

%% succesfull_merge(+Lst, +X)
%% see if a merge was succesfull by checking the constraints of the first half over all other events.
succesfull_merge(Lst, X) :-	length(Lst, Y),
							Y < X / 2.
succesfull_merge([First|Others], _) :- is_event_conflict_free_with_list(First, Others),!.

%% succesfull_merge(+Lst)
%% calls succesfull_merge(+Lst, +X)
succesfull_merge(Lst) :- length(Lst, X),
						 succesfull_merge(Lst, X),!.
