:-module(print, 			[pretty_print/3]).

:- use_module(auxilery, 	[in_interval/3,
							 end/3]).

pretty_print(schedule(EventArray)) :-	first_day(Begin),
										last_day(End),
										findall(Day, in_interval(Day, Begin, End), Days),
										pretty_print(EventArray, Days).

pretty_print(_, []).
pretty_print(EventArray, [CurrentDay | RemainingDays]) :-	filter_by_day(EventArray, CurrentDay, Filtered),
															chronological(Filtered, Sorted),
															print(CurrentDay, Sorted),
															pretty_print(EventArray, RemainingDays).

filter_by_day([], _, []).
filter_by_day([event(EID, RID, Day, Start) | RemainingEvents], Day, [event(EID, RID, Day, Start) | FilteredEvents]) :-	filter_by_day(RemainingEvents, Day, FilteredEvents), !.
filter_by_day([event(EID, RID, Day, Start) | RemainingEvents], Day2, FilteredEvents) :-	filter_by_day(RemainingEvents, Day2, FilteredEvents), !.

chronological([], []).
chronological([event(EID, RID, Day, Start)], [event(EID, RID, Day, Start)]]).
chronological([event(EID, RID, Day, Start) | RemainingEvents], Sorted) :- 	chronological(RemainingEvents, List),
																			insert(event(EID, RID, Day, Start), List, Sorted), !.

insert(event(EID, RID, Day, Start), [event(EID2, RID2, Day2, Start2) | RemainingEvents], [event(EID, RID, Day, Start), event(EID2, RID2, Day2, Start2) | RemainingEvents]) :-	Start < Start2, !.
insert(event(EID, RID, Day, Start), [event(EID2, RID2, Day2, Start2) | RemainingEvents], [event(EID2, RID2, Day2, Start2) | Sorted]) :-	insert(event(EID, RID, Day, Start), RemainingEvents, Sorted), !.
insert(event(EID, RID, Day, Start), [], [event(EID, RID, Day, Start)]).


print(Day, Events) :-	length(Events, X),
						X > 0, !,
						format('~n~n ***** ~w ~w ***** ~n', ['DAY', Day]),
						findall(RID, room(RID, RoomName), Rooms),
						print_per_room(Events, Rooms),!.


print_per_room([], []).
print_per_room(Events, [RID | Rooms]) :-	room(RID, RoomName),
											filter_by_room(Events, RID, Filtered),
											print_rooms(RoomName, Filtered),
											print_per_room(Events, Rooms).
print_per_room(_, _).

filter_by_room([], _, []).
filter_by_room([event(EID, RID, Day, Start) | RemainingEvents], RID, [event(EID, RID, Day, Start) | FilteredEvents]) :-	filter_by_room(RemainingEvents, RID, FilteredEvents), !.
filter_by_room([event(EID, RID, Day, Start) | RemainingEvents], RID2, FilteredEvents) :- filter_by_room(RemainingEvents, RID2, FilteredEvents), !.

print_rooms(RoomName, RoomEvents) :- 	format('~n~w ~w~n', ['ROOM: ', RoomName]),
										print_events(RoomEvents).


print_events([]).
print_events([event(EID, RID, Day, Start) | Events]) :- has_exam(CID, EID),
														end(EID, Start, End),
														course(CID, Cname),
														teaches(LID, CID),
														lecturer(LID, Lname),
														format('~w~w~w~w ~w ~w ~w ~w~n', [Start, ':00-', End, ':00', Cname, '(', Lname, ')']),
														print_events(Events), !.
print(_, _).