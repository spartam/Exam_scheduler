:- module(solution, [is_valid/1, is_valid_event/1, my_write/2, is_event_conflict_free_with_list/2]).

eid(event(EID, _, _, _), EID).
rid(event(_, RID, _, _), RID).
day(event(_, _, Day, _), Day).
start(event(_, _, _, Start), Start).

%% is_valid(?S)
%% generates all possible schedules or checks if the given schedule is valid
is_valid(schedule(EventList)) :-	nonvar(EventList),
									sort(EventList, Sorted),
									findall(EID, exam(EID, _), EIDS),
									is_conflict_free_list(Sorted, EIDS).

is_valid(schedule(EventList)) :-	findall(EID, exam(EID, _), EIDS),
									is_conflict_free_list(EventList, EIDS).

%% is_conflict_free_list(?EventArry, +EIDS)
is_conflict_free_list([], []).
is_conflict_free_list([event(EID, RID, Day, Start) | Events], [EID | EIDS]) :-	length(Events, EventsLength),
																				length(EIDS, EIDSLength),
																				EventsLength =:= EIDSLength, !,
																				is_event_conflict_free_with_list(event(EID, RID, Day, Start), Events),
																				is_conflict_free_list(Events, EIDS).
%% is_event_conflict_free_with_list(?Event, ?EventArray)
%% checks if there are any conflicts between an event and list
%% or generates any possible event with event list combination.
%% if the EIDS list is empty, the event should be valid.
is_event_conflict_free_with_list(Event, [])	:- is_valid_event(Event).

is_event_conflict_free_with_list(Event, [Head | Tail]) :-	is_conflict_free_pair(Event, Head),
															is_event_conflict_free_with_list(Event, Tail).

%% is_conflict_free_pair(?Event, ?Event)
%% checks if 2 events have conflicts between eachother
%% or calculates eacht possible pair of events that have no conflicts
%% whether one or none events are given.
%% uses conflict_free_events
is_conflict_free_pair(Event1, Event2) :-	is_valid_event(Event1),
											is_valid_event(Event2),
											eid(Event1, EID1),
											eid(Event2, EID2),
											EID1 \= EID2,
											conflict_free_events(Event1, Event2).

%%conflict_free_events(+E, +E)
%% checks if 2 given events have no conflicts towards eachother.
conflict_free_events(event(_, _, Day1, _), event(_, _, Day2, _)) :- 	Day1 \= Day2,!. %% both valid events on a different day == no conflicts
conflict_free_events(event(EID1, _, _, Start1), event(EID2, _, _, Start2)) :- 	duration(EID1, Duration1), %% on the same day but on timeslot overlap
																							End1 is Start1 + Duration1 - 1,
																							not(between(Start1, End1, Start2)),
																							duration(EID2, Duration2),
																							End2 is Start2 + Duration2 - 1,
																							not(between(Start2, End2, Start1)),!.
conflict_free_events(event(EID1, RID1, _, _), event(EID2, RID2, _, _)) :- 	RID1 \= RID2,	%% Same day, overlapping times, differen room, different teacher
																							has_exam(C1, EID1),
																							teaches(L1, C1),
																							has_exam(C2, EID2),
																							teaches(L2, C2),
																							L1 \= L2,!.

%% is_valid_event(?Event)
%% checks if a given event is valid
%% or calculates all possible valid events
is_valid_event(event(EID, RID, Day, Start)) :-	availability(RID, Day, Open, Close),
												duration(EID, Duration),
												End is Close - Duration,
												between(Open, End, Start),
												has_exam(CID, EID),
												capacity(RID, Capacity),
												findall(SID, follows(SID, CID), Students),
												length(Students, NumberOfStudents),
												Capacity >= NumberOfStudents.

%% my_write(+Stream, +Atom)
%% help function to write to file (debug uses)
my_write(Stream, Atom) :- 	write(Stream, Atom),
							nl(Stream).