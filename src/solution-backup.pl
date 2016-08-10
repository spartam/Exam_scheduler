%:- module(solution, 		[is_valid/1,
%							 is_valid_event/1,
%							 my_write/2]).
%:- use_module(auxilery, 	[in_interval/3,
%							 end/3]).

is_valid(schedule(EventList)) :-	nonvar(EventList),
									findall(EID, exam(EID, _), EL),
									length(EL, NumberOfExams),
									length(EventList, NumberOfExams),
									sort(EventList, Sorted),
									no_conflicts(Sorted, EL),!.

is_valid(schedule(EventList)) :-	findall(EID, exam(EID, _), EL),
									length(EL, NumberOfExams),
									length(EventList, NumberOfExams),
									no_conflicts(EventList, EL).

no_conflicts([], []) :- !.

no_conflicts([event(EID, RID, Day, Start) | Tail], [EID | RemainingEIDS]) :-	nonvar(EID),
																				nonvar(RID),
																				nonvar(Day),
																				nonvar(Start),
																				nonvar(Tail),
																				nonvar(RemainingEIDS),
																				length(Tail, RemainingLength),
																				length(RemainingEIDS, RemainingEIDSLength),
																				RemainingEIDSLength =:= RemainingLength, !,
																				no_conflicts_in_list(event(EID, RID, Day, Start), Tail),
																				no_conflicts(Tail, RemainingEIDS),!.

no_conflicts([event(EID, RID, Day, Start) | Tail], [EID | RemainingEIDS]) :-	length(Tail, RemainingLength),
																				length(RemainingEIDS, RemainingEIDSLength),
																				RemainingEIDSLength =:= RemainingLength, !,
																				no_conflicts_in_list(event(EID, RID, Day, Start), Tail),
																				no_conflicts(Tail, RemainingEIDS).

no_conflicts_in_list(Event, []) :-	is_valid_event(Event).
no_conflicts_in_list(Event, [Head | Tail]) :-	nonvar(Event),
												nonvar(Head),
												nonvar(Tail),
												conflict_free(Event, Head),
												no_conflicts_in_list(Event, Tail),!.
no_conflicts_in_list(Event, [Head | Tail]) :-	conflict_free(Event, Head),
												no_conflicts_in_list(Event, Tail).

conflict_free(Event1, Event2) :-	nonvar(Event1),
									nonvar(Event2),
									is_valid_event(Event1),
									is_valid_event(Event2),
									different_exam(Event1, Event2),
									no_clashes(Event1, Event2),!.

conflict_free(Event1, Event2) :-	is_valid_event(Event1),
									is_valid_event(Event2),
									different_exam(Event1, Event2),
									no_clashes(Event1, Event2).

no_clashes(Event1, Event2) :-	different_day(Event1, Event2),!.
no_clashes(Event1, Event2) :-	no_hour_overlap(Event1, Event2),!.
no_clashes(Event1, Event2) :-	different_teacher(Event1, Event2),
								different_room(Event1, Event2),!.

different_exam(event(EID, _, _, _), event(EID2, _, _, _)) :- EID \= EID2.
different_day(event(_, _, Day, _), event(_, _, Day2, _)) :- Day \= Day2.

no_hour_overlap(event(EID, _, _, Start), event(EID2, _, _, Start2)) :- 	end(EID, Start, End),
																		end(EID2, Start2, End2),
																		not(in_interval(Start, Start2, End2)),
																		not(in_interval(End, Start2, End2)), !.
no_hour_overlap(event(_, _, _, Start), event(EID2, _, _, Start2)) :-	end(EID2, Start2, End2),
																		Start =:= End2,!.
no_hour_overlap(event(EID, _, _, Start), event(_, _, _, Start2)) :-	end(EID, Start, End),
																		Start2 =:= End,!.


different_teacher(event(EID, _, _, _), event(EID2, _, _, _)) :- has_exam(CID, EID),
																teaches(LID, CID),
																has_exam(CID2, EID2),
																teaches(LID2, CID2),
																LID \= LID2.

different_room(event(_, RID, _, _), event(_, RID2, _, _)) :-	RID \= RID2. 


is_valid_event(event(EID, RID, Day, Start))	:-	nonvar(EID),
												nonvar(RID),
												nonvar(Day),
												nonvar(Start),
												exam(EID, _),
												room(RID, _),
												is_valid_room(Start, Day, RID, EID),!.

is_valid_event(event(EID, RID, Day, Start))	:-	exam(EID, _),
												room(RID, _),
												is_valid_room(Start, Day, RID, EID).

is_valid_room(Start, Day, RID, EID) :-	availability(RID, Day, Open, Close),
										in_interval(Start, Open, Close),
										end(EID, Start, End),
										in_interval(End, Open, Close),
										has_exam(CID, EID),
										capacity(RID, Capacity),
										findall(SID, follows(SID, CID), Students),
										length(Students, NumberOfStudents),
										Capacity >= NumberOfStudents.


during_exam_period(Day) :- 	first_day(FirstDay),
							last_day(LastDay),
							in_interval(Day, FirstDay, LastDay).
during_exam_period(_) :- fail, !.

my_write(Stream, Atom) :- 	write(Stream, Atom),
							nl(Stream).

