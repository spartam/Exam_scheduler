:- module(cost, [cost/2,
				 sorted_cost_event_list/2]).

:- use_module(auxilery, 	[during_lunch_break/1,
							 in_interval/3,
							 end/3]).
:- use_module(solution, 	[is_valid/1,
							 is_valid_event/1]).

fails_schedule(S):-	is_valid(S),
					not(cost(S, _)).

sorted_cost_event_list(EL, EID):- 	findall(event(EID, RID, Day, Hour), calculate_event_cost(event(EID, RID, Day, Hour), _), ELUnsorted),
									sort(ELUnsorted, EL).

cost(Schedule, Cost):- 	nonvar(Schedule),
						is_valid(Schedule),
						calculate_cost(Schedule, Cost),!.

cost(Schedule, Cost):- is_valid(Schedule),
 						calculate_cost(Schedule, Cost).

calculate_cost(schedule(Events), Cost) :- calculate_cost(Events, Cost).
calculate_cost([], 0).
calculate_cost([Event | OtherEvents], Cost) :-	calculate_cost(OtherEvents, OtherEventsCost),
												calculate_event_cost(Event, EventCost),
												Cost is EventCost + OtherEventsCost.

calculate_event_cost(Event, EventCost):-	is_valid_event(Event),
											lecturer_cost(Event, LecturerCost),
											student_cost(Event, StudentCost),
											findall(LID, lecturer(LID, _), Lecturers),
											findall(SID, student(SID, _), Students),
											length(Lecturers, NumberOfLecturers),
											length(Students, NumberOfStudents),
											EventCost is (LecturerCost / NumberOfLecturers) + (StudentCost / (NumberOfStudents * 2)).

lecturer_cost(Event, Cost):- 	lecturer_lunch_cost(Event, LunchCost),
								lecturer_exam_in_period_cost(Event, IDontWantThatMomentCost),
								lecturer_not_in_period_cost(Event, IDontWantThatMomentEitherCost),
								Cost is LunchCost + IDontWantThatMomentCost + IDontWantThatMomentEitherCost,!.

student_cost(Event, Cost):- student_lunch_cost(Event, LunchCost),
							student_not_in_period_cost(Event, IDontWantThatMomentCost),
							Cost is LunchCost + IDontWantThatMomentCost,!.


%% Lecturers
lecturer_lunch_cost(Event, 0):- not(during_lunch_break(Event)),!.
lecturer_lunch_cost(event(EID, _, _, _), Cost) :-	has_exam(CID, EID),
													findall(LID, teaches(LID, CID), LIDS),
													lunch_cost(LIDS, Cost),!.

lecturer_exam_in_period_cost(event(EID, _, Day, Start), Cost) :-	has_exam(CID, EID),
																	findall(LID, teaches(LID, CID), LIDS),
																	lecturer_exam_in_period_cost(LIDS, EID, Day, Start, Cost),!.

lecturer_exam_in_period_cost([], _, _, _, 0).
lecturer_exam_in_period_cost([LID|LIDS], EID, Day, Start, Cost) :-	sc_no_exam_in_period(LID, Day, From, Till, _),
																	NoOverlapTill is Till - 1,
																	not(in_interval(Start, From, NoOverlapTill)),
																	end(EID, Start, End),
																	NoOverlapEnd is End - 1,
																	not(in_interval(NoOverlapEnd, From, Till)),
																	lecturer_exam_in_period_cost(LIDS, EID, Day, Start, Cost), !.

									
lecturer_exam_in_period_cost([LID|LIDS], EID, Day, Start, Cost) :-	sc_no_exam_in_period(LID, Day, _, _, LIDCost),
																	lecturer_exam_in_period_cost(LIDS, EID, Day, Start, RemainingCost),
																	Cost is LIDCost + RemainingCost,!.

lecturer_exam_in_period_cost([_|LIDS], EID, Day, Start, Cost) :-	lecturer_exam_in_period_cost(LIDS, EID, Day, Start, Cost), !.

lecturer_not_in_period_cost(event(EID, _, Day, Start), Cost) :- 	has_exam(CID, EID),
																	findall(LID, teaches(LID, CID), LIDS),
																	not_in_period_cost(LIDS, EID, Day, Start, Cost),!.


%% students cost
student_lunch_cost(Event, 0):- not(during_lunch_break(Event)), !.
student_lunch_cost(event(EID, _, _, _), Cost):-	has_exam(CID, EID),
												findall(SID, follows(SID, CID), SIDS),
												lunch_cost(SIDS, Cost),!.

student_not_in_period_cost(event(EID, _, Day, Start), Cost) :- 	has_exam(CID, EID),
																findall(SID, follows(SID, CID), SIDS),
																not_in_period_cost(SIDS, EID, Day, Start, Cost),!.

%% Aiding functions that can be used for both
lunch_cost([], 0).
lunch_cost([PID | PIDS], Cost) :-	sc_lunch_break(PID, Penalty),
 									lunch_cost(PIDS, RemainingCost),
 									Cost is Penalty + RemainingCost, !.

not_in_period_cost([], _, _, _, 0).
not_in_period_cost([PID|PIDS], EID, Day, Start, Cost):-	sc_not_in_period(PID, EID, Day, From, Till, _),
														NoOverlapTill is Till - 1,
														not(in_interval(Start, From, NoOverlapTill)),
														end(EID, Start, End),
														NoOverlapEnd is End - 1,
														not(in_interval(NoOverlapEnd, From, Till)),
														not_in_period_cost(PIDS, EID, Day, Start, Cost), !.

not_in_period_cost([PID|PIDS], EID, Day, Start, Cost):- sc_not_in_period(PID, EID, Day, _, _, Penalty),
														not_in_period_cost(PIDS, EID, Day, Start, RemainingCost),
														Cost is Penalty + RemainingCost, !.

not_in_period_cost([_|PIDS], EID, Day, Start, Cost):-	not_in_period_cost(PIDS, EID, Day, Start, Cost), !.