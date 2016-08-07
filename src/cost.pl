:- module(cost, [cost/2,
				 sorted_cost_event_list/2]).

:- use_module(auxilery, 	[during_lunch_break/1,
							 in_interval/3,
							 end/3]).
:- use_module(solution2, 	[is_valid/1,
							 is_valid_event/1]).

fails_schedule(S):-	is_valid(S),
					not(cost(S, _)).

sorted_cost_event_list(EL, EID):- 	findall([Cost, event(EID, RID, Day, Hour)], event_cost(event(EID, RID, Day, Hour), Cost), ELUnsorted),
									sort(ELUnsorted, EL).

cost(Schedule, Cost):- 	nonvar(Schedule),
						is_valid(Schedule),
						calculate_cost(Schedule, Cost),!.

cost(Schedule, Cost):- 	is_valid(Schedule),
 						calculate_cost(Schedule, Cost).

calculate_cost(schedule(Events), Cost) :- 	calculate_cost(Events, StudentCost, LecturerCost),
											findall(LID, lecturer(LID, _), Lecturers),
											findall(SID, student(SID, _), Students),
											length(Lecturers, NumberOfLecturers),
											length(Students, NumberOfStudents),
											Cost is (LecturerCost / NumberOfLecturers) + (StudentCost / (NumberOfStudents * 2)), !.
calculate_cost([], 0, 0).
calculate_cost([Event | OtherEvents], StudentCost, TeacherCost) :-	calculate_cost(OtherEvents, OtherEventsStudentCost, OtherEventsTeacherCost),
																	calculate_b2b_cost(Event, OtherEvents, B2BStudent, B2BTeacher),
																	calculate_same_day_cost(Event, OtherEvents, SameDayStudent, SameDayLecturer),
																	calculate_event_cost(Event, EventStudentCost, EventTeacherCost),
																	StudentCost is EventStudentCost + B2BStudent + OtherEventsStudentCost + SameDayStudent,
																	TeacherCost is EventTeacherCost + B2BTeacher + OtherEventsTeacherCost + SameDayLecturer.

calculate_same_day_cost(_, [], 0, 0).
calculate_same_day_cost(event(EID, RID, Day, Start), [event(EID2, _, Day2, _) | Rest], StudentCost, LecturerCost) :- 	Day == Day2,
																														has_exam(CID, EID),
																														has_exam(CID2, EID2),
																														findall(SID, follows(SID, CID), C1S),
																														findall(SID, follows(SID, CID2), C2S),
																														intersection(C1S, C2S, BothStudent),
																														findall(LID, teaches(LID, CID), C1L),
																														findall(LID, teaches(LID, CID2), C2L),
																														intersection(C1L, C2L, BothLecturers),
																														calculate_same_day_cost(BothStudent, CurrentStudentCost),
																														calculate_same_day_cost(BothLecturers, CurrentLecturerCost),
																														calculate_same_day_cost(event(EID, RID, Day, Start), Rest, OtherStudentCosts, OtherLecturersCost),
																														StudentCost is OtherStudentCosts + CurrentStudentCost,
																														LecturerCost is OtherLecturersCost + CurrentLecturerCost.
																														
calculate_same_day_cost(event(EID, RID, Day, Start), [event(_, _, _, _) | Rest], StudentCost, LecturerCost) :- 	calculate_same_day_cost(event(EID, RID, Day, Start), Rest, StudentCost, LecturerCost).

calculate_same_day_cost([], 0).
calculate_same_day_cost([Current|Rest], Cost) :-	sc_same_day(Current, CurrentCost),
													calculate_same_day_cost(Rest, RestCost),
													Cost is CurrentCost + RestCost.
calculate_same_day_cost([_|Rest], Cost) :-	calculate_same_day_cost(Rest, Cost).

calculate_b2b_cost(_, [], 0, 0).
calculate_b2b_cost(event(EID, RID, Day, Start), [event(_, _, Day2, _) | Rest], StudentCost, LecturerCost) :-	Day \= Day2,
																												calculate_b2b_cost(event(EID, RID, Day, Start), Rest, StudentCost, LecturerCost).

calculate_b2b_cost(event(EID, RID, Day, Start), [event(EID2, _, _, Start2) | Rest], StudentCost, LecturerCost) :- 	not(end(EID, Start, Start2)),
																														not(end(EID2, Start2, Start)),
																														calculate_b2b_cost(event(EID, RID, Day, Start), Rest, StudentCost, LecturerCost).

calculate_b2b_cost(event(EID, RID, Day, Start), [event(EID2, _, _, _) | Rest], StudentCost, LecturerCost) :-	has_exam(CID, EID),
																														has_exam(CID2, EID2),
																														findall(SID, follows(SID, CID), C1S),
																														findall(SID, follows(SID, CID2), C2S),
																														intersection(C1S, C2S, BothStudent),
																														findall(LID, teaches(LID, CID), C1L),
																														findall(LID, teaches(LID, CID2), C2L),
																														intersection(C1L, C2L, BothLecturers),
																														calculate_b2b_cost(BothStudent, CurrentB2BStudentCost),
																														calculate_b2b_cost(BothLecturers, CurrentB2BLecturerCost),
																														calculate_b2b_cost(event(EID, RID, Day, Start), Rest, StudentRestCost, LecturerRestCost),
																														StudentCost is CurrentB2BStudentCost + StudentRestCost,
																														LecturerCost is CurrentB2BLecturerCost + LecturerRestCost,!.
calculate_b2b_cost([], 0).
calculate_b2b_cost([Current | Rest], Cost) :- 	sc_b2b(Current, CurrentCost),
												calculate_b2b_cost(Rest, RestCost),
												Cost is CurrentCost + RestCost, !.

event_cost(Event, Cost):- is_valid_event(Event),
						  calculate_event_cost(Event, StudentCost, LecturerCost),
						  Cost is StudentCost + LecturerCost.

calculate_event_cost(Event, StudentCost, LecturerCost):-	is_valid_event(Event),
															lecturer_cost(Event, LecturerCost),
															student_cost(Event, StudentCost).

lecturer_cost(Event, Cost):- 	lecturer_lunch_cost(Event, IWantTooEatAtThatMomentCost),
								lecturer_exam_in_period_cost(Event, IDontWantThatMomentCost),
								lecturer_not_in_period_cost(Event, IDontWantThatMomentEitherCost),
								Cost is IWantTooEatAtThatMomentCost + IDontWantThatMomentCost + IDontWantThatMomentEitherCost,!.

student_cost(Event, Cost):- student_lunch_cost(Event, IWantTooEatAtThatMomentCost),
							student_not_in_period_cost(Event, IDontWantThatMomentCost),
							Cost is IWantTooEatAtThatMomentCost + IDontWantThatMomentCost,!.


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