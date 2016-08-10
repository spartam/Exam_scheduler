:- module(cost, [cost/2,
				 sorted_cost_event_list/2]).

:- use_module(auxilery, 	[during_lunch_break/1,
							 in_interval/3,
							 end/3]).
:- use_module(solution, 	[is_valid/1,
							 is_valid_event/1]).

%% sorted_cost_event_list(-EL, +EID)
%% Generates a list events of exams with EID sorted on cost
sorted_cost_event_list(EL, EID):- 	findall([Cost, event(EID, RID, Day, Hour)], event_cost(event(EID, RID, Day, Hour), Cost), ELUnsorted),
									sort(ELUnsorted, EL).

%% cost(?Schedule, ?Cost)
%% calculates all schedules and there costs.
%% calculates all schedules given a certain cost
%% calculates the cost of a given schedule
cost(Schedule, Cost):- 	nonvar(Schedule),
						is_valid(Schedule),
						calculate_cost(Schedule, Cost),!.

cost(Schedule, Cost):- 	is_valid(Schedule),
 						calculate_cost(Schedule, Cost).

%% calculate_cost(+Events, -Cost)
%% calculates the cost of an EventsList
calculate_cost(schedule(Events), Cost) :- 	calculate_cost(Events, StudentCost, LecturerCost),
											findall(LID, lecturer(LID, _), Lecturers),
											findall(SID, student(SID, _), Students),
											length(Lecturers, NumberOfLecturers),
											length(Students, NumberOfStudents),
											Cost is (LecturerCost / NumberOfLecturers) + (StudentCost / (NumberOfStudents * 2)), !.

%% calculate_cost(+Events, -StudentCost, -TeacherCost)
%% calculates the cost for teachers and students separatly of a given EventsList
calculate_cost([], 0, 0).
calculate_cost([Event | OtherEvents], StudentCost, TeacherCost) :-	calculate_cost(OtherEvents, OtherEventsStudentCost, OtherEventsTeacherCost),
																	calculate_b2b_cost(Event, OtherEvents, B2BStudent, B2BTeacher),
																	calculate_same_day_cost(Event, OtherEvents, SameDayStudent, SameDayLecturer),
																	calculate_event_cost(Event, EventStudentCost, EventTeacherCost),
																	StudentCost is EventStudentCost + B2BStudent + OtherEventsStudentCost + SameDayStudent,
																	TeacherCost is EventTeacherCost + B2BTeacher + OtherEventsTeacherCost + SameDayLecturer.

%% calculate_same_day_cost(+Event, +EventList, -StudentCost, -TeacherCost)
%% calculates the teacher and student cost for an event taking place on the same day of events in the list
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

%% calculate_same_day_cost(+PID, -Cost)
%% adds the costs of the PIDS to the total same day costs penalty
calculate_same_day_cost([], 0).
calculate_same_day_cost([Current|Rest], Cost) :-	sc_same_day(Current, CurrentCost),
													calculate_same_day_cost(Rest, RestCost),
													Cost is CurrentCost + RestCost.
calculate_same_day_cost([_|Rest], Cost) :-	calculate_same_day_cost(Rest, Cost).

%% calculate_b2b_cost(+Event, +Events, -StudentCost, -TeacherCost)
%% calculates the cost for events taking place B2B
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

%% calculate_b2b_cost(+PIDS, -Cost)
%% calculates the B2B cost of a given list of people
calculate_b2b_cost([], 0).
calculate_b2b_cost([Current | Rest], Cost) :- 	sc_b2b(Current, CurrentCost),
												calculate_b2b_cost(Rest, RestCost),
												Cost is CurrentCost + RestCost, !.

%% event_cost(?Event, ?Cost)
%% calculates all possible events and their cost
%% Generates all events with a certain cost
%% Generates the cost of a given Event
%% checks if a given event has the given cost
%% only used when only the cost of the event itself is required
event_cost(event(EID, RID, Day, Start), Cost):- is_valid_event(event(EID, RID, Day, Start)),
						  calculate_event_cost(event(EID, RID, Day, Start), StudentCost, LecturerCost),
						  has_exam(CID, EID),
						  findall(SID, follows(SID, CID), CS),
						  findall(LID, teaches(LID, CID), CL),
						  length(CS, CSC),
						  length(CL, CLC),
						  Cost is ((StudentCost / CSC) + (LecturerCost / CLC)) * 2.

%% calculate_event_cost(?Event, ?StudentCost, ?LecturerCost)
%% calculates the teacher and studentcost of an event.
%% calculates the event with a certain studentcost and / or TeacherCost
calculate_event_cost(Event, StudentCost, LecturerCost):-	is_valid_event(Event),
															lecturer_cost(Event, LecturerCost),
															student_cost(Event, StudentCost).

%% lecturer_cost(+Event, -Cost)
%% calculates teacher cost of a given event
lecturer_cost(Event, Cost):- 	lecturer_lunch_cost(Event, IWantTooEatAtThatMomentCost),
								lecturer_exam_in_period_cost(Event, IDontWantThatMomentCost),
								lecturer_not_in_period_cost(Event, IDontWantThatMomentEitherCost),
								Cost is IWantTooEatAtThatMomentCost + IDontWantThatMomentCost + IDontWantThatMomentEitherCost,!.

%% student cost(+Event, -Cost)
%% calculates student cost of a given event
student_cost(Event, Cost):- student_lunch_cost(Event, IWantTooEatAtThatMomentCost),
							student_not_in_period_cost(Event, IDontWantThatMomentCost),
							Cost is IWantTooEatAtThatMomentCost + IDontWantThatMomentCost,!.


%% Lecturers
%% CODE DUPLICATION!!
%% NOOOO
%% What did you do?
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