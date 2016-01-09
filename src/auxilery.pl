:- module(auxilery, 	[in_interval/3,
						 end/3,
						 during_lunch_break/1]).

start_lunch_break(12).
end_lunch_break(13).

%% End(+EID, +Start, -End)
%% calculates the End time of an exam
end(EID, Start, End) :- duration(EID, Duration),
						in_interval(Start, 0, 23),
						End is Start + Duration, !.


%% max(+X, +Y, ?Z)
%% gives the max of 2 given numbers
%% returns true if Z is the max
max(X, Y, Z) :- X > Y,
				Z is X, !.

max(X, Y, Z) :- Y >= X ,
				Z is Y, !.
max(_, _, _) :- fail, !.

%% min(+X, +Y, -Z)
%% gives the max of 2 given numbers
%% returns true if Z is the min
min(X, Y, Z) :- X > Y,
				Z is Y, !.

min(X, Y, Z) :- Y >= X ,
				Z is X, !.
min(_, _, _) :- fail, !.


%% in_interval(?X, +Start, +End)
%% returns true if X is in the interval between Start and End
%% or calculates all numbers between the interval
in_interval(X, Start, End) :- 	nonvar(X),
								X >= Start,
								End >= X, !.

in_interval(X, Start, _) :- 	X is Start.
in_interval(X, Start, End) :- 	Start \= End,
								min(Start, End, Begin),
								max(Start, End, NewEnd),
								NewStart is Begin + 1,
								in_interval(X, NewStart, NewEnd).

%% during_lunch_break(+Event)
%% returns True if the event takes place during lunch
%% The validity of the event must be checked before calling this function.
during_lunch_break(event(EID, _, _, Start)) :- 	end(EID, Start, End),
												start_lunch_break(LCStart),
												end_lunch_break(LCStop),
												Start =< LCStart,
												End >= LCStop,!.