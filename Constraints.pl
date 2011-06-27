/*
	Constraints.pl

	FIXME: Fach ohne Raumeinschränkungen: Kann in jedem Raum stattfinden
	
*/

% constrainGrid (+Grid)
% ---------------------
constrainGrid(Grid) :-

	% A room can be empty or have a course in it
	voidVoidVoidConstraint(Grid),
   
   	% Classes cannot share rooms and teachers
	dontShareRoomsAndTeachers(Grid),
   
	% teachers can only teach certain courses and courses can only be taught in certain rooms
	roomAndTeacherForCourse(Grid),
	
	% rooms can be closed at certain times
	closedRooms(Grid),
	
	% A class has to spend a certain amount of hours on each course
	timeForCourse(Grid).


% constraintWithLog (+Constraint)
% -------------------------------
constraintWithLog(Constraint) :-
	write(':: Establishing constraint: '), write(Constraint), nl,
	Constraint.


% voidVoidVoidConstraint (+Grid)
% ------------------------------
voidVoidVoidConstraint(Grid) :-
	selectFLR(Grid, _, _, _, ListOfFLR),
	voidVoidVoidConstraint_loop(ListOfFLR).
	
voidVoidVoidConstraint_loop([]).
voidVoidVoidConstraint_loop([flr(F, L, R) | RestOfFLR]) :-
	(F #= 0 #/\ L #= 0 #/\ R #= 0) #\/ (F #\= 0 #/\ L #\= 0 #/\ R #\=0),
	voidVoidVoidConstraint_loop(RestOfFLR).
	
	
% Classes cannot share rooms and teachers
% ---------------------------------------
% FIXME: Noch hardgecodet für zwei klassen.
dontShareRoomsAndTeachers(Grid) :-
	findall(
		[DayId, HourId],
		(
			dayAtoms(DayAtoms),
			hourAtoms(HourAtoms),
			member(Day, DayAtoms),
			member(Hour, HourAtoms),
			nth(DayId, DayAtoms, Day),
			nth(HourId, HourAtoms, Hour)
		),
		DayHourCombinations
	),
	dontShareRoomsAndTeachers(Grid, DayHourCombinations).

dontShareRoomsAndTeachers(_, []).
dontShareRoomsAndTeachers(Grid, [[Day, Hour] | RestOfCombinations]) :-
	selectSingleFLR(Grid, 1, Day, Hour, flr(_, L1, R1)),
	selectSingleFLR(Grid, 2, Day, Hour, flr(_, L2, R2)),
	selectSingleFLR(Grid, 3, Day, Hour, flr(_, L3, R3)),
	
	differentOrBothZero([[R1, R2], [R1, R3], [R2, R3]]),
	differentOrBothZero([[L1, L2], [L1, L3], [L2, L3]]),
	dontShareRoomsAndTeachers(Grid, RestOfCombinations).

differentOrBothZero([]).
differentOrBothZero([[V1, V2] | RestOfPairs]) :-
	(V1 #= 0) #\/ (V2 #= 0) #\/ (V1 #\= V2),
	differentOrBothZero(RestOfPairs).


% A class has to spend a certain amount of hours on each course
% -------------------------------------------------------------
timeForCourse(Grid) :-
	findall([ClassId, CourseId, Hours], hoursForCourse(ClassId, CourseId, Hours), HoursForCourse),
	timeForCourse(Grid, HoursForCourse).

timeForCourse(_, []).
timeForCourse(Grid, [[ClassId, CourseId, Hours]|T]) :-
	selectFLR(Grid, ClassId, _, _, FLR),
	listOfF(FLR, AllF),
	fd_exactly(Hours, AllF, CourseId),
	timeForCourse(Grid, T).


% teachers can only teach certain courses and courses can only be taught in certain rooms
% ---------------------------------------------------------------------------------------
roomAndTeacherForCourse(Grid) :-
	selectFLR(Grid, _, _, _, ListOfFLR),
	teachersForCourses(ListOfFLR),
	roomForCourse(ListOfFLR).
	
teachersForCourses([]).
teachersForCourses([flr(F, L, _) | RestOfFLR]) :-
	bagof([Course, Teacher], teacherForCourse(Course, Teacher), List),
	append(List, [[0,0]], NewList),
	fd_relation(NewList, [F,L]),
	teachersForCourses(RestOfFLR).

roomForCourse([]).
roomForCourse([flr(F, _, R) | RestOfFLR]) :-
	bagof([Course,Room], courseInRoom(Course, Room), List),
	append(List, [[0,0]], NewList),
	fd_relation(NewList, [F,R]),
	roomForCourse(RestOfFLR).
	

% rooms is closed at certain times
% --------------------------------

closedRooms(Grid) :-
	findall([Room, Day, Hour], roomClosed(Room, Day, Hour), ClosedRoomsList),
	closedRooms(Grid, ClosedRoomsList).

closedRooms(_, []).
closedRooms(Grid, [[Room, Day, Hour] | RestOfClosedRooms]) :-
% FIXME: 2 Klassen hardcoding flexibel machen
	selectSingleFLR(Grid, 1, Day, Hour, flr(_, _, R1)),
	selectSingleFLR(Grid, 2, Day, Hour, flr(_, _, R2)),
	selectSingleFLR(Grid, 3, Day, Hour, flr(_, _, R3)),
	
	R1 #\= Room,
	R2 #\= Room,
	R3 #\= Room,
	closedRooms(Grid, RestOfClosedRooms).
	
	