/*
	Constraints.pl
*/

% constrainGrid (+Grid)
% ---------------------
constrainGrid(Grid) :-

	% A room can be empty or have a course in it
	write('\t- A room can be empty or have a course in it'),nl,
	voidVoidVoidConstraint(Grid),
	
	% teachers can only teach certain courses and courses can only be taught in certain rooms
	write('\t- teachers can only teach certain courses and courses can only be taught in certain rooms'),nl,
	roomAndTeacherForCourse(Grid),
	
	% A class has to spend a certain amount of hours on each course
	write('\t- A class has to spend a certain amount of hours on each course'),nl,
	timeForCourse(Grid),
	
	% rooms can be closed at certain times
	write('\t- rooms can be closed at certain times'),nl,
	closedRooms(Grid),
	
	teacherLocks(Grid),
   
   	% Classes cannot share rooms and teachers
	write('\t- Classes cannot share rooms and teachers'),nl,
	dontShareRoomsAndTeachers(Grid),
	
	nl.
   




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
	
	differentOrBothZero([[R1, R2]]),
	differentOrBothZero([[L1, L2]]),
	dontShareRoomsAndTeachers(Grid, RestOfCombinations).

differentOrBothZero([]).
differentOrBothZero([[V1, V2] | RestOfPairs]) :-
	(V1 #= 0) #\/ (V2 #= 0) #\/ (V1 #\= V2),
	differentOrBothZero(RestOfPairs).


% A class has to spend a certain amount of hours on each course
% -------------------------------------------------------------
timeForCourse(Grid) :-
	classAtoms(ClassAtoms),
	translateClass(ClassAtoms, ClassIds),
	timeForCourse(Grid, ClassIds).
	
timeForCourse(_, []).
timeForCourse(Grid, [ClassId | RestOfClassId]) :-
	selectFLR(Grid, ClassId, _, _, FLR),
	listOfF(FLR, AllF),
	
	findall([CourseId, Hours], hoursForCourse(ClassId, CourseId, Hours), HoursForCourse),
	timeForCourse_apply(AllF, HoursForCourse),
	
	timeForCourse(Grid, RestOfClassId).
	
timeForCourse_apply(_, []).
timeForCourse_apply(AllF, [[CourseId, Hours] | RestOfHoursForCourse]) :-
	fd_exactly(Hours, AllF, CourseId),
	timeForCourse_apply(AllF, RestOfHoursForCourse).


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
	

% room is closed at Day:Hour
% --------------------------
closedRooms(Grid) :-
	findall([Room, Day, Hour], roomClosed(Room, Day, Hour), ClosedRoomsList),
	closedRooms(Grid, ClosedRoomsList).

closedRooms(_, []).
closedRooms(Grid, [[Room, Day, Hour] | RestOfClosedRooms]) :-
	selectSingleFLR(Grid, 1, Day, Hour, flr(_, _, R1)),
	selectSingleFLR(Grid, 2, Day, Hour, flr(_, _, R2)),
	R1 #\= Room,
	R2 #\= Room,
	closedRooms(Grid, RestOfClosedRooms).
	
	
% teacher is unavailable at Day:Hour
% ----------------------------------
teacherLocks(Grid) :-
	findall([Teacher, Day, Hour], teacherLock(Teacher, Day, Hour), LockedTeacherList),
	teacherLocks(Grid, LockedTeacherList).

teacherLocks(_, []).
teacherLocks(Grid, [[Teacher, Day, Hour] | RestOfLockedTeacherList]) :-
	selectSingleFLR(Grid, 1, Day, Hour, flr(_, L1, _)),
	selectSingleFLR(Grid, 2, Day, Hour, flr(_, L2, _)),
	L1 #\= Teacher,
	L2 #\= Teacher,
	closedRooms(Grid, RestOfLockedTeacherList).
	
	