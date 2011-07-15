/*
	GridGeneration.pl
*/

% generateGridWithDomains (+DayAtoms, +HourAtoms, +ClassAtoms, +CourseAtoms, +TeacherAtoms, +RoomAtoms, -Grid)
% ------------------------------------------------------------------------------------------------------------
generateGridWithDomains(DayAtoms, HourAtoms, ClassAtoms, CourseAtoms, TeacherAtoms, RoomAtoms, Grid) :-

	length(DayAtoms, NumberOfDays),
	length(HourAtoms, NumberOfHours),
	length(ClassAtoms, NumberOfClasses),
	length(CourseAtoms, NumberOfCourses),
	length(TeacherAtoms, NumberOfTeachers),
	length(RoomAtoms, NumberOfRooms),
	
	% create grid
	length(Grid, NumberOfClasses),
	fillWithDaysAndHours(Grid, NumberOfDays, NumberOfHours),
	
	% apply domains and add flr syntax
	selectFLR(Grid, _, _, _, AllFLR),
	applyDomains(AllFLR, NumberOfCourses, NumberOfTeachers, NumberOfRooms), !.

	
% fillWithDaysAndHours(+Class, +NumberOfDays, +NumberOfHours)
% -----------------------------------------------------------
fillWithDaysAndHours([], _, _).
fillWithDaysAndHours([Class | RestOfClasses], NumberOfDays, NumberOfHours) :-
	length(Class, NumberOfDays),
	fillWithHours(Class, NumberOfHours),
	fillWithDaysAndHours(RestOfClasses, NumberOfDays, NumberOfHours).

fillWithHours([], _).
fillWithHours([Day | RestOfDays], NumberOfHours) :-
	length(Day, NumberOfHours),
	fillWithHours(RestOfDays, NumberOfHours).


% applyDomains(+ListOfFlr, +NumberOfCourses, +NumberOfTeachers, +NumberOfRooms)
% -----------------------------------------------------------------------------
applyDomains([], _, _, _).
applyDomains([flr(F, L, R) | RestOfFlrs], NumberOfCourses, NumberOfTeachers, NumberOfRooms) :-
	fd_domain(F, 0, NumberOfCourses),
	fd_domain(L, 0, NumberOfTeachers),
	fd_domain(R, 0, NumberOfRooms),
	applyDomains(RestOfFlrs, NumberOfCourses, NumberOfTeachers, NumberOfRooms).
	
	
	
	
	
	
	
	
	
	