/*
	ScheduleEntries.pl

	Stundenplan:
	Eine Liste aus Einträgen der Form planeintrag(k,t,s,f,l,r) wobei
	k eine Klassenbezeichnung,
	t ein Wochentag,
	s ein Stundenname,
	f eine Fachbezeichnung,
	l ein Lehrername und
	r ein Raumname sind.
*/


% generateScheduleEntries (+Grid, +ClassAtoms, +DayAtoms, +HourAtoms, +CourseAtoms, +TeacherAtoms, +RoomAtoms -Stundenplan)
% -------------------------------------------------------------------------------------------------------------------------
scheduleEntries([], [], _, _, _, _, _, _).
scheduleEntries([Class | RestOfClasses], [ClassAtom | RestOfClassAtoms], DayAtoms, HourAtoms, CourseAtoms, TeacherAtoms, RoomAtoms, Stundenplan) :-
	entries_class(Class, ClassAtom, DayAtoms, HourAtoms, CourseAtoms, TeacherAtoms, RoomAtoms, Entries),
	scheduleEntries(RestOfClasses, RestOfClassAtoms, DayAtoms, HourAtoms, CourseAtoms, TeacherAtoms, RoomAtoms, RestOfStundenplan), !,
	append(RestOfStundenplan, Entries, Stundenplan).
	
	
entries_class([], _, [], _, _, _, _, _).
entries_class([Day | RestOfDays], ClassAtom, [DayAtom | RestOfDayAtoms], HourAtoms, CourseAtoms, TeacherAtoms, RoomAtoms, Entries) :-
	entries_day(Day, ClassAtom, DayAtom, HourAtoms, CourseAtoms, TeacherAtoms, RoomAtoms, DayEntries),
	entries_class(RestOfDays, ClassAtom, RestOfDayAtoms, HourAtoms, CourseAtoms, TeacherAtoms, RoomAtoms, RestOfEntries), !,
	append(RestOfEntries, DayEntries, Entries).
	
	
entries_day([], _, _, [], _, _, _, _).

entries_day([flr(0,0,0) | RestOfFlr], ClassAtom, DayAtom, [_ | RestOfHourAtoms], CourseAtoms, TeacherAtoms, RoomAtoms, Entries) :-
	entries_day(RestOfFlr, ClassAtom, DayAtom, RestOfHourAtoms, CourseAtoms, TeacherAtoms, RoomAtoms, Entries).
	
entries_day([flr(F,L,R) | RestOfFlr], ClassAtom, DayAtom, [HourAtom | RestOfHourAtoms], CourseAtoms, TeacherAtoms, RoomAtoms, Entries) :-
	entries_day(RestOfFlr, ClassAtom, DayAtom, RestOfHourAtoms, CourseAtoms, TeacherAtoms, RoomAtoms, RestOfEntries),
	nth(F, CourseAtoms, CourseAtom), !, 
	nth(L, TeacherAtoms, TeacherAtom),!,
	nth(R, RoomAtoms, RoomAtom), !,
	append(RestOfEntries, [planeintrag(ClassAtom, DayAtom, HourAtom, CourseAtom, TeacherAtom, RoomAtom)], Entries).