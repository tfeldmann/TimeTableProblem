/*
	Parser.pl
	
	TODO:
  		- mehrere Stunden- und Tagesdeklarationen können vereinigt werden
    	- mehrere Fachraumdeklarationen resultieren in einer zusammengeführten Menge
		- mehrere Fachlehrer resultieren in einer zusammengeführten Menge
*/

/*	
	- lehrersperre(l,[t1,s1,t2,s2,...])
	deklariert den Lehrer l als nicht verfügbar
	für die angegebenen Zeiten, wobei
	ti sind Tag- und si Stundenbezeichnungen
	
	- doppelstunden([f1,f2,...])
	deklariert, dass der Unterricht in Fach f immer nur in Doppelstunden stattfinden
	darf (z.B. bei Sport),wobei f_n Fachbezeichnungen sind.
*/

:- dynamic(dayAtoms/1).
:- dynamic(hourAtoms/1).
:- dynamic(classAtoms/1).
:- dynamic(courseAtoms/1).
:- dynamic(teacherAtoms/1).
:- dynamic(roomAtoms/1).

:- dynamic(hoursForCourse/3).
:- dynamic(teacherForCourse/2).
:- dynamic(courseInRoom/2).
:- dynamic(roomClosed/3).

% parseTTP (+TTP, -DayAtoms, -HourAtoms, -ClassAtoms, -CourseAtoms, -TeacherAtoms, -RoomAtoms)
% --------------------------------------------------------------------------------------------
parseTTP(TTP, DayAtoms, HourAtoms, ClassAtoms, CourseAtoms, TeacherAtoms, RoomAtoms) :-

	getDayAtoms(TTP, DayAtoms),	
	getHourAtoms(TTP, HourAtoms),		
	getClassAtoms(TTP, ClassAtoms),		
	getCourseAtoms(TTP, CourseAtoms),	
	getTeacherAtoms(TTP, TeacherAtoms),	
	getRoomAtoms(TTP, RoomAtoms),
	
	asserta(dayAtoms(DayAtoms)),
	asserta(hourAtoms(HourAtoms)),
	asserta(classAtoms(ClassAtoms)),
	asserta(courseAtoms(CourseAtoms)),
	asserta(teacherAtoms(TeacherAtoms)),
	asserta(roomAtoms(RoomAtoms)),
	
	assertCourseDurationsForClasses(TTP),
	assertTeachersForClasses(TTP),
	assertCourseInRoom(TTP),
	assertClosedRooms(TTP).


% subroutines used in parseTTP/7
% ------------------------------
getDayAtoms([], _) :- write('Keine Tage deklariert'), fail.
getDayAtoms([tage(DayAtoms) | _], DayAtoms) :- !.
getDayAtoms([_ | RestOfTTP], DayAtoms) :-
	getDayAtoms(RestOfTTP, DayAtoms).

getHourAtoms([], _) :- write('Keine möglichen Unterrichtsstunden deklariert'), fail.
getHourAtoms([stunden(HourAtoms) | _], HourAtoms) :- !.
getHourAtoms([_ | RestOfTTP], HourAtoms) :-
	getHourAtoms(RestOfTTP, HourAtoms).

getClassAtoms([], _).
getClassAtoms([curriculum(Class, _) | RestOfTTP], NewClassAtoms) :-
	getClassAtoms(RestOfTTP, ClassAtoms),
	append(ClassAtoms, [Class], NewClassAtoms).
getClassAtoms([_ | RestOfTTP], ClassAtoms) :-
	getClassAtoms(RestOfTTP, ClassAtoms).

getCourseAtoms([], _).
getCourseAtoms([fachlehrer(Course, _) | RestOfTTP], NewCourseAtoms) :-
	getCourseAtoms(RestOfTTP, CourseAtoms),
	union(CourseAtoms, [Course], NewCourseAtoms).
getCourseAtoms([_ | RestOfTTP], CourseAtoms) :-
	getCourseAtoms(RestOfTTP, CourseAtoms).

getTeacherAtoms([], _).
getTeacherAtoms([fachlehrer(_, Teacher) | RestOfTTP], NewTeacherAtoms) :-
	getTeacherAtoms(RestOfTTP, TeacherAtoms),
	union(TeacherAtoms, Teacher, NewTeacherAtoms).
getTeacherAtoms([_ | RestOfTTP], TeacherAtoms) :-
	getTeacherAtoms(RestOfTTP, TeacherAtoms).

getRoomAtoms([], _).
getRoomAtoms([fachraum(_, Room) | RestOfTTP], NewRoomAtoms) :-
	getRoomAtoms(RestOfTTP, RoomAtoms),
	union(RoomAtoms, Room, NewRoomAtoms).
getRoomAtoms([_ | RestOfTTP], RoomAtoms) :-
	getRoomAtoms(RestOfTTP, RoomAtoms).


% assertCourseDurationsForClasses (+TTP)
%
% asserts predicates in form of "hoursForCourse(class, course, duration).
% -----------------------------------------------------------------------
assertCourseDurationsForClasses([]).
assertCourseDurationsForClasses([curriculum(Class, ListOfDurations) | RestOfTTP]) :-
	assertDurations(Class, ListOfDurations),
	assertCourseDurationsForClasses(RestOfTTP).
assertCourseDurationsForClasses([_ | RestOfTTP]) :-
	assertCourseDurationsForClasses(RestOfTTP).

assertDurations(_, []).
assertDurations(Class, [Course, Duration | RestOfDurations]) :-
	classAtoms(ClassAtoms),
	courseAtoms(CourseAtoms),
	nth(ClassId, ClassAtoms, Class),
	nth(CourseId, CourseAtoms, Course),
	asserta(hoursForCourse(ClassId, CourseId, Duration)),
	assertDurations(Class, RestOfDurations).
assertDurations(_, [_]) :- write('Liste hat eine ungerade Anzahl an Elementen').


% assertTeachersForClasses (+TTP)
%
% asserts predicates in form of "teacherForCourse(Course, Teacher).
% -----------------------------------------------------------------------
assertTeachersForClasses([]).
assertTeachersForClasses([fachlehrer(Course, ListOfTeachers) | RestOfTTP]) :-
	assertTeachers(Course, ListOfTeachers),
	assertTeachersForClasses(RestOfTTP).
assertTeachersForClasses([_ | RestOfTTP]) :-
	assertTeachersForClasses(RestOfTTP).

assertTeachers(_, []).
assertTeachers(Course, [Teacher | RestOfTeachers]) :-

	% translate to IDs
	courseAtoms(CourseAtoms),
	teacherAtoms(TeacherAtoms),
	nth(CourseId, CourseAtoms, Course),
	nth(TeacherId, TeacherAtoms, Teacher),
	
	asserta(teacherForCourse(CourseId, TeacherId)),
	assertTeachers(Course, RestOfTeachers).
	
	
% assertCourseInRoom (+TTP)
% -------------------------
assertCourseInRoom([]).
assertCourseInRoom([fachraum(Course, ListOfRooms) | RestOfTTP]) :-
	assertCourseInRoom(Course, ListOfRooms),
	assertCourseInRoom(RestOfTTP).
assertCourseInRoom([_ | RestOfTTP]) :-
	assertCourseInRoom(RestOfTTP).

assertCourseInRoom(_, []).
assertCourseInRoom(Course, [Room | RestOfRooms]) :-
	courseAtoms(CourseAtoms),
	roomAtoms(RoomAtoms),
	nth(CourseId, CourseAtoms, Course),
	nth(RoomId, RoomAtoms, Room),
	
	asserta(courseInRoom(CourseId, RoomId)),
	assertCourseInRoom(Course, RestOfRooms).
	
	
% assertClosedRoom (+TTP)
%
%	- raumsperre(r,[t1,s1,t2,s2,...])
%		deklariert den Raum r als nicht verfügbar für die angegebenen
%		Zeiten, wobei t_n sind Tag- und s_n Stundenbezeichnungen
% ---------------------------------------------------------------------
assertClosedRooms([]).
assertClosedRooms([raumsperre(Room, ListOfDayAndTime) | RestOfTTP]) :-
	assertClosedRooms(Room, ListOfDayAndTime),
	assertClosedRooms(RestOfTTP).
assertClosedRooms([_ | RestOfTTP]) :-
	assertClosedRooms(RestOfTTP).
	
assertClosedRooms(_, []).
assertClosedRooms(Room, [Day, Hour | RestOfTime]) :-
	roomAtoms(RoomAtoms),
	dayAtoms(DayAtoms),
	hourAtoms(HourAtoms),
	nth(RoomId, RoomAtoms, Room),
	nth(DayId, DayAtoms, Day),
	nth(HourId, HourAtoms, Hour),
	
	asserta(roomClosed(RoomId, DayId, HourId)),
	assertClosedRooms(Room, RestOfTime).
