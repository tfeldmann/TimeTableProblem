/*
	CommonFunctions.pl
	
	Testgrid:
	Grid = [[[a1,a2,a3],[b1,b2,b3],[c1,c2,c3]], [[d1,d2,d3],[e1,e2,e3],[f1,f2,f3]], [[f1,f2,f3],[g1,g2,g3],[h1,h2,h3]]]
*/


% union (+List1, +List2, -UnionOfList1AndList2)
% ---------------------------------------------
union([], L, L).
union([H|T1], L, T2) :-
	member(H, L), !,
	union(T1, L, T2).
union([H|T1], L, [H|T2]) :-
	union(T1, L, T2).
	
	
% selectFLR(?TTP, ?KID, ?TID, ?SID, -FLR)
% ---------------------------------------
selectFLR(Grid, ClassId, DayId, HourId, ListOfFLR) :-
	bagof(FLR, ClassId^DayId^HourId^selectSingleFLR(Grid, ClassId, DayId, HourId, FLR), ListOfFLR).
	
selectSingleFLR(Grid, ClassId, DayId, HourId, FLR) :-
	nth(ClassId, Grid, Class),
	nth(DayId, Class, Day),
	nth(HourId, Day, FLR).
	

% listOfFLR(+ListOfFLR, -ListOfF, -ListOfL, -ListOfR)
% ---------------------------------------------------
listOfFLR(ListOfFLR, ListOfF, ListOfL, ListOfR) :- 
	bagof(F1, L1^R1^member(flr(F1, L1, R1), ListOfFLR), ListOfF),
	bagof(L2, F2^R2^member(flr(F2, L2, R2), ListOfFLR), ListOfL),
	bagof(R3, F3^L3^member(flr(F3, L3, R3), ListOfFLR), ListOfR).
	
listOfF([], _).
listOfF([flr(F, _, _) | RestOfFLR], ListOfF) :-
	listOfF(RestOfFLR, RestListOfF), !,
	append(RestListOfF, [F], ListOfF).
	
listOfL([], _).
listOfL([flr(_, L, _) | RestOfFLR], ListOfL) :-
	listOfL(RestOfFLR, RestListOfL), !,
	append(RestListOfL, [L], ListOfL).
	
listOfR([], _).
listOfR([flr(_, _, R) | RestOfFLR], ListOfR) :-
	listOfR(RestOfFLR, RestListOfR), !,
	append(RestListOfR, [R], ListOfR).
	
	
% selectVars (+ListOfFLR, -ListOfVars)
% ------------------------------------
selectAllVars([], _).
selectAllVars([flr(F,L,R) | RestOfFLR], ListOfVars) :-
	selectAllVars(RestOfFLR, RestOfListOfVars), !,
	append(RestOfListOfVars, [F,L,R], ListOfVars).
	

% listOfVarPairs (+ListOfVars, -ListOfVarPairs)
%
% ListOfVarPairs represents a list of all possible pairs (without
% duplicates) that can be combined using the variables in ListOfVars
% ------------------------------------------------------------------
listOfVarPairs([], _).
listOfVarPairs([Var | RestOfVars], ListOfPairs) :-
	pairsForVar(Var, RestOfVars, ListOfPairsForVar), !,
	listOfVarPairs(RestOfVars, RestListOfPairs), !,
	append(RestListOfPairs, ListOfPairsForVar, ListOfPairs), !.


% pairsForVar (+Var, +OtherVariables, -Pairs)
% -------------------------------------------
pairsForVar(_, [], _).
pairsForVar(Var, [Partner | RestOfPartners], ListOfPairs) :-
	pairsForVar(Var, RestOfPartners, RestListOfPairs), !,
	append(RestListOfPairs, [[Var, Partner]], ListOfPairs).
	
/*
 *      Translations
 */

% translateCourse (+ListOfCourse, -ListOfCourseIds)
% translateCourse (+Course, -CourseId)
% -------------------------------------------------
translateCourse([], _).
translateCourse([Course | RestOfCourses], ListOfCourseIds) :-
	translateCourse(RestOfCourses, RestListOfCourseIds),
	courseAtoms(CourseAtoms),
	nth(CourseId, CourseAtoms, Course), !,
	append(RestListOfCourseIds, [CourseId], ListOfCourseIds).
translateCourse(Course, CourseId) :-
	courseAtoms(CourseAtoms),
	nth(CourseId, CourseAtoms, Course).


% translateTeacher (+ListOfTeacher, -ListOfTeacherIds)
% translateTeacher (+Teacher, -TeacherId)
% -------------------------------------------------
translateTeacher([], _).
translateTeacher([Teacher | RestOfTeachers], ListOfTeacherIds) :-
	translateTeacher(RestOfTeachers, RestListOfTeacherIds),
	teacherAtoms(TeacherAtoms),
	nth(TeacherId, TeacherAtoms, Teacher), !,
	append(RestListOfTeacherIds, [TeacherId], ListOfTeacherIds).
translateTeacher(Teacher, TeacherId) :-
	teacherAtoms(TeacherAtoms),
	nth(TeacherId, TeacherAtoms, Teacher).	


% translateRoom (+ListOfRoom, -ListOfRoomIds)
% translateRoom (+Room, -RoomId)
% -------------------------------------------------
translateRoom([], _).
translateRoom([Room | RestOfRooms], ListOfRoomIds) :-
	translateRoom(RestOfRooms, RestListOfRoomIds),
	roomAtoms(RoomAtoms),
	nth(RoomId, RoomAtoms, Room), !,
	append(RestListOfRoomIds, [RoomId], ListOfRoomIds).
translateRoom(Room, RoomId) :-
	roomAtoms(RoomAtoms),
	nth(RoomId, RoomAtoms, Room).


% translateClass (+ListOfClass, -ListOfClassIds)
% translateClass (+Class, -ClassId)
% -------------------------------------------------
translateClass([], _).
translateClass([Class | RestOfClasss], ListOfClassIds) :-
	translateClass(RestOfClasss, RestListOfClassIds),
	classAtoms(ClassAtoms),
	nth(ClassId, ClassAtoms, Class), !,
	append(RestListOfClassIds, [ClassId], ListOfClassIds).
translateClass(Class, ClassId) :-
	classAtoms(ClassAtoms),
	nth(ClassId, ClassAtoms, Class).