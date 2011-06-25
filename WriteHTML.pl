/*
	writeHTML.pl
	
	creates the file "Stundenplan.html" in the program directory which contains a
	visual representation of the solution to the given ttp problem.
	The file "timetable.css" is required to be in the same directory.
 */


% writeHTML (+Grid)
% -----------------
writeHTML(Grid) :-

	% Document head
	% -------------
	tell('Stundenplan.html'),
	write('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">\n'),
	write('<html>\n<head>\n\t<title>Stundenplan</title>\n'),
	write('\t<meta http-equiv="content-type" content="text/html; charset=UTF-8">\n'),
	write('\t<link rel="stylesheet" href="timetable.css" type="text/css">\n'),
	write('</head>\n<body>'),
	
	% Timetables
	% ----------
	forall((classAtoms(ClassAtoms),member(Class, ClassAtoms)), writeTable(Grid, Class)),
	
	% End of document
	% ---------------
	write('\n\n</body>\n</html>'),
	told.
	
	
% writeTable (+Grid, +Class)
% -------------------
writeTable(Grid, Class) :-

	write('\n<table border="1">'),
	
	% Day captions
	% ------------
	write('\n<tr>'),
	write('\n\t<th><span class="class">'),
	write(Class),
	write('</span></th>'),
	forall((dayAtoms(DayAtoms), member(Day, DayAtoms)), (write('\n\t<th>'), write(Day), write('</th>'))), !,
	write('\n</tr>'),
	
	% Entries
	% -------
	forall((hourAtoms(HourAtoms), member(Hour, HourAtoms)), writeRow(Grid, Class, Hour)),
	
	write('\n</table>').
	

% writeRow(+Grid, +Class, +Row)
% ----------------------
writeRow(Grid, Class, Hour) :-
	classAtoms(ClassAtoms),
	hourAtoms(HourAtoms),
	nth(ClassId, ClassAtoms, Class),
	nth(HourId, HourAtoms, Hour),

	write('\n<tr>'),
	write('\n\t<td>'), write(Hour), write('</td>'),
	forall(selectSingleFLR(Grid, ClassId, _, HourId, FLR), writeCell(FLR)),
	write('\n</tr>').


% writeCell (+FLR)
% ----------------
writeCell(flr(0,0,0)) :-
	write('\n\t<td></td>').
	
writeCell(flr(F, L, R)) :-
	courseAtoms(CourseAtoms),
	teacherAtoms(TeacherAtoms),
	roomAtoms(RoomAtoms),
	nth(F, CourseAtoms, Course),
	nth(L, TeacherAtoms, Teacher),
	nth(R, RoomAtoms, Room),
	
	write('\n\t<td>'),
	write('<span class="Course">'), write(Course), write('</span><br />'),
	write('<span class="Teacher">'), write(Teacher), write('</span><br />'),
	write('<span class="Room">'), write(Room), write('</span><br />'),
	write('</td>').

		
/*
// UNCOMMENT FOR TEST ENVIRONMENT

roomAtoms(['Musiksaal', 'Raum 1', 'Raum 2', 'Raum 3', 'Raum 4', 'Raum 5', 'Sporthalle', 'PC-Pool']).
teacherAtoms(['Sabine Müller', 'Günther Tewes', 'Michael Paffrath', 'Prof. Dr. Klaus Wollhöver', 'Horst Petrias',
				'Prof. Dr. Manfred Meyer', 'Prof. Dr. Olaf Just']).
dayAtoms(['Montag', 'Dienstag', 'Mittwoch', 'Donnerstag', 'Freitag']).
classAtoms(['Klasse 3', 'Klasse 2', 'Klasse 1']).
hourAtoms(['von 8 bis 9', 'von 9 bis 10', 'von 10 bis 11', 'von 11 bis 12']).
courseAtoms(['Deutsch', 'Musik', 'Mathe', 'Informatik']).

:- include('CommonFunctions.pl').

test :-
	Grid = [[[flr(4,1,1),flr(3,1,1),flr(2,1,1),flr(1,2,1)],[flr(1,1,1),flr(1,4,1),flr(1,1,1),flr(1,5,5)],
	[flr(0,0,0),flr(0,0,0),flr(0,0,0),flr(0,0,0)],[flr(0,0,0),flr(0,0,0),flr(0,0,0),flr(0,0,0)],
	[flr(0,0,0),flr(0,0,0),flr(0,0,0),flr(0,0,0)]],[[flr(4,1,1),flr(3,1,1),flr(3,1,1),flr(2,1,1)],
	[flr(2,1,1),flr(2,1,1),flr(2,1,1),flr(2,1,1)],[flr(1,1,1),flr(0,0,0),flr(0,0,0),flr(0,0,0)],
	[flr(0,0,0),flr(0,0,0),flr(0,0,0),flr(0,0,0)],[flr(0,0,0),flr(0,0,0),flr(0,0,0),flr(0,0,0)]],
	[[flr(4,1,1),flr(4,1,1),flr(4,1,1),flr(4,1,1)],[flr(4,1,1),flr(3,1,1),flr(3,1,1),flr(2,1,1)],
	[flr(2,1,1),flr(1,1,1),flr(1,1,1),flr(1,1,1)],[flr(0,0,0),flr(0,0,0),flr(0,0,0),flr(0,0,0)],
	[flr(0,0,0),flr(0,0,0),flr(0,0,0),flr(0,0,0)]]],
	writeHTML(Grid),
	write(Grid).
*/