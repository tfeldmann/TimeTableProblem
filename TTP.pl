/*
	TTP Problem - Stundenplanproblem
	
	©2011, Thomas Feldmann
*/

:- include('Parser.pl').
:- include('CommonFunctions.pl').
:- include('GridGeneration.pl').
:- include('Constraints.pl').
:- include('WriteHTML.pl').
:- include('ScheduleEntries.pl').


% ttp(+TTP, -Stundenplan)
% -----------------------
ttp(TTP, Stundenplan) :-

	% Parse and apply constraints
	parseTTP(TTP, DayAtoms, HourAtoms, ClassAtoms, CourseAtoms, TeacherAtoms, RoomAtoms),
	
	nl,
	write('Tage:\t\t'), 	write(DayAtoms), nl,
	write('Stunden:\t'), 	write(HourAtoms), nl,
	write('Klassen:\t'), 	write(ClassAtoms), nl,
	write('Lehrer:\t\t'),	write(TeacherAtoms), nl,
	write('Räume:\t\t'),	write(RoomAtoms), nl,
	nl,
	write('- Deklaration wurde eingelesen.'), nl,
	
	% Generate the Gritd (filled with anonymous variables)
	generateGridWithDomains(DayAtoms, HourAtoms, ClassAtoms, CourseAtoms, TeacherAtoms, RoomAtoms, Grid),
	write('- Grid wurde generiert.'), nl,
	
	% Constrain the grid to meet the expectations of the TTP
	constrainGrid(Grid),
	write('- Stundenplan wurde eingeschränkt.'),
	
	% Generate a solution
	selectFLR(Grid, _, _, _, AllFLR),
	selectAllVars(AllFLR, AllVars),
	fd_labeling(AllVars, [variable_method(ff), backtracks(B)]),
	
	nl,nl,
	write(' - Anzahl der Backtracks: '), write(B),
	nl,nl,
	
	writeHTML(Grid),
	write(' - Stundenplan wurde erstellt.'),
	
	nl, nl,
	write(Grid),

	scheduleEntries(Grid, ClassAtoms, DayAtoms, HourAtoms, CourseAtoms, TeacherAtoms, RoomAtoms, Stundenplan).


% test (-Stundenplan)
% -------------------
test(X) :-
	ttp([
		tage(['Montag', 'Dienstag', 'Mittwoch']),
		stunden(['von 8 bis 9', 'von 9 bis 10', 'von 10 bis 11']),

		fachraum('Mathe', ['Raum 1', 'Raum 2', 'Raum 3', 'Raum 4', 'Raum 5', 'Sporthalle', 'PC-Pool']),
		fachraum('Informatik', ['PC-Pool', 'Raum 2']),
		fachraum('Musik', ['Musiksaal']),
		fachraum('Deutsch', ['Raum 1', 'Raum 2', 'Raum 3']),
		fachraum('Sport', ['Sporthalle']),

		fachlehrer('Informatik', ['Just', 'Meyer']),
		fachlehrer('Mathe', ['Wollhöver', 'Petrias']),
		fachlehrer('Musik', ['Tewes', 'Paffrath']),
		fachlehrer('Deutsch', ['Lanze']),
		fachlehrer('Sport', ['Mersch-Hebing']),

		curriculum('Klasse 1', ['Mathe', 2, 'Informatik', 1, 'Musik', 2, 'Deutsch', 1, 'Sport', 1]),
		curriculum('Klasse 2', ['Mathe', 2, 'Informatik', 1, 'Musik', 1, 'Deutsch', 1]),
		
		raumsperre('Musiksaal', ['Dienstag', 'von 8 bis 9']),
		raumsperre('Raum 2', ['Dienstag', 'von 9 bis 10']),
		
		lehrersperre('Tewes', ['Montag', 'von 8 bis 9']),
		
		doppelstunden(['Sport', 'Informatik'])
		], X).