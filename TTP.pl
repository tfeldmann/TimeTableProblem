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
	write('- Deklaration wird eingelesen.'), nl,
	parseTTP(TTP, DayAtoms, HourAtoms, ClassAtoms, CourseAtoms, TeacherAtoms, RoomAtoms),	

	nl,
	write('Tage:\t\t'), 	write(DayAtoms), nl,
	write('Stunden:\t'), 	write(HourAtoms), nl,
	write('Klassen:\t'), 	write(ClassAtoms), nl,
	write('Lehrer:\t\t'),	write(TeacherAtoms), nl,
	write('Räume:\t\t'),	write(RoomAtoms), nl,
	nl,

	% Generate the Gritd (filled with anonymous variables)
	write('- Grid wird generiert.'), nl,
	generateGridWithDomains(DayAtoms, HourAtoms, ClassAtoms, CourseAtoms, TeacherAtoms, RoomAtoms, Grid),
	
	% Constrain the grid to meet the expectations of the TTP
	write('- Stundenplan wird eingeschränkt.'), nl,
	constrainGrid(Grid),
	
	% Generate a solution
	write(' - Problem wird gelöst'),
	selectFLR(Grid, _, _, _, AllFLR),
	selectAllVars(AllFLR, AllVars), !,
	
	fd_labeling(AllVars, [variable_method(ff), backtracks(Backtracks)]),
	
	nl,nl,
	write('Anzahl der Backtracks: '), write(Backtracks),
	nl,nl,
	
	% Stundenplan.html erstellen.
	write(' - Stundenplan.html wird erstellt.'),
	writeHTML(Grid),
	
	nl, nl,
	write(Grid),

	scheduleEntries(Grid, ClassAtoms, DayAtoms, HourAtoms, CourseAtoms, TeacherAtoms, RoomAtoms, Stundenplan).


% Musterproblem
% -------------
meyer(X) :-
	ttp(
		[
			tage(['Montag', 'Dienstag', 'Mittwoch','Donnerstag', 'Freitag']),
			stunden(['von 8 bis 9', 'von 9 bis 10', 'von 10 bis 11', 'von 11 bis 12', 'von 12 bis 13']),
		
			curriculum('Klasse 1',['Deutsch',5,'Mathe',5,'Sachkunde',2,'Sport',2,'Schwimmen',2,'Musik',2,'Kunst',2,'Religion',2,'Englisch',1,'Informatik',2]),
			curriculum('Klasse 2',['Deutsch',6,'Mathe',5,'Sachkunde',2,'Sport',2,'Schwimmen',2,'Musik',1,'Kunst',2,'Religion',2,'Englisch',2,'Informatik',2]),
			curriculum('Klasse 3',['Deutsch',5,'Mathe',5,'Sachkunde',4,'Sport',2,'Schwimmen',2,'Musik',1,'Kunst',2,'Religion',2,'Englisch',2,'Informatik',4]),
		
			fachraum('Kunst',		['Kunstraum']),
			fachraum('Musik',		['Musikraum']),
			fachraum('Sport',		['Sporthalle']),
			fachraum('Schwimmen',	['Schwimmhalle']),
			fachraum('Deutsch',		['Raum 1','Raum 2','Raum 3']),
			fachraum('Englisch',	['Raum 1','Raum 2','Raum 3']),
			fachraum('Mathe',		['Raum 1','Raum 2','Raum 3']),
			fachraum('Religion',	['Raum 1','Raum 2','Raum 3']),
			fachraum('Sachkunde',	['Labor']),
			fachraum('Informatik',	['PC-Pool']),
		
			fachlehrer('Kunst',		['Schulze']),
			fachlehrer('Musik',		['Schulze']),
			fachlehrer('Deutsch',	['Müller']),
			fachlehrer('Religion',	['Müller']),
			fachlehrer('Sport',		['Schmidt']),
			fachlehrer('Schwimmen',	['Schmidt']),
			fachlehrer('Englisch',	['Meyer']),
			fachlehrer('Informatik',['Meyer']),
			fachlehrer('Mathe',		['Schneider']),
			fachlehrer('Sachkunde',	['Schneider']),
		
			doppelstunden(['Sport','Schwimmen','Kunst']),
		
			% Herr Müller kann nur 'Montag's bis 'Donnerstag's
			lehrersperre('Müller', 		['Freitag', 'von 8 bis 9', 'Freitag', 'von 9 bis 10', 'Freitag', 'von 10 bis 11', 'Freitag', 'von 11 bis 12', 'Freitag', 'von 12 bis 13', 'Freitag', 'von 13 bis 14']),
		
			% die Schwimmhalle steht nur 'Dienstag's zur Verfügung
			raumsperre('Schwimmhalle',	['Montag', 'von 8 bis 9', 'Montag', 'von 9 bis 10', 'Montag', 'von 10 bis 11', 'Montag', 'von 11 bis 12', 'Montag', 'von 12 bis 13', 'Montag', 'von 13 bis 14',
										'Mittwoch', 'von 8 bis 9', 'Mittwoch', 'von 9 bis 10', 'Mittwoch', 'von 10 bis 11', 'Mittwoch', 'von 11 bis 12', 'Mittwoch', 'von 12 bis 13', 'Mittwoch', 'von 13 bis 14',
										'Donnerstag', 'von 8 bis 9', 'Donnerstag', 'von 9 bis 10', 'Donnerstag', 'von 10 bis 11', 'Donnerstag', 'von 11 bis 12', 'Donnerstag', 'von 12 bis 13', 'Donnerstag', 'von 13 bis 14',
										'Freitag', 'von 8 bis 9', 'Freitag', 'von 9 bis 10', 'Freitag', 'von 10 bis 11', 'Freitag', 'von 11 bis 12', 'Freitag', 'von 12 bis 13', 'Freitag', 'von 13 bis 14']),
									
			% Herr Schmidt kann nur 'Montag's und 'Dienstag's		
			lehrersperre('Schmidt',		['Mittwoch', 'von 8 bis 9', 'Mittwoch', 'von 9 bis 10', 'Mittwoch', 'von 10 bis 11', 'Mittwoch', 'von 11 bis 12', 'Mittwoch', 'von 12 bis 13', 'Mittwoch', 'von 13 bis 14',
										'Donnerstag', 'von 8 bis 9', 'Donnerstag', 'von 9 bis 10', 'Donnerstag', 'von 10 bis 11', 'Donnerstag', 'von 11 bis 12', 'Donnerstag', 'von 12 bis 13', 'Donnerstag', 'von 13 bis 14',
									    'Freitag', 'von 8 bis 9', 'Freitag', 'von 9 bis 10', 'Freitag', 'von 10 bis 11', 'Freitag', 'von 11 bis 12', 'Freitag', 'von 12 bis 13', 'Freitag', 'von 13 bis 14']),
								
			% Frau Schulze kann nur 'Donnerstag's und 'Freitag's
			lehrersperre('Schulze',		['Montag', 'von 8 bis 9', 'Montag', 'von 9 bis 10', 'Montag', 'von 10 bis 11', 'Montag', 'von 11 bis 12', 'Montag', 'von 12 bis 13', 'Montag', 'von 13 bis 14',
										'Dienstag', 'von 8 bis 9', 'Dienstag', 'von 9 bis 10', 'Dienstag', 'von 10 bis 11', 'Dienstag', 'von 11 bis 12', 'Dienstag', 'von 12 bis 13', 'Dienstag', 'von 13 bis 14',
										'Mittwoch', 'von 8 bis 9', 'Mittwoch', 'von 9 bis 10', 'Mittwoch', 'von 10 bis 11', 'Mittwoch', 'von 11 bis 12', 'Mittwoch', 'von 12 bis 13', 'Mittwoch', 'von 13 bis 14']),
									
			% Herr Meyer kann nur 'Mittwoch's bis 'Freitag's
			lehrersperre('Meyer',		['Montag', 'von 8 bis 9', 'Montag', 'von 9 bis 10', 'Montag', 'von 10 bis 11', 'Montag', 'von 11 bis 12', 'Montag', 'von 12 bis 13', 'Montag', 'von 13 bis 14',
										'Dienstag', 'von 8 bis 9', 'Dienstag', 'von 9 bis 10', 'Dienstag', 'von 10 bis 11', 'Dienstag', 'von 11 bis 12', 'Dienstag', 'von 12 bis 13', 'Dienstag', 'von 13 bis 14'])
		], X).

% test (-Stundenplan)
% -------------------
test(X) :-
	ttp([
		tage(['Montag', 'Dienstag', 'Mittwoch']),
		stunden(['von 8 bis 9', 'von 9 bis 10', 'von 10 bis 11', 'von 11 bis 12']),

		fachraum('Mathe', ['Raum 1', 'Raum 2', 'Raum 3', 'Sporthalle', 'PC-Pool']),
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
		
		lehrersperre('Tewes', ['Dienstag', 'von 8 bis 9'])
		
		% TODO: Doppelstunden implementieren
%		doppelstunden(['Sport', 'Informatik'])
		], X).