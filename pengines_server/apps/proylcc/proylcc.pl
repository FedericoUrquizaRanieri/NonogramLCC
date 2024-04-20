:- module(proylcc,
	[  
		put/8
	]).

:-use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% replace(?X, +XIndex, +Y, +Xs, -XsY)
%
% XsY is the result of replacing the occurrence of X in position XIndex of Xs by Y.

replace(X, 0, Y, [X|Xs], [Y|Xs]).

replace(X, XIndex, Y, [Xi|Xs], [Xi|XsY]):-
    XIndex > 0,
    XIndexS is XIndex - 1,
    replace(X, XIndexS, Y, Xs, XsY).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%documentar
%
% put(+Content, +Pos, +RowsClues, +ColsClues, +Grid, -NewGrid, -RowSat, -ColSat).
%
rowToList(0,[H|_Row],H).
rowToList(RowN,[_H|Row],Out):-
	RowNX is RowN-1,
    rowToList(RowNX,Row,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%documentar
%
% put(+Content, +Pos, +RowsClues, +ColsClues, +Grid, -NewGrid, -RowSat, -ColSat).
%
colToList(_ColN,[],[]).
colToList(ColN,[Row|Grid],[Out|ColList]):-
	colToListSearch(ColN,Row,Out),
	colToList(ColN,Grid,ColList).
	
	
colToListSearch(0,[H|_Row],H).
colToListSearch(ColN,[_H|Row],Out):-
	ColNX is ColN-1,
	colToListSearch(ColNX,Row,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%documentar
%
% put(+Content, +Pos, +RowsClues, +ColsClues, +Grid, -NewGrid, -RowSat, -ColSat).
%
checkList(RowN, RowsClues, NewRow,Out):-
	findClue(RowsClues,RowN,Ret),
	searchClue(Ret,NewRow,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%documentar
%
% put(+Content, +Pos, +RowsClues, +ColsClues, +Grid, -NewGrid, -RowSat, -ColSat).
%
findClue([X|_RowClue],0,X).
findClue([_H|RowClue],Index,Retorno):-
	Index > 0,
    IndexS is Index - 1,
	findClue(RowClue,IndexS,Retorno).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%documentar
%
% put(+Content, +Pos, +RowsClues, +ColsClues, +Grid, -NewGrid, -RowSat, -ColSat).
%	
searchClue([],[],true).
searchClue([],[H|Row],Out):-
	H = "X",
	searchClue([],Row,Out).
searchClue([],[H|_Row],false):-
	H = "#".
searchClue([],_Row,_Out):-
    !.
searchClue([H|Clues],[X|Row],Out):-
	X = "X",
	searchClue([H|Clues],Row,Out),
	!.
searchClue([H|Clues],[X|Row],Out):-
	X = "#",
	searchActiveClue([H|Clues],[X|Row],Out),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% put(+Content, +Pos, +RowsClues, +ColsClues, +Grid, -NewGrid, -RowSat, -ColSat).
%
searchActiveClue([0],[],true).
searchActiveClue([_H|_Clues],[],false).
searchActiveClue([0|Clues],[X|Row],Out):-
	X = "X",
	searchClue(Clues,Row,Out),
    !.
searchActiveClue([_H|_Clues],[X|_Row],false):-
	X = "X",
    !.
searchActiveClue([0|_Clues],["#"|_Row],false).
searchActiveClue([H|Clues],[X|Row],Out):-
	X = "#",
	H > 0,
	Hs is H-1,
	searchActiveClue([Hs|Clues],Row,Out),
    !.

checkRows(RowN,RowClues,Grid,RowSat):-
	rowToList(RowN,Grid,Selected),
	checkList(RowN,RowClues,Selected,RowSat).
checkCols(ColN,ColClues,Grid,ColSat):-
	colToList(ColN,Grid,Selected),
	checkList(ColN,ColClues,Selected,ColSat).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% put(+Content, +Pos, +RowsClues, +ColsClues, +Grid, -NewGrid, -RowSat, -ColSat).
%

put(Content, [RowN, ColN], RowsClues, ColsClues, Grid, NewGrid, RowSat, ColSat):-
	% NewGrid is the result of replacing the row Row in position RowN of Grid by a new row NewRow (not yet instantiated).
	replace(Row, RowN, NewRow, Grid, NewGrid),

	% NewRow is the result of replacing the cell Cell in position ColN of Row by _,
	% if Cell matches Content (Cell is instantiated in the call to replace/5).	
	% Otherwise (;)
	% NewRow is the result of replacing the cell in position ColN of Row by Content (no matter its content: _Cell).			
	(replace(Cell, ColN, _, Row, NewRow),
	Cell == Content
		;
	replace(_Cell, ColN, Content, Row, NewRow)),
	copy_term(NewGrid,Aux4Col),
	copy_term(NewGrid,Aux4Row),
    checkRows(RowN,RowsClues,Aux4Row,RowSat),
    checkCols(ColN,ColsClues,Aux4Col,ColSat).