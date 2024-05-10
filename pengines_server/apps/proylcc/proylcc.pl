:- module(proylcc,
	[  
		put/9
	]).

:-use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%generates a unique and correct solution

solvedBoard(Grid,CluesRow,CluesCol,Sol):-
	copy_term(Grid,NewGrid),
	generateSolution(NewGrid,CluesRow,CluesCol,Sol),
	initialCheckCol(Sol,CluesCol,Valid),
    checkValid(Valid,Out),
	Out = true,
	!.

%creates a solution that is correct with the clues of the rows
generateSolution([],[],_CluesCol,[]).
generateSolution([H|Grid],[Hc|CluesRow],CluesCol,[Out|Sol]):-
    generateList(Hc,H,Out),
	generateSolution(Grid,CluesRow,CluesCol,Sol).
	
%checks if a list is an array of true
checkValid([true],true):-
	!.
checkValid([H|T],Out):-
	H = true,
	checkValid(T,Out).

%generates a possible solution for a list and a clue
generateList([],[],[]).
generateList([],[H|Row],["X"|Out]):-
	H = "X",
	generateList([],Row,Out).
generateList([H|Clues],[X|Row],Out):-
	X = "#",
	generateListActive([H|Clues],[X|Row],Out).
generateList([H|Clues],[X|Row],["X"|Out]):-
	X = "X",
	generateList([H|Clues],Row,Out).


generateListActive([0],[],[]).
generateListActive([0|Clues],[X|Row],["X"|Out]):-
	X = "X",
	generateList(Clues,Row,Out).
generateListActive([H|Clues],[X|Row],["#"|Out]):-
	X = "#",
	H > 0,
	Hs is H-1,
	generateListActive([Hs|Clues],Row,Out).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%checks if the game has been completed

isAWin(Grid,ColsClues,RowClues,Win):-
	initialCheckCol(Grid,ColsClues,SatCols),
	initialCheckList(Grid,RowClues,SatRows),
	isAWinCheck(SatRows,SatCols,Win).

isAWinCheck([],[],true):-
	!.
isAWinCheck([true|Rows],[true|Cols],Out):-
    isAWinCheck(Rows,Cols,Out),
	!.
isAWinCheck([_HR|_Rows],[_HC|_Cols],false):-
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% transposeGrid(?X, +XIndex, +Y, +Xs, -XsY)
%
% XsY is the result of replacing the occurrence of X in position XIndex of Xs by Y.
gridLength(Cont,[],Cont).
gridLength(Cont,[_H|Grid],Out):-
	ContX is Cont+1,
	gridLength(ContX,Grid,Out).

transposeGrid(ColPivot,ColPivot,_Grid,[]).
transposeGrid(Col,ColPivot,Grid,[Ht|TGrid]):-
    colToList(Col,Grid,Ht),
    ColX is Col+1,
    transposeGrid(ColX,ColPivot,Grid,TGrid).
	

initialCheckCol(Grid,Clues,SatClues):-
	gridLength(0,Grid,Len),
	transposeGrid(0,Len,Grid,TGrid),
	initialCheckList(TGrid,Clues,SatClues).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% initialCheckList(?X, +XIndex, +Y, +Xs, -XsY)
%
% XsY is the result of replacing the occurrence of X in position XIndex of Xs by Y.
initialCheckList([],[],[]).
initialCheckList([H|Grid],[Hc|Clues],[Out|SatClues]):-
	searchClue(Hc,H,Out),
	initialCheckList(Grid,Clues,SatClues).



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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% rowToList(+RowN, +Row, -Out).
%
% Given a grid and a position returns the list in the position RowN of the grid
rowToList(0,[H|_Row],H).
rowToList(RowN,[_H|Row],Out):-
	RowNX is RowN-1,
    rowToList(RowNX,Row,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% colToList(+ColN, +Grid, -ColList).
%
% colToListSearch(+ColN,+Row,-Out).
%
% This methods create a list of the elements of a column in a grid
colToList(_ColN,[],[]).
colToList(ColN,[Row|Grid],[Out|ColList]):-
	colToListSearch(ColN,Row,Out),
	colToList(ColN,Grid,ColList).
	
	
colToListSearch(0,[H|_Row],H).
colToListSearch(ColN,[_H|Row],Out):-
	ColNX is ColN-1,
	colToListSearch(ColNX,Row,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% checkList(+RowN, +RowsClues, +NewRow,-Out).
%
%Given a list, the clues and  a position finds the clue of that list and checks if it is correct
%
checkList(RowN, RowsClues, NewRow,Out):-
	findClue(RowsClues,RowN,Ret),
	searchClue(Ret,NewRow,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% findClue(+RowClue, +Index,-Retorno).
%
% This method finds a clue corresponding to a position
findClue([X|_RowClue],0,X).
findClue([_H|RowClue],Index,Retorno):-
	Index > 0,
    IndexS is Index - 1,
	findClue(RowClue,IndexS,Retorno).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%documentar
%
% searchClue(+Clues, +Row,-Out).
%	
% This method searchs in a list for a painted square and checks if the painted sqquares are in order
%
searchClue([],[],true).
searchClue([0],[],true).
searchClue([_H],[],false).
searchClue([_H|_Clues],[],false).
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
% searchActiveClue(+Clues, +Row, -Out).
%
% The method searchs for continous painted squares in a list and checks for the clues
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% checkRows(+RowN, +RowClues, +Grid, -RowSat).
%
% checkCols(+ColN, +ColClues, +Grid, -ColSat).
%
% This methods recieve a position, clues and a grid and return the satisfaction of the clues
% Each method differs in the components given. One acts over a column and the other over a row
%

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

put(Content, [RowN, ColN], RowsClues, ColsClues, Grid, NewGrid, RowSat, ColSat,Win):-
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
	% Copy_term creates a copy of the new grid to operate over
	% Then it check on the grid the clues
	copy_term(NewGrid,Aux4Col),
	copy_term(NewGrid,Aux4Row),
	copy_term(NewGrid,AuxTotal),
    checkRows(RowN,RowsClues,Aux4Row,RowSat),
    checkCols(ColN,ColsClues,Aux4Col,ColSat),
	isAWin(AuxTotal,ColsClues,RowsClues,Win).