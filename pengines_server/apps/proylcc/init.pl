:- module(init, [ init/3 ]).

/**
 * init(-RowsClues, -ColsClues, Grid).
 * Predicate specifying the initial grid, which will be shown at the beginning of the game,
 * including the rows and columns clues.
 */

 init(
    [[3], [1,2], [4], [5], [5]],	% RowsClues
    [[2], [5], [1,3], [5], [4]], 	% ColsClues

    [["X", _ , _ , _ , _ ], 		
     ["X", _ ,"X", _ , _ ],
     ["X", _ , _ , _ , _ ],		% Grid
     ["#","#","#", _ , _ ],
     [ _ , _ ,"#","#","#"]
    ]
%
%transposeGrid(ColPivot,ColPivot,_Grid,[]).
%transposeGrid(Col,ColPivot,Grid,[Ht|TGrid]):-
%	colToList(Col,Grid,Ht),
%	ColX is Col+1,
%	transposeGrid(ColX,ColPivot,Grid,TGrid).
%
%initialCheckList([],[],[]).
%initialCheckList([H|Grid],[Hc|Clues],[Out|SatClues]):-
%	searchClue(Hc,H,Out),
%	initialCheckList(Grid,Clues,SatClues).

).