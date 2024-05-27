:- module(init, [ init/3 ]).

/**
 * init(-RowsClues, -ColsClues, Grid).
 * Predicate specifying the initial grid, which will be shown at the beginning of the game,
 * including the rows and columns clues.
 */

 init(
    [[3], [1,2], [4], [9], [9], [8],[6]],	% RowsClues
    [[2], [6], [1,4], [7], [6], [4], [4], [4], [4]], 	% ColsClues

    [[ _ , _ , _ , _ , _ , _ , _ , _ , _ ],
     [ _ , _ , _ , _ , _ , _ , _ , _ , _ ],
     [ _ , _ , _ , _ , _ , _ , _ , _ , _ ],		% Grid
     [ _ , _ , _ , _ , _ , _ , _ , _ , _ ],
     [ _ , _ , _ , _ , _ , _ , _ , _ , _ ],
     [ _ , _ , _ , _ , _ , _ , _ , _ , _ ],
     [ _ , _ , _ , _ , _ , _ , _ , _ , _ ]
    ]
).