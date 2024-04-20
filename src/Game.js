import React, { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import Square from './Square';

let pengine;

function Game() {

  // State
  const [grid, setGrid] = useState(null);
  const [rowsClues, setRowsClues] = useState(null);
  const [colsClues, setColsClues] = useState(null);
  const [waiting, setWaiting] = useState(false);
  const [toggled,setToggled]= useState(false);

  useEffect(() => {
    // Creation of the pengine server instance.    
    // This is executed just once, after the first render.    
    // The callback will run when the server is ready, and it stores the pengine instance in the pengine variable. 
    PengineClient.init(handleServerReady);
  }, []);

  function handleServerReady(instance) {
    pengine = instance;
    const queryS = 'init(RowClues, ColumClues, Grid)';
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['Grid']);
        setRowsClues(response['RowClues']);
        setColsClues(response['ColumClues']);
      }
    });
  }

  function handleClick(i, j) {
    // No action on click if we are waiting.
    if (waiting) {
      return;
    }
    // Build Prolog query to make a move and get the new satisfacion status of the relevant clues.    
    const squaresS = JSON.stringify(grid).replaceAll('"_"', '_'); // Remove quotes for variables. squares = [["X",_,_,_,_],["X",_,"X",_,_],["X",_,_,_,_],["#","#","#",_,_],[_,_,"#","#","#"]]
    const content = '#'; // Content to put in the clicked square.
    const rowsCluesS = JSON.stringify(rowsClues);
    const colsCluesS = JSON.stringify(colsClues);
    const queryS = `put("${content}", [${i},${j}], ${rowsCluesS}, ${colsCluesS}, ${squaresS}, ResGrid, RowSat, ColSat)`; // queryS = put("#",[0,1],[], [],[["X",_,_,_,_],["X",_,"X",_,_],["X",_,_,_,_],["#","#","#",_,_],[_,_,"#","#","#"]], GrillaRes, FilaSat, ColSat)
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['ResGrid']);
        console.log(response['RowSat']);
        console.log(response['ColSat']);
      }
      setWaiting(false);
    });
  }

  if (!grid) {
    return null;
  }

  const titleText = 'NONOGRAM';
  return (
    <div className="game">
      <div className="game-text">
          {titleText}
      </div>
      <div className="content">
        <Board
          grid={grid}
          rowsClues={rowsClues}
          colsClues={colsClues}
          onClick={(i, j) => handleClick(i, j)}
        />
      </div>
        <div className="TButton">
        <div className='CrossSquare'>
            <Square
              value="X"
            />
          </div>
          <button className={`toggle-btn`} onClick={()=> setToggled(!toggled)}>
            <div className={`${toggled ? 'circleRight' : 'circleLeft' }`}></div>
          </button>
          <div className='PaintedSquare'>
            <Square
              value="#"
            />
          </div>
        </div>
    </div>
  );
}

export default Game;