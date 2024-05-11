import React, { useEffect, useState } from 'react';
import Board from './Board';
import PengineClient from './PengineClient';

let pengine;

function Game() {

  // State
  const [grid, setGrid] = useState(null);
  const [solvedGrid, setSolvedGrid] = useState(null);
  const [rowsClues, setRowsClues] = useState(null);
  const [colsClues, setColsClues] = useState(null);
  const [waiting, setWaiting] = useState(false);
  const [content, setContent] = useState('#');
  const [cluesFilas, setCluesFilas] = useState([]);
  const [cluesColumnas, setCluesColumnas] = useState([]);
  const [toggled, setToggled] = useState(false);
  const [status, setStatus] = useState('');
  const [idea, setIdea] = useState(false);
  const [complete, setComplete] = useState(false);

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
        handleStart();
        console.log(grid);
      }
    });
  }

  function handleStart() {
    const squaresRow = JSON.stringify(grid).replaceAll('"_"', '_');
    const squaresCol = JSON.stringify(grid).replaceAll('"_"', '_');
    const squares = JSON.stringify(grid).replaceAll('"_"', '_');
    const rowsCluesS = JSON.stringify(rowsClues);
    const colsCluesS = JSON.stringify(colsClues);
    const queryInitialcheckRow = `initialCheckList(${squaresRow}, ${rowsCluesS}, RowSatList)`;
    const queryInitialcheckCol = `initialCheckCol(${squaresCol}, ${colsCluesS}, ColSatList)`;

    pengine.query(queryInitialcheckRow, (success, response) => {
      if (success) {
        for (let index = 0; index < response['RowSatList'].length; index++) {
          if (response['RowSatList'][index])
            setCluesFilas([...cluesFilas, index]);
        }
      }
    });
    pengine.query(queryInitialcheckCol, (success, response) => {
      if (success) {
        for (let index = 0; index < response['ColSatList'].length; index++) {
          if (response['ColSatList'][index])
            setCluesColumnas([...cluesColumnas, index]);
        }
      }
    });

    const queryInitialcheckBoard = `solvedBoard(${squares}, ${rowsCluesS}, ${colsCluesS}, gridComplete)`;
    pengine.query(queryInitialcheckBoard, (success, response) => {
      if (success) {
        setSolvedGrid(response['gridComplete']);
      }
      console.log(solvedGrid);
    });
  }

  function handleClick(i, j) {
    // No action on click if we are waiting.
    if (waiting) {
      return;
    }
    // Build Prolog query to make a move and get the new satisfacion status of the relevant clues.    
    const squaresS = JSON.stringify(grid).replaceAll('"_"', '_'); // Remove quotes for variables. squares = [["X",_,_,_,_],["X",_,"X",_,_],["X",_,_,_,_],["#","#","#",_,_],[_,_,"#","#","#"]]
    const rowsCluesS = JSON.stringify(rowsClues);
    const colsCluesS = JSON.stringify(colsClues);
    const queryS = `put("${content}", [${i},${j}], ${rowsCluesS}, ${colsCluesS}, ${squaresS}, ResGrid, RowSat, ColSat, Win)`; // queryS = put("#",[0,1],[], [],[["X",_,_,_,_],["X",_,"X",_,_],["X",_,_,_,_],["#","#","#",_,_],[_,_,"#","#","#"]], GrillaRes, FilaSat, ColSat)
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['ResGrid']);
        if (response['RowSat']) {
          setCluesFilas([...cluesFilas, i])
        }
        else {
          setCluesFilas(cluesFilas.filter(e => e !== i))
        }
        if (response['ColSat']) {
          setCluesColumnas([...cluesColumnas, j])
        }
        else {
          setCluesColumnas(cluesColumnas.filter(e => e !== j))
        }
        if (response['Win']) {
          setStatus('YOU WIN! CONGRATS');

        }
        else {
          setStatus('');
        }
      }
      setWaiting(false);
    });
  }

  function completeOnClick(){
    setComplete(!complete);
  }

  useEffect(() => {
    setContent(toggled ? 'X' : '#');
  }, [toggled]);

  if (!grid) {
    return null;
  }

  const titleText = 'NONOGRAM';
  return (
    <div className="game">
      <div className="game-text">
        {titleText}
      </div>
      <div className="game-text">{status}</div>
      <div className="content">
        <Board
          grid={grid}
          rowsClues={rowsClues}
          colsClues={colsClues}
          cluesFilas={cluesFilas}
          cluesColumnas={cluesColumnas}
          onClick={(i, j) => handleClick(i, j)}
          onLoad={() => handleStart()}
        />
      </div>
      <div className="TButton">
        <button className={`toggle-btnCOMPLETE`} onClick={() => completeOnClick()}>
        </button>
        <div style={{display: "flex",alignItems: "center"}}>
          <div className='CrossSquare'>
            <b style={{ fontSize: 28 }}>X</b>
          </div>
          <button className={`toggle-btn`} onClick={() => setToggled(!toggled)}>
            <div className={`${toggled ? 'circleRight' : 'circleLeft'}`}></div>
          </button>
          <div className='paint-mode'>
          </div>
        </div>
        <button className={`toggle-btnIDEA`} onClick={() => setIdea(!idea)}>
        </button>
      </div>
    </div>
  );
}

export default Game;