import React, { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';

let pengine;

function Game() {

  // State
  const [grid, setGrid] = useState(null);
  const [solved, setSolved] = useState(null);
  const [rowsClues, setRowsClues] = useState(null);
  const [colsClues, setColsClues] = useState(null);
  const [waiting, setWaiting] = useState(false);
  const [content, setContent] = useState('#');
  const [cluesFilas, setCluesFilas] = useState([]);
  const [cluesColumnas, setCluesColumnas] = useState([]);
  const [toggled, setToggled] = useState(false);
  const [status, setStatus] = useState('');
  const [idea, setIdea] = useState(false);
  

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

  function handleStart() {
    const squaresRow = JSON.stringify(grid).replaceAll('"_"', '_');
    const squaresCol = JSON.stringify(grid).replaceAll('"_"', '_');
    const squaresComp = JSON.stringify(grid).replaceAll('"_"', '_');
    const rowsCluesS = JSON.stringify(rowsClues);
    const colsCluesS = JSON.stringify(colsClues);
    const queryInitialcheckRow = `initialCheckList(${squaresRow}, ${rowsCluesS}, RowSatList)`;
    const queryInitialcheckCol = `initialCheckCol(${squaresCol}, ${colsCluesS}, ColSatList)`;
    const queryCompleteBoard = `solvedBoard(${squaresComp}, ${rowsCluesS}, ${colsCluesS}, GridComplete)`;

    pengine.query(queryInitialcheckRow, (success, response) => {
      if (success) {
        const updatedCluesFilas = [...cluesFilas];
        for (let index = 0; index < response['RowSatList'].length; index++) {
          if (response['RowSatList'][index])
          updatedCluesFilas.push(index);
        }
        setCluesFilas(updatedCluesFilas);
      }   
    });
    pengine.query(queryInitialcheckCol, (success, response) => {
      if (success) {
        const updatedCluesColumnas = [...cluesColumnas];
        for (let index = 0; index < response['ColSatList'].length; index++) {
          if (response['ColSatList'][index])
          updatedCluesColumnas.push(index);
        }
        setCluesColumnas(updatedCluesColumnas);
      }   
    });

    pengine.query(queryCompleteBoard, (success, response) => {
      if(success){
        setSolved(response['GridComplete']);
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
    const rowsCluesS = JSON.stringify(rowsClues);
    const colsCluesS = JSON.stringify(colsClues);
    //creation of 2 separate querys for a normal click and one for a solution
    const querySX = `put("${solved[i][j]}", [${i},${j}], ${rowsCluesS}, ${colsCluesS}, ${squaresS}, ResGrid, RowSat, ColSat, Win)`;
    const queryS = `put("${content}", [${i},${j}], ${rowsCluesS}, ${colsCluesS}, ${squaresS}, ResGrid, RowSat, ColSat, Win)`;
    setWaiting(true);
    if(idea){
      pengine.query(querySX, (success, response) => {
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
            setWaiting(true);
          }
          else {
            setStatus('');
            setWaiting(false);
          }
        }
      });
    }
    else{
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
            setWaiting(true);
          }
          else {
            setStatus('');
            setWaiting(false);
          }
        }
      });
    }
  }

  function completeGrid(){
    if(!waiting){
      var auxGrid = grid;
      setGrid(solved);
      setWaiting(true);
      setTimeout(() => {
        setGrid(auxGrid);
        setWaiting(false);
      }, 3000);
    }
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
        <button className={`toggle-btnCOMPLETE`} onClick={() => completeGrid()}>
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
        <button className={`toggle-btnIDEA ${idea ? 'active' : ''}`} onClick={() => setIdea(!idea)}>
        </button>
      </div>
    </div>
  );
}

export default Game;