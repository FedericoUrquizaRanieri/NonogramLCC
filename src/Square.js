import React, { useState } from 'react';

function Square({ value, onClick }) {
    let color;
  
  // Define el color de fondo basado en el valor
  if (value === '#') {
    color = '#344862'; // Cambia el color a tu preferencia
  } else {
    color = 'white'; // Otro color de fondo para otros valores
  }
    return (
        <button className="square" onClick={onClick} style={{background:color, boxShadow: '0 2px 4px rgba(0, 0, 0, 0.1)', transition: 'background-color 0.3s'}}>
            {value !== '_' ? value : null}
        </button>
    );
}

export default Square;