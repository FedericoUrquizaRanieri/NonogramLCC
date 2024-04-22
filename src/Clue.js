import React from 'react';

function Clue({ clue , satisfied}) {
    return (
        <div className={`clue ${satisfied? "satisfied": ""}`} >
            {clue.map((num, i) =>
                <div key={i}>
                    {num}
                </div>
            )}
        </div>
    );
}



export default Clue;