import React from 'react';
import './App.css';
import Simplification from './Simplification';
import { useSatyrosAPI } from './useSatyrosAPI';

const f1: FormulaLike<Expressible> = [[[1, 2, '::<?', 0], [3, '::<>?', 1]], [[1, 2, '::>=?', -1]]];

const App: React.FunctionComponent = () => {
  const satyrosAPI1 = useSatyrosAPI(f1);

  if (satyrosAPI1 === undefined) {
    return null;
  } else {
    const ef1 = satyrosAPI1.expressedFormula;
    const c = satyrosAPI1.conversionTable;

    return (
      <div className="App">
        <Simplification
          width={500}
          height={200}
          expressibleFormula={f1}
          expressedFormula={ef1}
          convert={false}
        />
        <header className="App-header">
          <a
            className="App-link"
            href="https://reactjs.org"
            target="_blank"
            rel="noopener noreferrer"
          >
            Learn React
          </a>
        </header>
      </div>
    );
  }
};

export default App;
