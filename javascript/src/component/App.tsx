import React from 'react';

import { expressedFormulaToString, expressibleToExpressedFormula, operatorToString } from '../Util';
import './App.css';
import Example1 from './Example1';
import Simplification from './Simplification';

const App: React.FunctionComponent = () => {
  const [vars, setVars] = React.useState<number>(2);
  const [op, setOp] = React.useState<Operator>('::<=?');
  const [r, setR] = React.useState('3');
  const [conv, setConv] = React.useState(false);
  const handleSelectChange: React.FormEventHandler<HTMLSelectElement> = React.useCallback((e) => {
    setVars(Number.parseInt(e.currentTarget.value[0], 10));
    setOp(e.currentTarget.value.slice(1) as Operator);
  }, [setOp]);
  const handleInputChange: React.FormEventHandler<HTMLInputElement> = React.useCallback((e) => {
    setR(e.currentTarget.value);
  }, [setR]);
  const handleCheckboxChange: React.FormEventHandler<HTMLInputElement> = React.useCallback((e) => {
    setConv(e.currentTarget.checked);
  }, [setR]);
  const expressible = React.useMemo<Expressible>(() => vars === 1 ? [1, op, Number.parseFloat(r)] : [1, 2, op, Number.parseFloat(r)], [op, r]);

  return (
    <div className='App'>
      <fieldset className='expression-picker'>
        <legend>Choose an expression to be converted</legend>
        <label>
          Type of operation
          <select value={vars + op} onChange={handleSelectChange}>
            <option label='x1 - x2 ≤ r' value='2::<=?' />
            <option label='x1 - x2 < r' value='2::<?' />
            <option label='x1 - x2 ≥ r' value='2::>=?' />
            <option label='x1 - x2 > r' value='2::>=?' />
            <option label='x1 - x2 = r' value='2::=?' />
            <option label='x1 - x2 ≠ r' value='2::<>?' />
            <option label='x1 ≤ r' value='1::<=?' />
            <option label='x1 < r' value='1::<?' />
            <option label='x1 ≥ r' value='1::>=?' />
            <option label='x1 > r' value='1::>=?' />
            <option label='x1 = r' value='1::=?' />
            <option label='x1 ≠ r' value='1::<>?' />
          </select>
        </label>
        <label>
          Value of r = <span>{r}</span>
          <input type='range' min='-5' max='5' step='0.1' value={r} onChange={handleInputChange} />
        </label>
        <br />
        <br />
        <label>
          Convert? <input type='checkbox' checked={conv} onChange={handleCheckboxChange} />
        </label>
      </fieldset>
      <div>
        Original Expression: x1 {vars === 2 ? '- x2' : ''} {`${operatorToString(op)} ${r}`}
        <br />
        {conv ? `Converted Expression: ${expressedFormulaToString(expressibleToExpressedFormula(expressible))}` : (<br />)}
      </div>
      <Simplification
        width={640}
        height={400}
        convert={conv}
        expressible={expressible}
      />
      <Example1 />
    </div>
  );
};

export default App;
