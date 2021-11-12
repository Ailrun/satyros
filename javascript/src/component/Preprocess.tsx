import React, { Fragment } from 'react';

import { expressedFormulaToFragment, expressibleToExpressedFormula, operatorToString } from '../Util';
import PreprocessGraph from './PreprocessGraph';

import './Preprocess.css';

interface Props {
  underSAT: boolean;
}

const Preprocess: React.FunctionComponent<Props> = ({ underSAT }) => {
  const [vars, setVars] = React.useState<number>(2);
  const [op, setOp] = React.useState<Operator>('::<=?');
  const [r, setR] = React.useState('3');
  const [processed, setProcessed] = React.useState(false);
  const handleTypeChange: React.FormEventHandler<HTMLSelectElement> = React.useCallback((e) => {
    setVars(Number.parseInt(e.currentTarget.value[0], 10));
    setOp(e.currentTarget.value.slice(1) as Operator);
  }, [setVars, setOp]);
  const handleRChange: React.FormEventHandler<HTMLInputElement> = React.useCallback((e) => {
    setR(e.currentTarget.value);
  }, [setR]);
  const handleProcessedChange: React.FormEventHandler<HTMLInputElement> = React.useCallback((e) => {
    setProcessed(e.currentTarget.checked);
  }, [setProcessed]);
  const expressible = React.useMemo<Expressible>(() => vars === 1 ? [1, op, Number.parseFloat(r)] : [1, 2, op, Number.parseFloat(r)], [vars, op, r]);

  return (
    <div className='Preprocess'>
      <fieldset className='expression-picker'>
        <legend>Choose an expression to be converted</legend>
        <label>
          Type of operation =
          <select value={vars + op} onChange={handleTypeChange}>
            <option label='x₁ - x₂ ≤ r' value='2::<=?' />
            <option label='x₁ - x₂ < r' value='2::<?' />
            <option label='x₁ - x₂ ≥ r' value='2::>=?' />
            <option label='x₁ - x₂ > r' value='2::>=?' />
            { underSAT ? (
                <Fragment>
                  <option label='x₁ - x₂ = r' value='2::=?' />
                  <option label='x₁ - x₂ ≠ r' value='2::<>?' />
                </Fragment>
              ) : null
            }
            <option label='x₁ ≤ r' value='1::<=?' />
            <option label='x₁ < r' value='1::<?' />
            <option label='x₁ ≥ r' value='1::>=?' />
            <option label='x₁ > r' value='1::>=?' />
            { underSAT ? (
                <Fragment>
                  <option label='x₁ = r' value='1::=?' />
                  <option label='x₁ ≠ r' value='1::<>?' />
                </Fragment>
              ) : null
            }
          </select>
        </label>
        <label>
          Value of r =
          <div>
            <input type='range' min='-5' max='5' step='0.1' value={r} onChange={handleRChange} />
            <input type='number' min='-5' max='5' value={r} onChange={handleRChange} />
          </div>
        </label>
        <br />
        <br />
        <label>
          Convert? <input type='checkbox' checked={processed} onChange={handleProcessedChange} />
        </label>
      </fieldset>
      <div className='expression-viewer'>
        Original Expression: x<sub>1</sub> {vars === 2 ? (<Fragment>- x<sub>2</sub></Fragment>) : ''} {`${operatorToString(op)} ${r}`}
        <br />
        <span className={processed ? '' : 'invisible'}>Processed Expression: {expressedFormulaToFragment(expressibleToExpressedFormula(expressible))}</span>
      </div>
      <PreprocessGraph
        width={640}
        height={400}
        processed={processed}
        expressible={expressible}
      />
    </div>
  );
};

export default Preprocess;
