import * as d3 from 'd3';
import React, { Fragment, useMemo } from 'react';

import { useSatyrosAPI } from '../hook/useSatyrosAPI';
import { ImplicationGraph } from './ImplicationGraph';
import { BellmanFordGraph } from './BellmanFordGraph';
import { stepsExcept, haskellCallWrapper, undosExcept } from '../Util';
import ConversionTable from './ConversionTable';
import ClauseDB from './ClauseDB';

const f: FormulaLike<Expressible> =
  [
    [
      [1, 4, '::>=?', 4],
      [4, 1, '::>=?', 5],
    ],
    [
      [4, 5, '::>=?', 4],
      [5, 4, '::>=?', 4],
    ],
    [
      [5, 1, '::>=?', 5],
      [1, 5, '::>=?', 4],
    ],
    [
      [2, 3, '::>=?', 1],
      [3, 2, '::>=?', 2],
    ],
    [
      [1, '::>=?', 0],
    ],
    [
      [1, '::<=?', 13 - 5],
    ],
    [
      [2, '::>=?', 0],
    ],
    [
      [2, '::<=?', 13 - 2],
    ],
    [
      [3, '::>=?', 0],
    ],
    [
      [3, '::<=?', 13 - 1],
    ],
    [
      [4, '::>=?', 0],
    ],
    [
      [4, '::<=?', 13 - 4],
    ],
    [
      [5, '::>=?', 0],
    ],
    [
      [5, '::<=?', 13 - 4],
    ],
  ];

const TrueText: React.FunctionComponent<{ style?: React.CSSProperties }> = ({ style, children }) => {
  return (
    <span style={{ color: d3.schemeSet2[0], ...style }}>{ children }</span>
  );
};

const FalseText: React.FunctionComponent<{ style?: React.CSSProperties }> = ({ style, children }) => {
  return (
    <span style={{ color: d3.schemeSet2[1], ...style }}>{ children }</span>
  );
};

const Example8: React.FunctionComponent = () => {
  const satyrosAPI = useSatyrosAPI(f);
  const [stage, setStage] = React.useState<number>(0);
  const [implicationNodes, setImplicationNodes] = React.useState<SatyrosImplicationVertex[]>([]);
  const [implicationLinks, setImplicationLinks] = React.useState<SatyrosImplicationEdge[]>([]);
  const [bellmanFordNodes, setBellmanFordNodes] = React.useState<SatyrosBellmanFordVertex[]>([]);
  const [bellmanFordLinks, setBellmanFordLinks] = React.useState<SatyrosBellmanFordEdge[]>([]);
  const [ctVersion, setCtVersion] = React.useState<number>(0);
  const [fVersion, setFVersion] = React.useState<number>(0);
  const conversion = useMemo(() => satyrosAPI?.conversionTable, [satyrosAPI]);
  const formula = useMemo(() => satyrosAPI !== undefined ? haskellCallWrapper(satyrosAPI!.getFormula)[0] : undefined, [satyrosAPI, fVersion]);

  const updateState = React.useCallback(() => {
    setCtVersion(s => s + 1);
    setFVersion(s => s + 1);
    satyrosAPI!.getImplicationGraph((ns, ls) => {
      setImplicationNodes(ns);
      setImplicationLinks(ls);
    });
    satyrosAPI!.getBellmanFordGraph((ns, ls) => {
      setBellmanFordNodes(ns);
      setBellmanFordLinks(ls);
    });
  }, [satyrosAPI]);
  const step = React.useCallback(() => {
    switch (stage) {
      case 0:
        setStage(s => s + 1);
        satyrosAPI!.step(stepsExcept(
          cb => {
            setStage(s => s + 1);
            satyrosAPI!.step(cb);
          },
          {
            DecisionComplete() {
              updateState();
            },
          },
        ));
        break;
      case 16:
        setStage(s => s + 1);
        satyrosAPI!.step(stepsExcept(
          cb => {
            setStage(s => s + 1);
            satyrosAPI!.step(cb);
          },
          {
            PropagationNth() {
              updateState();
            },
          },
        ));
        break;
      case 17:
        setStage(s => s + 1);
        satyrosAPI!.step(stepsExcept(
          cb => {
            setStage(s => s + 1);
            satyrosAPI!.step(cb);
          },
          {
            BCPConflictDrivenClause() {
              updateState();
            },
          },
        ));
        break;
      case 196:
        setStage(s => s + 1);
        satyrosAPI!.step(stepsExcept(
          cb => {
            setStage(s => s + 1);
            satyrosAPI!.step(cb);
          },
          {
            BacktraceComplete() {
              updateState();
            },
          },
        ));
        break;
      case 197:
        setStage(s => s + 1);
        satyrosAPI!.step(stepsExcept(
          cb => {
            setStage(s => s + 1);
            satyrosAPI!.step(cb);
          },
          {
            PropagationNth() {
              updateState();
            },
          },
        ));
        break;
      case 200:
        setStage(s => s + 1);
        satyrosAPI!.step(stepsExcept(
          cb => {
            setStage(s => s + 1);
            satyrosAPI!.step(cb);
          },
          {
            BacktraceComplete() {
              updateState();
            },
          },
        ));
        break;
      case 345:
        setStage(s => s + 1);
        satyrosAPI!.step(stepsExcept(
          cb => {
            setStage(s => s + 1);
            satyrosAPI!.step(cb);
          },
          {
            PropagationNth() {
              updateState();
            },
          },
        ));
        break;
      case 353:
        setStage(s => s + 1);
        satyrosAPI!.step(stepsExcept(
          cb => {
            setStage(s => s + 1);
            satyrosAPI!.step(cb);
          },
          {
            Finish() {
              updateState();
            },
          },
        ));
        break;
      default:
        console.log('WTF?');
        break;
    }
  }, [satyrosAPI, stage, updateState]);
  const undo = React.useCallback(() => {
    const stages = [0, 16, 17, 196, 197, 200, 345, 353, 490];
    const stageIndex = stages.indexOf(stage);
    let localCounter = stages[stageIndex] - stages[stageIndex - 1];
    setStage(s => s - 1);
    satyrosAPI!.undo(undosExcept(
      cb => {
        localCounter--;
        if (localCounter !== 0) {
          setStage(s => s - 1);
          satyrosAPI!.undo(cb);
        } else {
          updateState();
        }
      },
      {
        Start() {
          updateState();
        }
      },
    ));
  }, [satyrosAPI, stage, updateState]);
  const reset = React.useCallback(() => {
    satyrosAPI!.reset();
    setStage(0);
    setImplicationNodes([]);
    setImplicationLinks([]);
    setBellmanFordNodes([]);
    setBellmanFordLinks([]);
  }, [satyrosAPI]);
  const text = React.useMemo<React.ReactNode>(() => {
    switch (stage) {
      case 0: return 'Let\'s start with SAT solver.';
      case 16: return <Fragment>SAT Solver picks <FalseText><var>b1</var> (false)</FalseText>, <TrueText><var>b3</var> (true)</TrueText>, <TrueText><var>b8</var> (true)</TrueText>, <TrueText><var>b7</var> (true)</TrueText>, <FalseText><var>b5</var> (false)</FalseText>, and <TrueText><var>b4</var> (true)</TrueText> in the order, and this determines values of all boolean variables. Now, we can run QFIDL with the expression highlighted in the conversion table.</Fragment>;
      case 17: return <Fragment>First, we construct a graph. Here, I omitted the <var>min</var> node as its edges are obvious. Now, we apply Bellman-Ford algorithm to find minimum distances…</Fragment>;
      case 196: return <Fragment>Oops! we find a negative cycle. Two edges between <var>x4</var> and <var>x5</var> caused the cycle.</Fragment>;
      case 197: return <Fragment>We translated this cycle into a SAT conflict, ¬<var>b4</var> ∨ ¬<var>b3</var>, and revert the recent-most decision, <TrueText><var>b4</var> (true)</TrueText>.</Fragment>;
      case 200: return <Fragment>Now we derived <FalseText><var>b4</var> (false)</FalseText> instead. Does this allow us to find minimum distances?</Fragment>;
      case 345: return <Fragment>Unfortunately, our graph still has a negative cycle. Two edges between <var>x2</var> and <var>x3</var> caused the cycle, so we block it like ¬<var>b8</var> ∨ ¬<var>b7</var> and go back to the level where we assigned <TrueText><var>b7</var> (true)</TrueText>.</Fragment>;
      case 353: return <Fragment>With <FalseText><var>b7</var> (false)</FalseText>, can we find minimum distances?</Fragment>;
      case 490: return <Fragment>Yes! We can find! This gives us <var>x1</var> = 4, <var>x2</var> = 0, <var>x3</var> = 2, <var>x4</var> = 9, <var>x5</var> = 0.</Fragment>;
      default: return 'NYI…';
    }
  }, [stage]);

  if (satyrosAPI === undefined) {
    return null;
  } else {
    return (
      <div style={{ position: 'relative', height: 1000 + 120 }}>
        <div style={{ height: 100, boxSizing: 'border-box', border: '1px black solid', padding: 10, margin: '10px auto' }}>{text}</div>
        <div style={{ position: 'absolute', left: '50%', top: 120, transform: 'translate(-450px, 0)', width: 1000, margin: '0 auto' }}>
        <div style={{ display: 'block', width: 500, padding: '3px', border: 'solid 1px', borderColor: d3.schemeAccent[7], textAlign: 'center' }}><span style={{ color: d3.schemeSet2[7], margin: '0 1em' }}>Unassigned</span> <TrueText style={{margin: '0 1em'}}>True</TrueText> <FalseText style={{margin: '0 1em'}}>False</FalseText><br/><svg width='1em' height='1em' viewBox='-0.5em -0.5em 1em 1em' style={{ marginLeft: '1em' }}><rect width='1em' height='1em'></rect></svg> : decision variable <svg width='1em' height='1em' viewBox='-0.5em -0.5em 1em 1em' style={{ marginLeft: '1em' }}><circle cx='0.5em' cy='0.5em' r='0.5em'></circle></svg> : derived variable</div>
          <button onClick={reset} disabled={stage === 0}>Reset</button>
          <button onClick={undo} disabled={stage === 0}>{'<'} Undo</button>
          <button onClick={step} disabled={stage === 490}>Step {'>'}</button>
          <br />
          <ImplicationGraph
            width={500}
            height={400}
            nodes={implicationNodes}
            links={implicationLinks}
          />
          <BellmanFordGraph
            width={500}
            height={400}
            nodes={bellmanFordNodes}
            links={bellmanFordLinks}
            hideMin={true}
          />
          <br />
          <ClauseDB
            width={500}
            height={600}
            formula={formula!}
            assignmentAPI={satyrosAPI.assignment}
            update={fVersion}
          />
          <div style={{ width: 462, float: 'right' }}>
            <ConversionTable
              width={462}
              height={600}
              conversionTable={conversion!}
              assignmentAPI={satyrosAPI.assignment}
              update={ctVersion}
            />
          </div>
        </div>
      </div>
    );
  }
};

export default Example8;
