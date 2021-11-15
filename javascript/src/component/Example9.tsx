import * as d3 from 'd3';
import React, { Fragment, useMemo } from 'react';

import { useSatyrosAPI1 } from '../hook/useSatyrosAPI1';
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

const Example9: React.FunctionComponent = () => {
  const satyrosAPI1 = useSatyrosAPI1(f);
  const [stage, setStage] = React.useState<number>(0);
  const [implicationNodes, setImplicationNodes] = React.useState<SatyrosImplicationVertex[]>([]);
  const [implicationLinks, setImplicationLinks] = React.useState<SatyrosImplicationEdge[]>([]);
  const [bellmanFordNodes, setBellmanFordNodes] = React.useState<SatyrosBellmanFordVertex[]>([]);
  const [bellmanFordLinks, setBellmanFordLinks] = React.useState<SatyrosBellmanFordEdge[]>([]);
  const [ctVersion, setCtVersion] = React.useState<number>(0);
  const [fVersion, setFVersion] = React.useState<number>(0);
  const conversion = useMemo(() => satyrosAPI1?.conversionTable, [satyrosAPI1]);
  const formula = useMemo(() => satyrosAPI1 !== undefined ? haskellCallWrapper(satyrosAPI1!.getFormula)[0] : undefined, [satyrosAPI1, fVersion]);

  const updateState = React.useCallback(() => {
    setCtVersion(s => s + 1);
    setFVersion(s => s + 1);
    satyrosAPI1!.getImplicationGraph((ns, ls) => {
      setImplicationNodes(ns);
      setImplicationLinks(ls);
    });
    satyrosAPI1!.getBellmanFordGraph((ns, ls) => {
      setBellmanFordNodes(ns);
      setBellmanFordLinks(ls);
    });
  }, [satyrosAPI1]);
  const step = React.useCallback(() => {
    switch (stage) {
      case 0:
        setStage(s => s + 1);
        satyrosAPI1!.step(stepsExcept(
          cb => {
            setStage(s => s + 1);
            satyrosAPI1!.step(cb);
          },
          {
            BCPComplete() {
              updateState();
            },
          },
        ));
        break;
      case 1:
        setStage(s => s + 1);
        satyrosAPI1!.step(stepsExcept(
          cb => {
            setStage(s => s + 1);
            satyrosAPI1!.step(cb);
          },
          {
            PropagationNth() {
              updateState();
            },
          },
        ));
        break;
      case 2:
        setStage(s => s + 1);
        satyrosAPI1!.step(stepsExcept(
          cb => {
            setStage(s => s + 1);
            satyrosAPI1!.step(cb);
          },
          {
            DecisionResult() {
              updateState();
            },
          },
        ));
        break;
      case 111:
        setStage(s => s + 1);
        satyrosAPI1!.step(stepsExcept(
          cb => {
            setStage(s => s + 1);
            satyrosAPI1!.step(cb);
          },
          {
            PropagationNth() {
              updateState();
            },
          },
        ));
        break;
      case 114:
        setStage(s => s + 1);
        satyrosAPI1!.step(stepsExcept(
          cb => {
            setStage(s => s + 1);
            satyrosAPI1!.step(cb);
          },
          {
            DecisionResult() {
              updateState();
            },
          },
        ));
        break;
      case 230:
        setStage(s => s + 1);
        satyrosAPI1!.step(stepsExcept(
          cb => {
            setStage(s => s + 1);
            satyrosAPI1!.step(cb);
          },
          {
            PropagationNth() {
              updateState();
            },
          },
        ));
        break;
      case 232:
        setStage(s => s + 1);
        satyrosAPI1!.step(stepsExcept(
          cb => {
            setStage(s => s + 1);
            satyrosAPI1!.step(cb);
          },
          {
            BCPComplete() {
              updateState();
            },
          },
        ));
        break;
      case 355:
        setStage(s => s + 1);
        satyrosAPI1!.step(stepsExcept(
          cb => {
            setStage(s => s + 1);
            satyrosAPI1!.step(cb);
          },
          {
            BCPComplete() {
              updateState();
            },
          },
        ));
        break;
      case 486:
        setStage(s => s + 1);
        satyrosAPI1!.step(stepsExcept(
          cb => {
            setStage(s => s + 1);
            satyrosAPI1!.step(cb);
          },
          {
            PropagationNth() {
              updateState();
            },
          },
        ));
        break;
      case 487:
        setStage(s => s + 1);
        satyrosAPI1!.step(stepsExcept(
          cb => {
            setStage(s => s + 1);
            satyrosAPI1!.step(cb);
          },
          {
            BCPConflictDrivenClause() {
              updateState();
            },
          },
        ));
        break;
      case 620:
        setStage(s => s + 1);
        satyrosAPI1!.step(stepsExcept(
          cb => {
            setStage(s => s + 1);
            satyrosAPI1!.step(cb);
          },
          {
            BacktraceComplete() {
              updateState();
            },
          },
        ));
        break;
      case 621:
        setStage(s => s + 1);
        satyrosAPI1!.step(stepsExcept(
          cb => {
            setStage(s => s + 1);
            satyrosAPI1!.step(cb);
          },
          {
            BCPComplete() {
              updateState();
            },
          },
        ));
        break;
      case 622:
        setStage(s => s + 1);
        satyrosAPI1!.step(stepsExcept(
          cb => {
            setStage(s => s + 1);
            satyrosAPI1!.step(cb);
          },
          {
            PropagationNth() {
              updateState();
            },
          },
        ));
        break;
      case 623:
        setStage(s => s + 1);
        satyrosAPI1!.step(stepsExcept(
          cb => {
            setStage(s => s + 1);
            satyrosAPI1!.step(cb);
          },
          {
            DecisionResult() {
              updateState();
            },
          },
        ));
        break;
      case 752:
        setStage(s => s + 1);
        satyrosAPI1!.step(stepsExcept(
          cb => {
            setStage(s => s + 1);
            satyrosAPI1!.step(cb);
          },
          {
            DecisionResult() {
              updateState();
            },
          },
        ));
        break;
      case 892:
        setStage(s => s + 1);
        satyrosAPI1!.step(stepsExcept(
          cb => {
            setStage(s => s + 1);
            satyrosAPI1!.step(cb);
          },
          {
            DecisionComplete() {
              updateState();
            },
          },
        ));
        break;
      default:
        console.log('WTF?');
        break;
    }
  }, [satyrosAPI1, stage, updateState]);
  const undo = React.useCallback(() => {
    const stages = [0, 1, 2, 111, 114, 230, 232, 355, 486, 487, 620, 621, 622, 623, 752, 892, 1202];
    const stageIndex = stages.indexOf(stage);
    let localCounter = stages[stageIndex] - stages[stageIndex - 1];
    setStage(s => s - 1);
    satyrosAPI1!.undo(undosExcept(
      cb => {
        localCounter--;
        if (localCounter !== 0) {
          setStage(s => s - 1);
          satyrosAPI1!.undo(cb);
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
  }, [satyrosAPI1, stage, updateState]);
  const reset = React.useCallback(() => {
    satyrosAPI1!.reset();
    setStage(0);
    setImplicationNodes([]);
    setImplicationLinks([]);
    setBellmanFordNodes([]);
    setBellmanFordLinks([]);
  }, [satyrosAPI1]);
  const text = React.useMemo<React.ReactNode>(() => {
    switch (stage) {
      case 0: return 'Let\'s start with SAT solver.';
      case 1: return <Fragment>SAT Solver derive values from unit clauses, and try to propagate constraints. Unfortunately, there{'\''}s nothing to propagate here.</Fragment>;
      case 2: return <Fragment>Anyway, maybe these unit clauses already have some QFIDL conflicts. Let{'\''}s check them with QFIDL solver!</Fragment>;
      case 111: return <Fragment>There was no negative cycle in the graph, yay!</Fragment>;
      case 114: return <Fragment>Now SAT solver decide a variable. It{'\''}s <FalseText><var>b1</var> (false)</FalseText> in this case. This derives <TrueText><var>b2</var> (true)</TrueText>. Does this decision OK? QFIDL solver will check it.</Fragment>;
      case 230: return <Fragment>It is also safe as there{'\''}s no negative cycle.</Fragment>;
      case 232: return <Fragment>Thus, SAT solver can choose one more variable. This time, it{'\''}s <TrueText><var>b3</var> (true)</TrueText>. This does not derive any more variables. Is it still safe?</Fragment>;
      case 355: return <Fragment>It turns out yes, and it allows SAT solver to go further. How about <TrueText><var>b8</var> (true)</TrueText>?</Fragment>;
      case 486: return <Fragment>It{'\''}s still ok… How about <TrueText><var>b7</var> (true)</TrueText>?</Fragment>;
      case 487: return <Fragment>Let{'\''}s run QFIDL solver once again…</Fragment>;
      case 620: return <Fragment>And it has a negative loop! This means our last decision, <TrueText><var>b7</var> (true)</TrueText>, was not a valid one from the perspective of QFIDL.</Fragment>;
      case 621: return <Fragment>Thus, we go back to previous level, and add a new clause for what we just learned: ¬<var>b8</var> ∨ ¬<var>b7</var>.</Fragment>;
      case 622: return <Fragment>We can derive <FalseText><var>b7</var> (false)</FalseText>. But we don{'\''}t know whether this one is safe or not for sure.</Fragment>;
      case 623: return <Fragment>So we run QFIDL solver…</Fragment>;
      case 752: return <Fragment>And yes, it is safe!</Fragment>;
      case 892: return <Fragment>SAT decides once more, <FalseText><var>b5</var> (false)</FalseText>, which implies <TrueText><var>b6</var> (true)</TrueText>, and we still don{'\''}t have any negative cycle.</Fragment>;
      case 1202: return <Fragment>By setting <FalseText><var>b4</var> (false)</FalseText>, the decision process over, and we checked there{'\''}s no negative cycle. This gives us final assignment: <var>x1</var> = 4, <var>x2</var> = 0, <var>x3</var> = 2, <var>x4</var> = 9, <var>x5</var> = 0.</Fragment>;
      default: return 'NYI...';
    }
  }, [stage]);

  console.log(stage);
  if (satyrosAPI1 === undefined) {
    return null;
  } else {
    return (
      <div style={{ position: 'relative', height: 1000 + 120 }}>
        <div style={{ height: 100, boxSizing: 'border-box', border: '1px black solid', padding: 10, margin: '10px auto' }}>{text}</div>
        <div style={{ position: 'absolute', left: '50%', top: 120, transform: 'translate(-450px, 0)', width: 1000, margin: '0 auto' }}>
        <div style={{ display: 'block', width: 500, padding: '3px', border: 'solid 1px', borderColor: d3.schemeAccent[7], textAlign: 'center' }}><span style={{ color: d3.schemeSet2[7], margin: '0 1em' }}>Unassigned</span> <TrueText style={{margin: '0 1em'}}>True</TrueText> <FalseText style={{margin: '0 1em'}}>False</FalseText><br/><svg width='1em' height='1em' viewBox='-0.5em -0.5em 1em 1em' style={{ marginLeft: '1em' }}><rect width='1em' height='1em'></rect></svg> : decision variable <svg width='1em' height='1em' viewBox='-0.5em -0.5em 1em 1em' style={{ marginLeft: '1em' }}><circle cx='0.5em' cy='0.5em' r='0.5em'></circle></svg> : derived variable</div>
          <button onClick={reset} disabled={stage === 0}>Reset</button>
          <button onClick={undo} disabled={stage === 0}>{'<'} Undo</button>
          <button onClick={step} disabled={stage === 1202}>Step {'>'}</button>
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
            assignmentAPI={satyrosAPI1.assignment}
            update={fVersion}
          />
          <div style={{ width: 462, float: 'right' }}>
            <ConversionTable
              width={462}
              height={600}
              conversionTable={conversion!}
              assignmentAPI={satyrosAPI1.assignment}
              update={ctVersion}
            />
          </div>
        </div>
      </div>
    );
  }
};

export default Example9;
