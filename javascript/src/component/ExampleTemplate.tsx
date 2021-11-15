import React from 'react';

import { useSatyrosAPI } from '../hook/useSatyrosAPI';
import { ImplicationGraph } from './ImplicationGraph';
import { BellmanFordGraph } from './BellmanFordGraph';
import { stepsExcept, haskellCallWrapper, oneUndo } from '../Util';
import ConversionTable from './ConversionTable';
import ClauseDB from './ClauseDB';

const f1: FormulaLike<Expressible> =
  [
    [
      [1, 2, '::<?', 0],
      [3, '::<>?', 1],
    ],
    [
      [3, '::=?', 1],
      [1, 2, '::<=?', 0],
    ],
  ];

const Example1: React.FunctionComponent = () => {
  const satyrosAPI1 = useSatyrosAPI(f1);
  const [stage, setStage] = React.useState<number>(0);
  const [result, setResult] = React.useState<boolean | undefined>(undefined);
  const [last, setLast] = React.useState<boolean>(true);
  const [implicationNodes, setImplicationNodes] = React.useState<SatyrosImplicationVertex[]>([]);
  const [implicationLinks, setImplicationLinks] = React.useState<SatyrosImplicationEdge[]>([]);
  const [bellmanFordNodes, setBellmanFordNodes] = React.useState<SatyrosBellmanFordVertex[]>([]);
  const [bellmanFordLinks, setBellmanFordLinks] = React.useState<SatyrosBellmanFordEdge[]>([]);
  const [ctVersion, setCtVersion] = React.useState<number>(0);
  const [fVersion, setFVersion] = React.useState<number>(0);

  const satStep = React.useCallback(() => {
    setStage(s => s + 1);
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
  const qfidlStep = satStep;
  const satUndo = React.useCallback(() => {
    setStage(s => s - 1);
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
  const qfidlUndo = satUndo;
  const undo = React.useCallback(() => {
    if (!last) {
      setLast(true);
    } else if (result !== undefined) {
      setResult(undefined);
    }
    satyrosAPI1!.undo(oneUndo(qfidlUndo, satUndo, () => { }));
  }, [last, result, satUndo, qfidlUndo, satyrosAPI1]);
  const step = React.useCallback(() => {
    console.log(stage);
    if (result !== undefined) {
      if (last) {
        satyrosAPI1!.step(stepsExcept(satyrosAPI1!.step, {
          Finish(_r) {
            console.log('Finish');
            qfidlStep();
            setLast(false);
          },
        }));
      }
      return;
    }
    switch (stage) {
      case 0:
        satyrosAPI1!.step(stepsExcept(
          cb => {
            setStage(s => s + 1);
            satyrosAPI1!.step(cb);
          },
          {
            DecisionComplete() {
              console.log('DecisionComplete');
              satStep();
            },
          },
        ));
        break;
      case 7:
        satyrosAPI1!.step(stepsExcept(
          cb => {
            setStage(s => s + 1);
            satyrosAPI1!.step(cb);
          },
          {
            NegativeCycleFind() {
              console.log('NegativeCycleCheck');
              qfidlStep();
            },
            NegativeCyclePass() {
              console.log('NegativeCyclePass');
              qfidlStep();
              setResult(true);
            }
          },
        ));
        break;
      default:
        satyrosAPI1!.step(stepsExcept(
          cb => {
            setStage(s => s + 1);
            satyrosAPI1!.step(cb);
          },
          {
            NegativeCycleFind() {
              console.log('NegativeCycleFind');
              qfidlStep();
            },
            BacktraceExhaustion() {
              console.log('NegativeCycleCheck');
              satStep();
              setResult(false);
            },
            NegativeCyclePass() {
              console.log('NegativeCyclePass');
              qfidlStep();
              setResult(true);
            },
          },
        ));
        break;
      // default:
      //   (function recStep(): void {
      //     satyrosAPI1!.step(oneStep(recStep, satStep, setResult));
      //   })();
      //   break;
    }
  }, [satyrosAPI1, stage, result, last, satStep, qfidlStep]);
  const reset = React.useCallback(() => {
    satyrosAPI1!.reset();
    setStage(0);
    setBellmanFordNodes([]);
    setBellmanFordLinks([]);
  }, [qfidlUndo, satyrosAPI1]);

  if (satyrosAPI1 === undefined) {
    return null;
  } else {
    const c = satyrosAPI1.conversionTable;
    const f = haskellCallWrapper(satyrosAPI1.getFormula)[0];

    return (
      <div>
        <button onClick={reset}>Reset</button>
        <button onClick={undo}>{'<'} Undo</button>
        <button onClick={step}>Step {'>'}</button>
        <br />
        <ImplicationGraph
          width={640}
          height={400}
          nodes={implicationNodes}
          links={implicationLinks}
        />
        <BellmanFordGraph
          width={640}
          height={400}
          nodes={bellmanFordNodes}
          links={bellmanFordLinks}
          hideMin={false}
        />
        <br />
        <ClauseDB
          width={640}
          height={400}
          formula={f}
          assignmentAPI={satyrosAPI1.assignment}
          update={fVersion}
        />
        <ConversionTable
          width={640}
          height={400}
          conversionTable={c}
          assignmentAPI={satyrosAPI1.assignment}
          update={ctVersion}
        />
        {result === undefined ? '' : result ? 'true' : 'false'}
      </div>
    );
  }
};

export default Example1;
