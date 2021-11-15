import React from 'react';

import { useSatyrosAPI } from '../hook/useSatyrosAPI';
import { BellmanFordGraph } from './BellmanFordGraph';
import { stepsExcept, undosExcept } from '../Util';

const f: FormulaLike<Expressible> =
  [
    [
      [1, 4, '::<=?', 2],
    ],
    [
      [1, 2, '::<=?', 4],
    ],
    [
      [2, 1, '::<=?', -4],
    ],
    [
      [2, 3, '::<=?', 5],
    ],
    [
      [3, 4, '::<=?', -1],
    ],
  ];

const Example3: React.FunctionComponent = () => {
  const satyrosAPI = useSatyrosAPI(f);
  const [stage, setStage] = React.useState<number>(0);
  const [skipStep, setSkipStep] = React.useState<boolean>(false);
  const [skipUndo, setSkipUndo] = React.useState<boolean>(false);
  const [bellmanFordNodes, setBellmanFordNodes] = React.useState<SatyrosBellmanFordVertex[]>([]);
  const [bellmanFordLinks, setBellmanFordLinks] = React.useState<SatyrosBellmanFordEdge[]>([]);

  const satStep = React.useCallback(() => {
    satyrosAPI!.getBellmanFordGraph((ns, ls) => {
      setBellmanFordNodes(ns);
      setBellmanFordLinks(ls);
    });
  }, [satyrosAPI]);
  const qfidlStep = satStep;
  const satUndo = React.useCallback(() => {
    satyrosAPI!.getBellmanFordGraph((ns, ls) => {
      setBellmanFordNodes(ns);
      setBellmanFordLinks(ls);
    });
  }, [satyrosAPI]);
  const qfidlUndo = satUndo;
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
            PropagationCheck() {
              console.log('PropagationCheck', stage);
              qfidlStep();
            },
          },
        ));
        break;
      case 51:
        break;
      default:
        setStage(s => s + 1);
        satyrosAPI!.step(stepsExcept(
          cb => {
            setStage(s => s + 1);
            satyrosAPI!.step(cb);
          },
          {
            PropagationFindShorter() {
              console.log('PropagationFindShorter', stage);
              qfidlStep();
            },
            Finish() {
              console.log('Finish', stage);
              qfidlStep();
            },
          },
        ));
        break;
    }
  }, [satyrosAPI, stage, qfidlStep]);
  const undo = React.useCallback(() => {
    switch (stage) {
      case 4:
        setStage(s => s - 1);
        satyrosAPI!.undo(undosExcept(
          cb => {
            setStage(s => s - 1);
            satyrosAPI!.undo(cb);
          },
          {
            Start() {
              qfidlUndo();
            },
          },
        ));
        break;
      default:
        setStage(s => s - 1);
        satyrosAPI!.undo(undosExcept(
          cb => {
            setStage(s => s - 1);
            satyrosAPI!.undo(cb);
          },
          {
            PropagationCheck() {
              console.log('PropagationCheck');
              if (stage !== 5) {
                setSkipUndo(true);
              } else {
                qfidlUndo();
              }
            },
            PropagationFindShorter() {
              console.log('PropagationFindShorter', stage);
              qfidlUndo();
            },
            Start() {
              console.log('Start', stage);
              qfidlUndo();
            },
          },
        ));
        break;
    }
  }, [satyrosAPI, stage, qfidlUndo]);
  const reset = React.useCallback(() => {
    satyrosAPI!.reset();
    setStage(0);
    setBellmanFordNodes([]);
    setBellmanFordLinks([]);
  }, [satyrosAPI]);

  React.useEffect(() => {
    if (skipStep) {
      setSkipStep(false);
      step();
    }
  }, [skipStep, step]);

  React.useEffect(() => {
    if (skipUndo) {
      setSkipUndo(false);
      undo();
    }
  }, [skipUndo, undo]);

  console.log(stage);
  if (satyrosAPI === undefined) {
    return null;
  } else {
    return (
      <div style={{ display: 'block', width: 560, margin: '0 auto' }}>
        <button disabled={stage === 0} onClick={reset}>Reset</button>
        <button disabled={stage === 0} onClick={undo}>{'<'} Undo</button>
        <button disabled={stage === 51} onClick={step}>Step to next distance update {'>'}</button>
        <br />
        <BellmanFordGraph
          width={560}
          height={400}
          nodes={bellmanFordNodes}
          links={bellmanFordLinks}
          hideMin={false}
        />
      </div>
    );
  }
};

export default Example3;
