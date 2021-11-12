import React from 'react';

import { useSatyrosAPI } from '../hook/useSatyrosAPI';
import { BellmanFordGraph } from './BellmanFordGraph';
import { stepsExcept, undosExcept } from '../Util';

const f: FormulaLike<Expressible> =
  [
    [
      [1, 2, '::<=?', 4],
    ],
    [
      [2, 3, '::<=?', 5],
    ],
    [
      [3, 1, '::<=?', -10],
    ],
  ];

const Example2: React.FunctionComponent = () => {
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
      case 23:
        break;
      default:
        setStage(s => s + 1);
        satyrosAPI!.step(stepsExcept(
          cb => {
            setStage(s => s + 1);
            satyrosAPI!.step(cb);
          },
          {
            PropagationCheck() {
              console.log('PropagationCheck', stage);
              if ([4, 11, 13].includes(stage)) {
                setSkipStep(true);
              } else {
                qfidlStep();
              }
            },
            PropagationFindShorter() {
              console.log('PropagationFindShorter');
              qfidlStep();
            },
            PropagationEnd() {
              console.log('PropagationEnd', stage);
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
              console.log('PropagationCheck', stage);
              if ([6, 14, 15].includes(stage)) {
                setSkipUndo(true);
              } else {
                qfidlUndo();
              }
            },
            PropagationFindShorter() {
              console.log('PropagationFindShorter');
              qfidlUndo();
            },
            PropagationEnd() {
              console.log('PropagationEnd');
              qfidlUndo();
            },
            Start() {
              console.log('Start');
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

  if (satyrosAPI === undefined) {
    return null;
  } else {
    return (
      <div style={{ display: 'block', width: 640, margin: '0 auto' }}>
        <button disabled={stage === 0} onClick={reset}>Reset</button>
        <button disabled={stage === 0} onClick={undo}>{'<'} Undo</button>
        <button disabled={stage === 23} onClick={step}>Step {'>'}</button>
        <br />
        <BellmanFordGraph
          width={640}
          height={400}
          nodes={bellmanFordNodes}
          links={bellmanFordLinks}
        />
      </div>
    );
  }
};

export default Example2;
