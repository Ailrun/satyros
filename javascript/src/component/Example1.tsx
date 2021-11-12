import React from 'react';

import { useSatyrosAPI } from '../hook/useSatyrosAPI';
import { BellmanFordGraph } from './BellmanFordGraph';
import { stepsExcept, oneStep } from '../Util';

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

const Example1: React.FunctionComponent = () => {
  const satyrosAPI = useSatyrosAPI(f);
  const [stage, setStage] = React.useState<number>(0);
  const [bellmanFordNodes, setBellmanFordNodes] = React.useState<SatyrosBellmanFordVertex[]>([]);
  const [bellmanFordLinks, setBellmanFordLinks] = React.useState<SatyrosBellmanFordEdge[]>([]);

  const satStep = React.useCallback(() => {
    satyrosAPI!.getBellmanFordGraph((ns, ls) => {
      setBellmanFordNodes(ns);
      setBellmanFordLinks(ls);
    });
  }, [satyrosAPI]);
  const qfidlStep = satStep;
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
              setStage(s => s + 1);
              satyrosAPI!.step(oneStep(qfidlStep, satStep, () => {}));
            },
          },
        ));
        break;
      default:
        break;
    }
  }, [satyrosAPI, stage, satStep, qfidlStep]);
  const reset = React.useCallback(() => {
    satyrosAPI!.reset();
    setStage(0);
    setBellmanFordNodes([]);
    setBellmanFordLinks([]);
  }, [satyrosAPI]);

  if (satyrosAPI === undefined) {
    return null;
  } else {
    return (
      <div style={{ display: 'block', width: 640, margin: '0 auto' }}>
        <button disabled={stage === 0} onClick={reset}>Reset</button>
        <button disabled={stage === 3} onClick={step}>Construct</button>
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

export default Example1;
