import * as d3 from 'd3';
import React from 'react';

import { useSatyrosAPI } from '../hook/useSatyrosAPI';
import ConversionTable from './ConversionTable';

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

const Example6: React.FunctionComponent = () => {
  const satyrosAPI = useSatyrosAPI(f);
  const [ctVersion, setCtVersion] = React.useState<number>(0);
  const pseudoAssignment = React.useRef(new Map<number, [boolean, Clause]>());
  const pseudoAssignmentAPI = React.useMemo<SatyrosAssignmentAPI>(() => ({
    getValueMapList(cb) {
      cb(Array.from(pseudoAssignment.current.entries()));
    },
    getValue(v, cb) {
      cb(pseudoAssignment.current.get(v) ?? null);
    },
    getValueOfClauseMapList(cb) {
      cb([]);
    },
    getValueOfClause(_i, cb) {
      cb(null);
    },
    getValueOfFormula(cb) {
      cb(null);
    },
  }), [pseudoAssignment]);
  const handleClick = React.useCallback((d: [number, [Expressed, number][]]) => {
    const v = pseudoAssignment.current.get(d[0])?.[0];
    if (v === undefined) {
      pseudoAssignment.current.set(d[0], [true, []]);
    } else if (v) {
      pseudoAssignment.current.set(d[0], [false, []]);
    } else {
      pseudoAssignment.current.delete(d[0]);
    }
    setCtVersion(s => s + 1);
  }, [pseudoAssignment, setCtVersion]);

  if (satyrosAPI === undefined) {
    return null;
  } else {
    return (
      <div style={{ display: 'block', width: 462, margin: '0 auto' }}>
        <div style={{ display: 'block', padding: '3px', border: 'solid 1px', borderColor: d3.schemeAccent[7], textAlign: 'center' }}><span style={{ color: d3.schemeSet2[7], margin: '0 1em' }}>Unassigned</span> <span style={{ color: d3.schemeSet2[0], margin: '0 1em' }}>True</span> <span style={{ color: d3.schemeSet2[1], margin: '0 1em' }}>False</span></div>
        <ConversionTable
          width={462}
          height={500}
          conversionTable={satyrosAPI.conversionTable}
          assignmentAPI={pseudoAssignmentAPI}
          onClick={handleClick}
          update={ctVersion}
        />
      </div>
    );
  }
};

export default Example6;
