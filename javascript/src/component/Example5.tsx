import React from 'react';

import { useSatyrosAPI } from '../hook/useSatyrosAPI';
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

const Example5: React.FunctionComponent = () => {
  const satyrosAPI = useSatyrosAPI(f);

  if (satyrosAPI === undefined) {
    return null;
  } else {
    const formula = satyrosAPI.initialFormula;

    return (
      <div style={{ display: 'block', width: 160, margin: '0 auto' }}>
        <ClauseDB
          width={160}
          height={500}
          formula={formula}
          assignmentAPI={{ getValueMapList(cb) { cb([]); }, getValue(_v, cb) { cb(null); }, getValueOfClauseMapList(cb) { cb([]); }, getValueOfClause(_i, cb) { cb(null); }, getValueOfFormula(cb) { cb(null); } }}
          update={0}
        />
      </div>
    );
  }
};

export default Example5;
