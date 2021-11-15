import React, { Fragment } from 'react';

import { useSatyrosAPI } from '../hook/useSatyrosAPI';
import { haskellCallWrapper } from '../Util';
import ClauseDB from './ClauseDB';
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

const Example7: React.FunctionComponent = () => {
  const satyrosAPI = useSatyrosAPI(f);

  if (satyrosAPI === undefined) {
    return null;
  } else {
    const formula = haskellCallWrapper(satyrosAPI.getFormula)[0];

    return (
      <Fragment>
        <div style={{ display: 'block', width: 160, margin: '0 auto' }}>
          <ClauseDB
            width={160}
            height={200}
            formula={formula}
            assignmentAPI={satyrosAPI.assignment}
            update={0}
          />
        </div>
        <div style={{ display: 'block', width: 462, margin: '0 auto' }}>
          <ConversionTable
            width={462}
            height={500}
            conversionTable={satyrosAPI.conversionTable}
            assignmentAPI={satyrosAPI.assignment}
            update={0}
          />
        </div>
      </Fragment>
    );
  }
};

export default Example7;
