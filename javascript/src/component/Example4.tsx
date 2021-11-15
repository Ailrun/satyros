import React, { Fragment } from 'react';

import { useSatyrosAPI } from '../hook/useSatyrosAPI';
import { expressedFormulaToFragment } from '../Util';

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

const Example4: React.FunctionComponent = () => {
  const satyrosAPI = useSatyrosAPI(f);

  if (satyrosAPI === undefined) {
    return null;
  } else {
    const ef = satyrosAPI.expressedFormula;

    return (
      <Fragment>
        {expressedFormulaToFragment(ef, true)}
      </Fragment>
    );
  }
};

export default Example4;
