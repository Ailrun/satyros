import * as d3 from 'd3';
import React from 'react';

import { d3fyExpressibleFormula, d3fyExpressedFormula } from './Util';

interface Props {
  width: number;
  height: number;
  expressibleFormula: FormulaLike<Expressible>;
  expressedFormula: FormulaLike<Expressed>;
  convert: boolean;
}

const Simplification: React.FunctionComponent<Props> = ({
  width, height, expressibleFormula, expressedFormula, convert
}) => {
  const ref = React.useRef<SVGSVGElement>(null);

  React.useEffect(() => {
    // d3fyExpressibleFormula(expressibleFormula, ref.current!, width);

    d3fyExpressedFormula(expressedFormula, ref.current!, width);
  }, [expressibleFormula, expressedFormula]);

  React.useEffect(() => {
    const svg = d3.select(ref.current);

    return () => {
      svg.selectAll('g').remove();
    };
  }, []);

  return (
    <svg
      width={width}
      height={height}
      viewBox={`0 0 ${width} ${height}`}
      ref={ref}
    />
  );
};

export default Simplification;
