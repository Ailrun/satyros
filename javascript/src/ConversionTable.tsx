import * as d3 from 'd3';
import React from 'react';

interface Props {
  width: number;
  height: number;
  conversionTable: SatyrosConversionTable;
}

const ConversionTableViewer: React.FunctionComponent<Props> = ({
  width, height, conversionTable
}) => {
  const ref = React.useRef<SVGSVGElement>(null);

  React.useEffect(() => {
    const svg = d3.select(ref.current);
    const gFor = svg
      .append('g')
      .selectAll('g')
      .data(conversionTable.variableToExpressed)
      .join('g')
      .attr('transform', d => `translate(${d[0] * 120}, 20)`)
      .append('text')
      .text(d => `x${d[1][0]} - x${d[1][1]} < ${d[1][2]}`)
      .attr('text-anchor', 'middle')
      .attr('y', 30)
      .attr('font-size', '18px');
  }, [conversionTable]);

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

export default ConversionTableViewer;
