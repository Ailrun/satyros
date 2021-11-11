import * as d3 from 'd3';
import React from 'react';

import { haskellCallWrapper } from '../Util';

interface Props {
  width: number;
  height: number;
  conversionTable: SatyrosConversionTable;
  assignmentAPI: SatyrosAssignmentAPI;
  update: number;
}

const ConversionTable: React.FunctionComponent<Props> = ({
  width, height, conversionTable, assignmentAPI, update,
}) => {
  const ref = React.useRef<SVGSVGElement>(null);
  const textFill = React.useCallback((v) => d3.schemeSet2[v === undefined ? 7 : v ? 0 : 1], []);
  const textShadeFill = React.useCallback((v) => d3.schemePastel2[v === undefined ? 7 : v ? 0 : 1], []);
  const variableFill = React.useCallback(d => {
    const v = haskellCallWrapper<[[boolean, Clause] | null]>(cb => assignmentAPI.getValue(d[0], cb))[0];
    return textFill(v?.[0]);
  }, [textFill, assignmentAPI]);
  const variableShadeFill = React.useCallback(d => {
    const v = haskellCallWrapper<[[boolean, Clause] | null]>(cb => assignmentAPI.getValue(d[0], cb))[0];
    return textShadeFill(v?.[0]);
  }, [textShadeFill, assignmentAPI]);
  const expressedFill = React.useCallback(d => {
    const v = haskellCallWrapper<[[boolean, Clause] | null]>(cb => assignmentAPI.getValue(Math.abs(d[1]), cb))[0];
    return textFill(v === null || v[0] === (d[1] < 0) ? undefined : v[0]);
  }, [textFill, assignmentAPI]);
  const expressedShadeFill = React.useCallback(d => {
    const v = haskellCallWrapper<[[boolean, Clause] | null]>(cb => assignmentAPI.getValue(Math.abs(d[1]), cb))[0];
    return textShadeFill(v === null || v[0] === (d[1] < 0) ? undefined : v[0]);
  }, [textShadeFill, assignmentAPI]);

  React.useEffect(() => {
    const svg = d3.select(ref.current);
    const g = svg.append('g');

    const content = g.append('g')
      .attr('transform', 'translate(10, 10)');

    const bbox = content.node()?.getBBox()

    g.append('rect')
      .attr('stroke', `${d3.schemeAccent[7]}`)
      .attr('stroke-width', '1')
      .attr('fill', 'none')
      .attr('x', (bbox?.x ?? -1) + 1)
      .attr('y', (bbox?.y ?? -1) + 1)
      .attr('width', (bbox?.width ?? -18) + 18)
      .attr('height', (bbox?.height ?? -18) + 18);
  }, []);

  React.useEffect(() => {
    const g = d3.select(ref.current).select<SVGGElement>('g');

    const content = g.select<SVGGElement>('g');

    content.selectChildren<SVGGElement, [number, [Expressed, number][]]>('g')
      .data(d3.sort(d3.groups(conversionTable.expressedToLiteral, d => Math.abs(d[1])), d => d[0]))
      .join(
        enter => {
          const g = enter.append('g');

          g.append('text')
            .classed('conversion-variable', true)
            .text(d => `b${d[0]}`)
            .attr('text-anchor', 'start')
            .attr('y', 30)
            .attr('font-size', '14px')
            .attr('fill', variableFill)
            .attr('stroke', variableFill)
            .clone(true).lower()
            .classed('conversion-variable', false)
            .classed('conversion-variable-shade', true)
            .attr('stroke-width', 3)
            .attr('fill', variableShadeFill)
            .attr('stroke', variableShadeFill);

          return g;
        },
        update => {
          update.selectChildren<SVGTextElement, [number, [Expressed, number][]]>('text.conversion-variable')
            .attr('fill', variableFill)
            .attr('stroke', variableFill);

          update.selectChildren<SVGTextElement, [number, [Expressed, number][]]>('text.conversion-variable-shade')
            .attr('fill', variableShadeFill)
            .attr('stroke', variableShadeFill);

          return update;
        },
      )
      .attr('transform', (_d, i) => `translate(0, ${i * 80})`);

    content.selectChildren<SVGGElement, [number, [Expressed, number][]]>('g')
      .selectChildren<SVGGElement, [Expressed, number]>('g.conversion-expressed')
      .data(d => d3.sort(d[1], d => d[1] < 0), d => d[1])
      .join(
        enter => {
          const g = enter.append('g').classed('conversion-expressed', true);

          g.append('text').classed('conversion-expressed-text', true)
            .text(d => `x${d[0][0]} - x${d[0][1]} ≤ ${d[0][2]}`)
            .attr('text-anchor', 'start')
            .attr('x', 40)
            .attr('y', (_d, i) => i * 30 + 30)
            .attr('font-size', '14px')
            .attr('stroke', expressedFill)
            .attr('fill', expressedFill)
            .clone(true).lower()
            .classed('conversion-expressed-text', false)
            .classed('conversion-expressed-text-shade', true)
            .attr('stroke-width', 3)
            .attr('stroke', expressedShadeFill)
            .attr('fill', expressedShadeFill);

          return g;
        },
        update => {
          update.selectChildren<SVGTextElement, [Expressed, number]>('text.conversion-expressed-text')
            .attr('stroke', expressedFill)
            .attr('fill', expressedFill);

          update.selectChildren<SVGTextElement, [Expressed, number]>('text.conversion-expressed-text-shade')
            .attr('stroke', expressedShadeFill)
            .attr('fill', expressedShadeFill);

          return update;
        },
      );

    const bbox = content.node()?.getBBox();

    g.select('rect')
      .attr('x', (bbox?.x ?? - 1) + 1)
      .attr('y', (bbox?.y ?? - 1) + 1)
      .attr('width', (bbox?.width ?? -18) + 18)
      .attr('height', (bbox?.height ?? -18) + 18);
  }, [variableFill, variableShadeFill, expressedFill, expressedShadeFill, conversionTable, assignmentAPI, update]);

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

export default ConversionTable;
