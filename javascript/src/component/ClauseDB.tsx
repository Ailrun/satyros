import * as d3 from 'd3';
import React from 'react';

import { haskellCallWrapper } from '../Util';

interface Props {
  width: number;
  height: number;
  formula: Formula;
  assignmentAPI: SatyrosAssignmentAPI;
  update: number;
}

const ClauseDB: React.FunctionComponent<Props> = ({
  width, height, formula, assignmentAPI, update,
}) => {
  const variableMap = React.useMemo(() => new Map(haskellCallWrapper(assignmentAPI.getValueMapList)[0]), [assignmentAPI, update]);
  const clauseMap = React.useMemo(() =>  new Map(haskellCallWrapper(assignmentAPI.getValueOfClauseMapList)[0]), [assignmentAPI, update]);
  const ref = React.useRef<SVGSVGElement>(null);
  const textFill = React.useCallback((v) => d3.schemeSet2[v === undefined ? 7 : v ? 0 : 1], []);
  const textShadeFill = React.useCallback((v) => d3.schemePastel2[v === undefined ? 7 : v ? 0 : 1], []);
  const literalFill = React.useCallback(d => {
    if (typeof d === 'string') {
      return textFill(undefined);
    }
    const v = variableMap.get(Math.abs(d));
    return textFill(v === undefined ? undefined : (v[0] === (d > 0)));
  }, [textFill, variableMap]);
  const literalShadeFill = React.useCallback(d => {
    if (typeof d === 'string') {
      return textShadeFill(undefined);
    }
    const v = variableMap.get(Math.abs(d));
    return textShadeFill(v === undefined ? undefined : (v[0] === (d > 0)));
  }, [textShadeFill, variableMap]);
  const clauseFill = React.useCallback((_d, i) => {
    const v = clauseMap.get(i);
    return textFill(v ?? undefined);
  }, [textFill, clauseMap]);
  const clauseShadeFill = React.useCallback((_d, i) => {
    const v = clauseMap.get(i);
    return textShadeFill(v ?? undefined);
  }, [textShadeFill, clauseMap]);
  const formulaFill = React.useCallback(() => {
    const v = haskellCallWrapper<[boolean | null]>(cb => assignmentAPI.getValueOfFormula(cb))[0];
    return textFill(v ?? undefined);
  }, [textFill, assignmentAPI]);
  const formulaShadeFill = React.useCallback(() => {
    const v = haskellCallWrapper<[boolean | null]>(cb => assignmentAPI.getValueOfFormula(cb))[0];
    return textShadeFill(v ?? undefined);
  }, [textShadeFill, assignmentAPI]);

  React.useEffect(() => {
    const svg = d3.select(ref.current);
    const g = svg.append('g')
      .attr('transform', 'translate(0, 30)');

    const content = g.append('g')
      .attr('transform', 'translate(10, 10)')
      .attr('font-size', '14px');

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

    content.selectChildren('text.clause-db-formula')
      .data([0])
      .join(
        enter => {
          const g = enter.append('g')
            .classed('clause-db-formula', true);

          const text = g.append('text')
            .text('f')
            .classed('clause-db-formula-text', true);

          text.transition()
            .duration(500)
            .attr('fill', formulaFill)
            .attr('stroke', formulaFill);

          text.clone(true).lower()
            .classed('clause-db-formula-text', false)
            .classed('clause-db-formula-text-shade', true)
            .transition()
            .duration(500)
            .attr('fill', formulaShadeFill)
            .attr('stroke', formulaShadeFill);

          return text;
        },
        update => {
          update.each(function () {
            d3.select(this)
              .selectChild('text.clause-db-formula-text')
              .transition()
              .duration(500)
              .attr('fill', formulaFill)
              .attr('stroke', formulaFill);

            d3.select(this)
              .selectChild('text.clause-db-formula-text-shade')
              .transition()
              .duration(500)
              .attr('fill', formulaShadeFill)
              .attr('stroke', formulaShadeFill);
          })

          return update;
        }
      );

    content.selectChildren('g.clause-db-list')
      .data(formula, d => JSON.stringify(d))
      .join(
        enter => {
          const g = enter.append('g')
            .classed('clause-db-list', true)
            .attr('opacity', 0)
            .attr('transform', (_d, i) => `translate(0, ${i * 30 + 30})`);
          g.transition().attr('opacity', 1);

          g.append('text')
            .classed('clause-db-clause', true)
            .text((_d, i) => `c${i}`)
            .attr('stroke', clauseFill)
            .attr('fill', clauseFill)
            .clone(true).lower()
            .classed('clause-db-clause', false)
            .classed('clause-db-clause-shade', true)
            .attr('stroke-width', 2)
            .attr('stroke', clauseShadeFill)
            .attr('fill', clauseShadeFill);

          g.append('text')
            .classed('clause-db-clause-equal', true)
            .attr('x', 30)
            .text('=');

          return g;
        },
        update => {
          update.each(function (_d, i) {
            d3.select(this)
              .selectChild('text.clause-db-clause')
              .transition()
              .duration(500)
              .attr('fill', clauseFill(_d, i))
              .attr('stroke', clauseFill(_d, i));

            d3.select(this)
              .selectChild('text.clause-db-clause-shade')
              .transition()
              .duration(500)
              .attr('fill', clauseShadeFill(_d, i))
              .attr('stroke', clauseShadeFill(_d, i));
          });

          return update;
        },
      )
      .each(function (d) {
        const g = d3.select(this).datum(d);

        g.selectChildren('g.clause-db-literal')
          .data(([d0, ...d]) => d.reduce<(number | string)[]>((acc, v) => [...acc, '∨', v], [d0]), d => JSON.stringify(d))
          .join(
            enter => {
              const texts = enter.append('g')
                .classed('clause-db-literal', true);

              texts
                .append('text')
                .classed('clause-db-literal-text', true)
                .text(d => typeof d === 'string' ? d : d > 0 ? `b${d}` : `¬b${-d}`)
                .attr('x', (_d, i) => i * 30 + 80)
                .attr('text-anchor', 'end')
                .attr('stroke', literalFill)
                .attr('fill', literalFill)
                .clone(true).lower()
                .classed('clause-db-literal-text', false)
                .classed('clause-db-literal-text-shade', true)
                .attr('stroke-width', 2)
                .attr('stroke', literalShadeFill)
                .attr('fill', literalShadeFill);

              return texts;
            }
          );

        g.selectAll('.clause-db-literal-text')
          .transition()
          .duration(750)
          .attr('stroke', literalFill)
          .attr('fill', literalFill)

        g.selectAll('.clause-db-literal-text-shade')
          .transition()
          .duration(750)
          .attr('stroke', literalShadeFill)
          .attr('fill', literalShadeFill);
      });

    const bbox = content.node()?.getBBox();

    g.select('rect')
      .attr('x', (bbox?.x ?? - 1) + 1)
      .attr('y', (bbox?.y ?? - 1) + 1)
      .attr('width', (bbox?.width ?? -18) + 18)
      .attr('height', (bbox?.height ?? -18) + 18);
  }, [clauseFill, clauseShadeFill, formulaFill, formulaShadeFill, literalFill, literalShadeFill, formula, assignmentAPI, update]);

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

export default ClauseDB;
