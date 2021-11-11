import * as d3 from 'd3';
import React from 'react';
import { expressibleToExpressedFormula } from '../Util';

interface Props {
  width: number;
  height: number;
  expressible: Expressible;
  convert: boolean;
}

const Simplification: React.FunctionComponent<Props> = ({
  width, height, expressible, convert
}) => {
  const ref = React.useRef<SVGSVGElement>(null);
  const padding = 30;
  const ratio = height / width;
  const xScale = d3.scaleLinear([-5, 5], [-width/2 + padding, width/2 - padding]);
  const yScale = d3.scaleLinear([-5 * ratio, 5 * ratio], [height/2 - padding, -height/2 + padding]);

  React.useEffect(() => {
    const svg = d3.select(ref.current);

    svg.append('clipPath')
      .attr('id', 'graph-area')
      .append('rect')
      .attr('x', -width/2 + padding)
      .attr('y', -height/2 + padding)
      .attr('width', width - 2 * padding)
      .attr('height', height - 2 * padding)

    svg.append('g')
      .classed('expression-graph', true);

    const xAxis = d3.axisBottom(xScale)
      .ticks(10)
      .tickSizeInner(-height + 2 * padding)
      .tickSizeOuter(0)
      .tickPadding(padding/2);
    const yAxis = d3.axisLeft(yScale)
      .ticks(7)
      .tickSizeInner(-width + 2 * padding)
      .tickSizeOuter(0)
      .tickPadding(padding/2);

    const xAxisG = svg.append('g')
      .attr('transform', `translate(0, ${height/2 - padding})`)
      .call(xAxis);

    const yAxisG = svg.append('g')
      .attr('transform', `translate(${-width/2 + padding}, 0)`)
      .call(yAxis);

    xAxisG
      .call(g => {
        g.selectAll('.tick line')
          .attr('stroke', d3.schemeSet2[7]);

        g.selectAll('.tick').lower();
      });

    yAxisG
      .call(g => {
        g.selectAll('.tick line')
          .attr('stroke', d3.schemeSet2[7]);

        g.selectAll('.tick').lower();
      });

    svg.append('line')
      .attr('x1', -0.5)
      .attr('y1', -height/2 + padding)
      .attr('x2', -0.5)
      .attr('y2', height/2 - padding)
      .attr('stroke', 'black')
      .attr('stroke-width', 1);

    svg.append('line')
      .attr('x1', -width/2 + padding)
      .attr('y1', 0.5)
      .attr('x2', width/2 - padding)
      .attr('y2', 0.5)
      .attr('stroke', 'black')
      .attr('stroke-width', 1);
  }, []);

  React.useEffect(() => {
    const path = d3.select(ref.current)
      .selectChild('g.expression-graph')
      .selectChildren('path.expressible')
      .data([0])
      .join('path')
      .classed('expressible', true)
      .attr('clip-path', 'url(#graph-area)')
      .transition()
      .duration(300)
      .ease(d3.easeLinear)
      .attr('stroke-width', 3)
      .attr('fill', d3.schemeAccent[3])
      .attr('opacity', convert ? 0 : 1);

    const line = d3.path();

    if (expressible.length === 3) {
      switch (expressible[1]) {
        case '::<=?': {
          line.moveTo(xScale(expressible[2]), yScale(yScale.domain()[0] - 10));
          line.lineTo(xScale(expressible[2]), yScale(yScale.domain()[1] + 10));
          line.lineTo(xScale(xScale.domain()[0] - 10), yScale(yScale.domain()[1] + 10));
          line.lineTo(xScale(xScale.domain()[0] - 10), yScale(yScale.domain()[0] - 10));
          line.closePath();
          path.attr('d', line.toString())
            .attr('stroke', d3.schemeSet1[5]);
          break;
        }
        case '::<?': {
          line.moveTo(xScale(expressible[2]), yScale(yScale.domain()[0] - 10));
          line.lineTo(xScale(expressible[2]), yScale(yScale.domain()[1] + 10));
          line.lineTo(xScale(xScale.domain()[0] - 10), yScale(yScale.domain()[1] + 10));
          line.lineTo(xScale(xScale.domain()[0] - 10), yScale(yScale.domain()[0] - 10));
          line.closePath();
          path.attr('d', line.toString())
            .attr('stroke', '#FFF');
          break;
        }
        case '::>=?': {
          line.moveTo(xScale(expressible[2]), yScale(yScale.domain()[0] - 10));
          line.lineTo(xScale(expressible[2]), yScale(yScale.domain()[1] + 10));
          line.lineTo(xScale(xScale.domain()[1] + 10), yScale(yScale.domain()[1] + 10));
          line.lineTo(xScale(xScale.domain()[1] + 10), yScale(yScale.domain()[0] - 10));
          line.closePath();
          path.attr('d', line.toString())
            .attr('stroke', d3.schemeSet1[5]);
          break;
        }
        case '::>?': {
          line.moveTo(xScale(expressible[2]), yScale(yScale.domain()[0] - 10));
          line.lineTo(xScale(expressible[2]), yScale(yScale.domain()[1] + 10));
          line.lineTo(xScale(xScale.domain()[1] + 10), yScale(yScale.domain()[1] + 10));
          line.lineTo(xScale(xScale.domain()[1] + 10), yScale(yScale.domain()[0] - 10));
          line.closePath();
          path.attr('d', line.toString())
            .attr('stroke', '#FFF');
          break;
        }
        case '::=?': {
          line.moveTo(xScale(expressible[2]), yScale(yScale.domain()[0] - 10));
          line.lineTo(xScale(expressible[2]), yScale(yScale.domain()[1] + 10));
          path.attr('d', line.toString())
            .attr('stroke', d3.schemeSet1[5]);
          break;
        }
        case '::<>?': {
          line.moveTo(xScale(expressible[2]), yScale(yScale.domain()[0] - 10));
          line.lineTo(xScale(expressible[2]), yScale(yScale.domain()[1] + 10));
          line.lineTo(xScale(xScale.domain()[0] - 10), yScale(yScale.domain()[1] + 10));
          line.lineTo(xScale(xScale.domain()[0] - 10), yScale(yScale.domain()[0] - 10));
          line.closePath();
          line.moveTo(xScale(expressible[2]), yScale(yScale.domain()[0] - 10));
          line.lineTo(xScale(expressible[2]), yScale(yScale.domain()[1] + 10));
          line.lineTo(xScale(xScale.domain()[1] + 10), yScale(yScale.domain()[1] + 10));
          line.lineTo(xScale(xScale.domain()[1] + 10), yScale(yScale.domain()[0] - 10));
          line.closePath();
          path.attr('d', line.toString())
            .attr('stroke', '#FFF');
          break;
        }
      }
    } else {
      switch (expressible[2]) {
        case '::<=?': {
          line.moveTo(xScale(xScale.domain()[0] - 10), yScale(xScale.domain()[0] - 10 - expressible[3]));
          line.lineTo(xScale(xScale.domain()[1] + 10), yScale(xScale.domain()[1] + 10 - expressible[3]));
          line.lineTo(xScale(xScale.domain()[0] - 10), yScale(yScale.domain()[1] + 10));
          line.closePath();
          path.attr('d', line.toString())
            .attr('stroke', d3.schemeSet1[5]);
          break;
        }
        case '::<?': {
          line.moveTo(xScale(xScale.domain()[0] - 10), yScale(xScale.domain()[0] - 10 - expressible[3]));
          line.lineTo(xScale(xScale.domain()[1] + 10), yScale(xScale.domain()[1] + 10 - expressible[3]));
          line.lineTo(xScale(xScale.domain()[0] - 10), yScale(yScale.domain()[1] + 10));
          line.closePath();
          path.attr('d', line.toString())
            .attr('stroke', '#FFF');
          break;
        }
        case '::>=?': {
          line.moveTo(xScale(xScale.domain()[0] - 10), yScale(xScale.domain()[0] - 10 - expressible[3]));
          line.lineTo(xScale(xScale.domain()[1] + 10), yScale(xScale.domain()[1] + 10 - expressible[3]));
          line.lineTo(xScale(xScale.domain()[1] + 10), yScale(yScale.domain()[0] - 10));
          line.closePath();
          path.attr('d', line.toString())
            .attr('stroke', d3.schemeSet1[5]);
          break;
        }
        case '::>?': {
          line.moveTo(xScale(xScale.domain()[0] - 10), yScale(xScale.domain()[0] - 10 - expressible[3]));
          line.lineTo(xScale(xScale.domain()[1] + 10), yScale(xScale.domain()[1] + 10 - expressible[3]));
          line.lineTo(xScale(xScale.domain()[1] + 10), yScale(yScale.domain()[0] - 10));
          line.closePath();
          path.attr('d', line.toString())
            .attr('stroke', '#FFF');
          break;
        }
        case '::=?': {
          line.moveTo(xScale(xScale.domain()[0] - 10), yScale(xScale.domain()[0] - 10 - expressible[3]));
          line.lineTo(xScale(xScale.domain()[1] + 10), yScale(xScale.domain()[1] + 10 - expressible[3]));
          path.attr('d', line.toString())
            .attr('stroke', d3.schemeSet1[5]);
          break;
        }
        case '::<>?': {
          line.moveTo(xScale(xScale.domain()[0] - 10), yScale(xScale.domain()[0] - 10 - expressible[3]));
          line.lineTo(xScale(xScale.domain()[1] + 10), yScale(xScale.domain()[1] + 10 - expressible[3]));
          line.lineTo(xScale(xScale.domain()[0] - 10), yScale(yScale.domain()[1] + 10));
          line.closePath();
          line.moveTo(xScale(xScale.domain()[0] - 10), yScale(xScale.domain()[0] - 10 - expressible[3]));
          line.lineTo(xScale(xScale.domain()[1] + 10), yScale(xScale.domain()[1] + 10 - expressible[3]));
          line.lineTo(xScale(xScale.domain()[1] + 10), yScale(yScale.domain()[0] - 10));
          line.closePath();
          path.attr('d', line.toString())
            .attr('stroke', '#FFF');
          break;
        }
      }
    }
  }, [expressible[1], expressible[2], expressible[3], convert]);

  React.useEffect(() => {
    const path = d3.select(ref.current)
      .selectChild('g.expression-graph')
      .selectChildren('path.expressed')
      .data([0])
      .join('path')
      .classed('expressed', true)
      .attr('clip-path', 'url(#graph-area)')
      .transition()
      .duration(300)
      .ease(d3.easeLinear)
      .attr('stroke-width', 3)
      .attr('fill', d3.schemeAccent[1])
      .attr('stroke', d3.schemeSet1[3])
      .attr('opacity', convert ? 1 : 0);

    const line = d3.path();

    const expressedFormula = expressibleToExpressedFormula(expressible);

    if (expressedFormula.length === 1) {
      if (expressedFormula[0].length === 1) {
        if (expressedFormula[0][0][0] === 0) {
          line.moveTo(xScale(expressedFormula[0][0][2]), yScale(yScale.domain()[0]));
          line.lineTo(xScale(expressedFormula[0][0][2]), yScale(yScale.domain()[1]));
          line.lineTo(xScale(xScale.domain()[1] + 10), yScale(yScale.domain()[1] + 10));
          line.lineTo(xScale(xScale.domain()[1] + 10), yScale(yScale.domain()[0] - 10));
          line.closePath();
          path.attr('d', line.toString());
        } else if (expressedFormula[0][0][1] === 0) {
          line.moveTo(xScale(expressedFormula[0][0][2]), yScale(yScale.domain()[0]));
          line.lineTo(xScale(expressedFormula[0][0][2]), yScale(yScale.domain()[1]));
          line.lineTo(xScale(xScale.domain()[0] - 10), yScale(yScale.domain()[1] + 10));
          line.lineTo(xScale(xScale.domain()[0] - 10), yScale(yScale.domain()[0] - 10));
          line.closePath();
          path.attr('d', line.toString());
        } else {
          line.moveTo(xScale(xScale.domain()[0] - 10), yScale(xScale.domain()[0] - 10 - expressedFormula[0][0][2]));
          line.lineTo(xScale(xScale.domain()[1] + 10), yScale(xScale.domain()[1] + 10 - expressedFormula[0][0][2]));
          line.lineTo(xScale(xScale.domain()[0] - 10), yScale(yScale.domain()[1] + 10));
          line.closePath();
          path.attr('d', line.toString());
        }
      } else {
        if (expressedFormula[0][0][0] === 0) {
          line.moveTo(xScale(expressedFormula[0][0][2]), yScale(yScale.domain()[0]));
          line.lineTo(xScale(expressedFormula[0][0][2]), yScale(yScale.domain()[1]));
          line.lineTo(xScale(xScale.domain()[0] - 10), yScale(yScale.domain()[1] + 10));
          line.lineTo(xScale(xScale.domain()[0] - 10), yScale(yScale.domain()[0] - 10));
          line.closePath();
          line.moveTo(xScale(-expressedFormula[0][1][2]), yScale(yScale.domain()[0]));
          line.lineTo(xScale(-expressedFormula[0][1][2]), yScale(yScale.domain()[1]));
          line.lineTo(xScale(xScale.domain()[1] + 10), yScale(yScale.domain()[1] + 10));
          line.lineTo(xScale(xScale.domain()[1] + 10), yScale(yScale.domain()[0] - 10));
          line.closePath();
          path.attr('d', line.toString());
        } else if (expressedFormula[0][0][1] === 0) {
          line.moveTo(xScale(-expressedFormula[0][0][2]), yScale(yScale.domain()[0]));
          line.lineTo(xScale(-expressedFormula[0][0][2]), yScale(yScale.domain()[1]));
          line.lineTo(xScale(xScale.domain()[1] + 10), yScale(yScale.domain()[1] + 10));
          line.lineTo(xScale(xScale.domain()[1] + 10), yScale(yScale.domain()[0] - 10));
          line.closePath();
          line.moveTo(xScale(expressedFormula[0][1][2]), yScale(yScale.domain()[0]));
          line.lineTo(xScale(expressedFormula[0][1][2]), yScale(yScale.domain()[1]));
          line.lineTo(xScale(xScale.domain()[0] - 10), yScale(yScale.domain()[1] + 10));
          line.lineTo(xScale(xScale.domain()[0] - 10), yScale(yScale.domain()[0] - 10));
          line.closePath();
          path.attr('d', line.toString());
        } else {
          line.moveTo(xScale(xScale.domain()[0] - 10), yScale(xScale.domain()[0] - 10 - expressedFormula[0][0][2]));
          line.lineTo(xScale(xScale.domain()[1] + 10), yScale(xScale.domain()[1] + 10 - expressedFormula[0][0][2]));
          line.lineTo(xScale(xScale.domain()[0] - 10), yScale(yScale.domain()[1] + 10));
          line.closePath();
          line.moveTo(xScale(xScale.domain()[0] - 10), yScale(xScale.domain()[0] - 10 + expressedFormula[0][1][2]));
          line.lineTo(xScale(xScale.domain()[1] + 10), yScale(xScale.domain()[1] + 10 + expressedFormula[0][1][2]));
          line.lineTo(xScale(xScale.domain()[1] + 10), yScale(yScale.domain()[0] - 10));
          line.closePath();
          path.attr('d', line.toString());
        }
      }
    } else {
      if (expressedFormula[0][0][0] === 0) {
        if (expressedFormula[0][0][2] === - expressedFormula[1][0][2]) {
          line.moveTo(xScale(-expressedFormula[0][0][2]), yScale(yScale.domain()[0] - 10));
          line.lineTo(xScale(-expressedFormula[0][0][2]), yScale(yScale.domain()[1] + 10));
        }
        path.attr('d', line.toString());
      } else if (expressedFormula[0][0][1] === 0) {
        if (expressedFormula[0][0][2] === - expressedFormula[1][0][2]) {
          line.moveTo(xScale(expressedFormula[0][0][2]), yScale(yScale.domain()[0] - 10));
          line.lineTo(xScale(expressedFormula[0][0][2]), yScale(yScale.domain()[1] + 10));
        }
        path.attr('d', line.toString());
      } else {
        if (expressedFormula[0][0][2] === - expressedFormula[1][0][2]) {
          line.moveTo(xScale(xScale.domain()[0] - 10), yScale(xScale.domain()[0] - 10 + expressedFormula[0][0][2]));
          line.lineTo(xScale(xScale.domain()[1] + 10), yScale(xScale.domain()[1] + 10 + expressedFormula[0][0][2]));
        }
        path.attr('d', line.toString());
      }
    }
  }, [expressible[1], expressible[2], expressible[3], convert]);

  React.useEffect(() => {
    const svg = d3.select(ref.current);

    return () => {
      svg.selectAll().remove();
    };
  }, []);

  return (
    <svg
      width={width}
      height={height}
      viewBox={`${-width/2} ${-height/2} ${width} ${height}`}
      ref={ref}
    />
  );
};

export default Simplification;
