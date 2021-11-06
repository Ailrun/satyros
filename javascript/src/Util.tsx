import * as d3 from "d3";

const operatorToString = (operator: Operator): string => {
  switch (operator) {
    case "::<?": return "<";
    case "::<=?": return "≤";
    case "::>?": return ">";
    case "::>=?": return "≥";
    case "::<>?": return "≠";
    case "::=?": return "=";
  }
}

export const expressibleToString = (expressible: Expressible): string => {
  if (expressible.length === 3) {
    return `x${expressible[0]} ${operatorToString(expressible[1])} ${expressible[2]}`;
  } else {
    return `x${expressible[0]} - x${expressible[1]} ${operatorToString(expressible[2])} ${expressible[3]}`;
  }
};

export const expressedToString = (expressed: Expressed): string => `x${expressed[0]} - x${expressed[1]} ≤ ${expressed[2]}`;

export const d3fyExpressibleFormula = (expressibleFormula: FormulaLike<Expressible>, e: SVGSVGElement, width: number): void => {
  const space = 70;
  const svg = d3.select(e);

  const expressibleG = svg
    .append('g')
    .classed('expressible', true)
    .attr('transform', `translate(${width / 2 - expressibleFormula.flat().length * space + space}, 40)`)

  const expressibleClauseG = expressibleG
    .selectChildren('g.expressible-clause')
    .data(expressibleFormula)
    .join('g')
    .classed('expressible-clause', true)
    .attr('transform', (_d, i) => `translate(${expressibleFormula.slice(0, i).flat().length * 2 * space}, 0)`);

  expressibleClauseG
    .selectChildren('g.expressible-literal')
    .data(d => d)
    .join(enter => {
      const g = enter.append('g');

      g.append('text')
        .text(expressibleToString)
        .attr('text-anchor', 'middle')
        .attr('y', 40)
        .attr('font-size', '18px');

      g.append('circle')
        .attr('cx', 0)
        .attr('cy', 10)
        .attr('r', 10);

      return g;
    })
    .classed('expressible-literal', true)
    .attr('transform', (_d, i) => `translate(${i * 2 * space}, 0)`);

  expressibleClauseG
    .selectChildren('g.expressible-literal-con')
    .data(d => d.slice(1))
    .join('g')
    .classed('expressible-literal-con', true)
    .attr('transform', (_d, i) => `translate(${i * 2 * space + space}, 0)`)
    .append('text')
    .text('∨')
    .attr('text-anchor', 'middle')
    .attr('y', 30)
    .attr('font-size', '40px');

  expressibleG
    .selectChildren('g.expressible-clause-con')
    .data(expressibleFormula.slice(1))
    .join('g')
    .classed('expressible-clause-con', true)
    .attr('transform', (_d, i) => `translate(${expressibleFormula.slice(0, i + 1).flat().length * 2 * space - space}, 0)`)
    .append('text')
    .text('∧')
    .attr('text-anchor', 'middle')
    .attr('y', 30)
    .attr('font-size', '40px');
}

export const d3fyExpressedFormula = (expressedFormula: FormulaLike<Expressed>, e: SVGSVGElement, width: number): void => {
  const space = 70;
  const expressedG = d3.select(e)
    .classed('expressed', true)
    .attr('transform', `translate(${width / 2 - expressedFormula.flat().length * space + space}, 40)`)

  const expressedClauseG = expressedG
    .selectChildren('g.expressed-clause')
    .data(expressedFormula)
    .join('g')
    .classed('expressed-clause', true)
    .attr('transform', (_d, i) => `translate(${expressedFormula.slice(0, i).flat().length * 2 * space}, 0)`);

  expressedClauseG
    .selectChildren('g.expressed-literal')
    .data(d => d)
    .join(enter => {
      const g = enter.append('g');

      g.append('text')
        .text(expressedToString)
        .attr('text-anchor', 'middle')
        .attr('y', 40)
        .attr('font-size', '18px');

      g.append('circle')
        .attr('cx', 0)
        .attr('cy', 10)
        .attr('r', 10);

      return g;
    })
    .classed('expressed-literal', true)
    .attr('transform', (_d, i) => `translate(${i * 2 * space}, 0)`);

  expressedClauseG
    .selectChildren('g.expressed-literal-con')
    .data(d => d.slice(1))
    .join('g')
    .classed('expressed-literal-con', true)
    .attr('transform', (_d, i) => `translate(${i * 2 * space + space}, 0)`)
    .append('text')
    .text('∨')
    .attr('text-anchor', 'middle')
    .attr('y', 30)
    .attr('font-size', '40px');

  expressedG
    .selectChildren('g.expressed-clause-con')
    .data(expressedFormula.slice(1))
    .join('g')
    .classed('expressed-clause-con', true)
    .attr('transform', (_d, i) => `translate(${expressedFormula.slice(0, i + 1).flat().length * 2 * space - space}, 0)`)
    .append('text')
    .text('∧')
    .attr('text-anchor', 'middle')
    .attr('y', 30)
    .attr('font-size', '40px');
}
