import * as d3 from "d3";
import React, { Fragment } from "react";

export const haskellCallWrapper = <T extends any[]>(haskell: (cb: (...args: T) => void) => void): T => {
  let v: T;

  haskell((...args) => {
    v = args;
  });

  return v!;
}

export const stepsExcept: (step: SatyrosAPI['step'], cbs: Partial<SatyrosEffectCallback>) => SatyrosEffectCallback = (step, cbs) => {
  return Object.assign(oneStep(() => { step(stepsExcept(step, cbs)); }, () => { step(stepsExcept(step, cbs)); }, () => { throw new Error('Why Result?!'); }), cbs);
}

export const stepsUntil: (step: SatyrosAPI['step'], key: keyof SatyrosEffectCallback, cb: () => void) => SatyrosEffectCallback = (step, key, cb) => stepsExcept(step, { [key]: cb });

export const oneStep: (qfidlStep: () => void, satStep: () => void, resultStep: (r: boolean) => void) => SatyrosEffectCallback = (qfidlStep, satStep, resultStep) => ({
  Start() {},
  NegativeCyclePass: qfidlStep,
  NegativeCycleFind: qfidlStep,
  NegativeCycleCheck: qfidlStep,
  PropagationEnd: qfidlStep,
  PropagationNth: qfidlStep,
  PropagationFindShorter: qfidlStep,
  PropagationCheck: qfidlStep,
  BacktraceComplete: satStep,
  BacktraceExhaustion: satStep,
  DecisionComplete: satStep,
  DecisionResult: satStep,
  BCPConflictDrivenClause: satStep,
  BCPConflict: satStep,
  BCPComplete: satStep,
  BCPUnitClause: satStep,
  Finish: resultStep,
});

export const undosExcept: (undo: SatyrosAPI['undo'], cbs: Partial<SatyrosEffectCallback>) => SatyrosEffectCallback = (undo, cbs) => {
  return Object.assign(oneUndo(() => { undo(undosExcept(undo, cbs)); }, () => { undo(undosExcept(undo, cbs)); }, () => { throw new Error('Why Start?!'); }), cbs);
}

export const undosUntil: (undo: SatyrosAPI['undo'], key: keyof SatyrosEffectCallback, cb: () => void) => SatyrosEffectCallback = (undo, key, cb) => undosExcept(undo, { [key]: cb });

export const oneUndo: (qfidlUndo: () => void, satUndo: () => void, startUndo: (repeated: boolean) => void) => SatyrosEffectCallback = (qfidlUndo, satUndo, startUndo) => ({
  Start: startUndo,
  NegativeCyclePass: qfidlUndo,
  NegativeCycleFind: qfidlUndo,
  NegativeCycleCheck: qfidlUndo,
  PropagationEnd: qfidlUndo,
  PropagationNth: qfidlUndo,
  PropagationFindShorter: qfidlUndo,
  PropagationCheck: qfidlUndo,
  BacktraceComplete: satUndo,
  BacktraceExhaustion: satUndo,
  DecisionComplete: satUndo,
  DecisionResult: satUndo,
  BCPConflictDrivenClause: satUndo,
  BCPConflict: satUndo,
  BCPComplete: satUndo,
  BCPUnitClause: satUndo,
  Finish() {},
});

export const operatorToString = (operator: Operator): string => {
  switch (operator) {
    case "::<?": return "<";
    case "::<=?": return "≤";
    case "::>?": return ">";
    case "::>=?": return "≥";
    case "::<>?": return "≠";
    case "::=?": return "=";
  }
}

export const expressibleToExpressedFormula = (expressible: Expressible): FormulaLike<Expressed> => {
  if (expressible.length === 3) {
    expressible = [expressible[0], 0, expressible[1], expressible[2]];
  }

  switch (expressible[2]) {
    case '::<?': return [[[expressible[0], expressible[1], Math.ceil(expressible[3] - 1)]]];
    case '::<=?': return [[[expressible[0], expressible[1], Math.floor(expressible[3])]]];
    case '::>?': return [[[expressible[1], expressible[0], Math.ceil(- expressible[3] - 1)]]];
    case '::>=?': return [[[expressible[1], expressible[0], Math.floor(- expressible[3])]]];
    case '::=?': return [[[expressible[0], expressible[1], Math.floor(expressible[3])]], [[expressible[1], expressible[0], Math.floor(- expressible[3])]]];
    case '::<>?': return [[[expressible[0], expressible[1], Math.ceil(expressible[3] - 1)], [expressible[1], expressible[0], Math.ceil(- expressible[3] - 1)]]];
  }
};

export const expressedToString = (expressed: Expressed): string => `${expressed[0] === 0 ? 'z' : 'x' + expressed[0]} - ${expressed[1] === 0 ? 'z' : 'x' + expressed[1]} ≤ ${expressed[2]}`;

export const expressedToFragment = (expressed: Expressed): React.ReactNode => (
  <Fragment>
    {expressed[0] === 0 ? 'z' : <Fragment>x<sub>{expressed[0]}</sub></Fragment>} - {expressed[1] === 0 ? 'z' : <Fragment>x<sub>{expressed[1]}</sub></Fragment>} ≤ {expressed[2]}
  </Fragment>
);

export const expressibleToString = (expressible: Expressible): string => {
  if (expressible.length === 3) {
    return `x${expressible[0]} ${operatorToString(expressible[1])} ${expressible[2]}`;
  } else {
    return `x${expressible[0]} - x${expressible[1]} ${operatorToString(expressible[2])} ${expressible[3]}`;
  }
};

export const expressedFormulaToString = (expressedFormula: FormulaLike<Expressed>): string => {
  return expressedFormula.map(v => v.map(e => expressedToString(e)).join(' ∨ ')).join(' ∧ ');
};

export const expressedFormulaToFragment = (expressedFormula: FormulaLike<Expressed>): React.ReactNode => {
  return expressedFormula.map(
    (expressedClause, i) => (
      <Fragment key={i}>
        {i === 0 ? null : ' ∧ '}
        {
          expressedClause.map(
            (expressed, i) => (
              <Fragment key={i}>
                {i === 0 ? null : ' ∨ '}
                {expressedToFragment(expressed)}
              </Fragment>
            )
          )
        }
      </Fragment>
    ),
  );
};

export const d3fyExpressibleFormula = (expressibleFormula: FormulaLike<Expressible>, e: SVGSVGElement, width: number): void => {
  const space = 70;
  const svg = d3.select(e);

  const expressibleG = svg
    .selectAll('g.expressed')
    .data([0])
    .join('g')
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
    .selectAll('g.expressed')
    .data([0])
    .join('g')
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
