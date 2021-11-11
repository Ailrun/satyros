declare module 'd3-force-limit' {
  import * as d3 from 'd3';

  interface ForceLimit<N extends d3.SimulationNodeDatum, L extends d3.SimulationLinkDatum<N>> extends d3.Force<N, L> {
    radius(): number;
    radius(r: number): this;

    x0(): number;
    x0(x: number): this;
    x1(): number;
    x1(x: number): this;
    y0(): number;
    y0(y: number): this;
    y1(): number;
    y1(y: number): this;

    cushionWidth(): number;
    cushionWidth(w: number): this;

    cushionStrength(): number;
    cushionStrength(strength: number): this;
  }

  function d3ForceLimit<N extends d3.SimulationNodeDatum, L extends d3.SimulationLinkDatum<N>>(): ForceLimit<N, L>

  export = d3ForceLimit
}
