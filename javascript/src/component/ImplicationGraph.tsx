import * as d3 from 'd3';
import React from 'react';

import { useForceGraph } from '../hook/useForceGraph';

interface Props {
  readonly width: number;
  readonly height: number;
  readonly nodes: SatyrosImplicationVertex[];
  readonly links: SatyrosImplicationEdge[];
}

interface SimulationNode extends d3.SimulationNodeDatum {
  id: number;
  value: boolean;
  isDecision: boolean;
  level: number;
}

interface SimulationLink extends d3.SimulationLinkDatum<SimulationNode> {
  id: string;
  levelDiff: number;
  clauseIndex: number;
}

export const ImplicationGraph: React.FunctionComponent<Props> = ({
  width, height, nodes, links
}) => {
  const ref = React.useRef<SVGSVGElement>(null);

  const simulationNodes = React.useMemo(() => {
    return nodes.map(({
      variable, value, isDecision, level,
    }): SimulationNode => ({
      id: variable, value, isDecision, level,
    }));
  }, [nodes]);

  const simulationLinks = React.useMemo(() => {
    return links.map(({
      startVertex, endVertex, levelDiff, clauseIndex,
    }): SimulationLink => ({
      id: `${startVertex},${endVertex}`,
      source: startVertex,
      target: endVertex,
      levelDiff,
      clauseIndex,
    }));
  }, [links]);

  useForceGraph({
    id: 'implication',
    width,
    height,
    nodes: simulationNodes,
    links: simulationLinks,
    ref: ref.current,
    nodeId: d => d.id,
    nodeShape: d => d.isDecision ? 'rect' : 'circle',
    nodeText: d => `b${d.id}@${d.level}`,
    nodeFill: d => d3.schemePastel2[d.value ? 0 : 1],
    nodeFillNew: d => d3.schemeSet2[d.value ? 0 : 1],
    linkId: d => d.id,
    linkDistance: d => d.levelDiff * 20 + 50,
    linkText: d => `c${d.clauseIndex}`,
    linkStroke: d3.schemeSet2[7],
    linkStrokeNew: d3.schemeAccent[7],
  });

  return (
    <svg
      width={width}
      height={height}
      viewBox={`${- width / 2} ${- height / 2} ${width} ${height}`}
      ref={ref}
    />
  );
};
