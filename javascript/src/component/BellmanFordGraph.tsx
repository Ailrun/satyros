import * as d3 from 'd3';
import React from 'react';

import { useForceGraph } from '../hook/useForceGraph';

interface Props {
  readonly width: number;
  readonly height: number;
  readonly nodes: SatyrosBellmanFordVertex[];
  readonly links: SatyrosBellmanFordEdge[];
}

interface SimulationNode extends d3.SimulationNodeDatum {
  id: number;
  distance: number;
}

interface SimulationLink extends d3.SimulationLinkDatum<SimulationNode> {
  id: string;
  weight: number;
  lastActive: boolean;
}

export const BellmanFordGraph: React.FunctionComponent<Props> = ({
  width, height, nodes, links
}) => {
  const ref = React.useRef<SVGSVGElement>(null);

  const simulationNodes = React.useMemo(() => {
    return nodes.map(({
      variable, distance,
    }): SimulationNode => ({
      id: variable ?? -1,
      distance,
    }));
  }, [nodes]);

  const simulationLinks = React.useMemo(() => {
    return links.map(({
      startVertex, endVertex, weight, lastActive,
    }): SimulationLink => ({
      id: `${startVertex ?? -1},${endVertex ?? -1}`,
      source: startVertex ?? -1,
      target: endVertex ?? -1,
      weight,
      lastActive,
    }));
  }, [links]);

  useForceGraph({
    id: 'bellman-ford',
    width,
    height,
    nodes: simulationNodes,
    links: simulationLinks,
    ref: ref.current,
    nodeId: d => d.id,
    nodeText: d => `${d.id === -1 ? 'min' : d.id === 0 ? 'z' : 'x' + d.id} : ${d.distance}`,
    nodeFill: d3.schemeSet2[2],
    nodeFillNew: d3.schemeAccent[4],
    linkId: d => d.id,
    linkActive: d => d.lastActive,
    linkDistance: _d => 20,
    linkText: d => `${d.weight}`,
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
