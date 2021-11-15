import * as d3 from 'd3';
import d3ForceLimit from 'd3-force-limit';
import React from 'react';

const oldNodeTexts = d3.local<string>();
const oldLinkTexts = d3.local<string>();

export const useForceGraph = <N extends d3.SimulationNodeDatum, L extends d3.SimulationLinkDatum<N>>({
  id, width, height,
  nodes, links,
  ref,
  nodeId, nodeShape, nodeText, nodeFill, nodeFillNew,
  linkId, linkDistance, linkStroke, linkStrokeNew, linkStrokeActive, linkText, linkActive,
} : {
  id: string,
  width: number,
  height: number,
  nodes: N[],
  links: L[],
  ref: SVGSVGElement | null,
  nodeId(node: N): number | string,
  nodeShape?(node: N): 'circle' | 'rect',
  nodeText?(node: N): string,
  nodeFill?: ((node: N) => string) | string,
  nodeFillNew?: ((node: N) => string) | string,
  linkId(link: L): number | string,
  linkDistance(link: L): number,
  linkStroke?: string,
  linkStrokeNew?: string,
  linkStrokeActive?: ((link: L) => string) | string,
  linkText?(link: L): string,
  linkActive?(link: L): boolean,
}, deps: any[] = []) => {
  const radius = 8;
  const simulation = d3.forceSimulation<N, L>()
    .force('collide', d3.forceCollide(radius))
    .force('charge', d3.forceManyBody().strength(-1000).theta(0.5))
    .force('link', d3.forceLink<N, L>().id(d => nodeId(d)).strength(0.01).distance(linkDistance))
    .force('x', d3.forceX())
    .force('y', d3.forceY())
    .force('limits', d3ForceLimit().radius(5 * radius).x0(- width / 2).x1(width / 2).y0(- height / 2).y1(height / 2).cushionWidth(10 * radius).cushionStrength(1))
    .on('tick', ticked);
  const nodeRef = React.useRef<d3.Selection<SVGGElement, N, SVGGElement, any> | null>(null);
  const linkRef = React.useRef<d3.Selection<SVGGElement, L, SVGGElement, any> | null>(null);

  const nodeShapeImpl: (node: N) => 'circle' | 'rect' = React.useMemo(() => nodeShape ?? ((_d : N) => 'circle'), /* eslint-disable-line react-hooks/exhaustive-deps */ []);
  const nodeFillImpl = React.useMemo(() => nodeFill ?? '#000', /* eslint-disable-line react-hooks/exhaustive-deps */ []);
  const nodeFillNewImpl = React.useMemo(() => nodeFillNew ?? '#000', /* eslint-disable-line react-hooks/exhaustive-deps */ []);
  const linkStrokeImpl = linkStroke ?? '#000';
  const linkStrokeNewImpl = linkStrokeNew ?? '#000';
  const linkStrokeActiveImpl = linkStrokeActive ?? linkStrokeNewImpl;

  React.useEffect(() => {
    const svg = d3.select(ref);

    svg.append('defs')
      .append('marker')
      .attr('id', 'arrow-new')
      .attr('refX', 12)
      .attr('refY', 4)
      .attr('markerWidth', 8)
      .attr('markerHeight', 8)
      .attr('orient', 'auto')
      .append('path')
      .attr('d', 'M 0 0 8 4 0 8 2 4')
      .attr('fill', linkStrokeNewImpl);

    svg.append('defs')
      .append('marker')
      .attr('id', 'arrow')
      .attr('refX', 12)
      .attr('refY', 4)
      .attr('markerWidth', 8)
      .attr('markerHeight', 8)
      .attr('orient', 'auto')
      .append('path')
      .attr('d', 'M 0 0 8 4 0 8 2 4')
      .transition()
      .duration(500)
      .ease(d3.easeSinOut)
      .duration(3000)
      .attrTween('fill', () => {
        return d3.interpolateRgb(linkStrokeNewImpl, linkStrokeImpl);
      });

    linkRef.current = svg.append('g')
      .selectChildren('g');

    nodeRef.current = svg.append('g')
      .selectChildren('g');

    return () => {
      svg.selectChildren().remove();
    };
  }, /* eslint-disable-line react-hooks/exhaustive-deps */ [ref, ...deps]);

  function ticked() {
    if (ref !== null && linkRef.current !== null) {
      linkRef.current
        .attr('transform', d => {
          const xsource = (d.source as any).x;
          const xtarget = (d.target as any).x;
          const ysource = (d.source as any).y;
          const ytarget = (d.target as any).y;

          return `translate(${(xsource + xtarget) / 2}, ${(ysource + ytarget) / 2})`;
        })
        .select('path')
        .attr('d', d => {
          const xsource = (d.source as any).x;
          const xtarget = (d.target as any).x;
          const ysource = (d.source as any).y;
          const ytarget = (d.target as any).y;
          const r = Math.hypot(xtarget - xsource, ytarget - ysource);

          return `M${(xsource - xtarget) / 2},${(ysource - ytarget) / 2}A${r},${r} 0 0,1 ${(xtarget - xsource) / 2},${(ytarget - ysource) / 2}`;
        });

      linkRef.current
        .selectChildren<SVGTextElement, any>('text')
        .attr('x', d => {
          const r = Math.hypot((d.target as any).x - (d.source as any).x, (d.target as any).y - (d.source as any).y);
          const arc = svgArcToCenterParam(((d.source as any).x - (d.target as any).x) / 2, ((d.source as any).y - (d.target as any).y) / 2, r, r, 0, false, true, ((d.target as any).x - (d.source as any).x) / 2, ((d.target as any).y - (d.source as any).y) / 2);
          return (r + 5) * Math.cos(arc.startAngle + arc.deltaAngle / 2) + arc.cx;
        })
        .attr('y', d => {
          const r = Math.hypot((d.target as any).x - (d.source as any).x, (d.target as any).y - (d.source as any).y);
          const arc = svgArcToCenterParam(((d.source as any).x - (d.target as any).x) / 2, ((d.source as any).y - (d.target as any).y) / 2, r, r, 0, false, true, ((d.target as any).x - (d.source as any).x) / 2, ((d.target as any).y - (d.source as any).y) / 2);
          return (r + 5) * Math.sin(arc.startAngle + arc.deltaAngle / 2) + arc.cy;
        });
    }

    if (ref !== null && nodeRef.current !== null) {

      nodeRef.current
        .attr('transform', d => `translate(${(d as any).x}, ${(d as any).y})`);
    }
  }

  function drag(simulation: d3.Simulation<N, L>) {    
    function dragstarted(event: any) {
      if (!event.active) simulation.alphaTarget(0.3).restart();
      event.subject.fx = event.subject.x;
      event.subject.fy = event.subject.y;
    }
    
    function dragged(event: any) {
      event.subject.fx = event.x;
      event.subject.fy = event.y;
    }
    
    function dragended(event: any) {
      if (!event.active) simulation.alphaTarget(0);
      event.subject.fx = null;
      event.subject.fy = null;
    }
    
    return d3.drag()
      .on('start', dragstarted)
      .on('drag', dragged)
      .on('end', dragended);
  }

  React.useEffect(() => {
    const old = new Map(nodeRef.current!.data().map(d => [nodeId(d), d]));

    const newNodes: N[] = nodes.map(d => Object.assign(old.get(nodeId(d)) || {}, d));
    const newLinks: L[] = links.map(d => Object.assign({}, d));

    let hasEnteredNode = false;
    let hasEnteredLink = false;

    nodeRef.current = nodeRef.current!
      .data(newNodes, d => nodeId(d))
      .join(
        enter => {
          if (enter.size() > 0) {
            hasEnteredNode = true;
          }

          const g = enter.append('g')
            .attr('opacity', 0)
            .attr('stroke', '#333')
            .attr('stroke-opacity', 1)
            .attr('stroke-width', 0)
            .attr('stroke-linecap', 'round')
            .attr('stroke-linejoin', 'round');

          g.transition()
            .duration(500)
            .ease(d3.easeSinIn)
            .attr('opacity', 1);

          if (nodeText !== undefined) {
            g.append('text')
              .each(function (d) {
                oldNodeTexts.set(this, nodeText(d));
              })
              .text(d => nodeText(d))
              .classed('node-text', true)
              .attr('y', -10)
              .attr('text-anchor', 'middle')
              .attr('stroke', '#955')
              .attr('stroke-width', 1)
              .clone(true).lower()
              .classed('node-text', false)
              .classed('node-text-shade', true)
              .attr('stroke', 'white')
              .attr('stroke-width', 3);
          }

          g.append('rect')
            .classed('node-shape', true)
            .attr('x', - radius)
            .attr('y', - radius)
            .attr('width', 2 * radius)
            .attr('height', 2 * radius)
            .attr('rx', d => nodeShapeImpl(d) === 'rect' ? 0 : radius)
            .attr('fill', nodeFillNewImpl)
            .lower();

          return g;
        },
        update => {
          update.each(function(d) {
            const g = d3.select(this).datum(d);
            g.transition()
              .duration(500)
              .ease(d3.easeSinIn)
              .attr('opacity', 1);

            if (nodeText !== undefined) {
              let updated = false;

              g.selectChildren<SVGTextElement, N>('text.node-text')
                .datum(d)
                .text(function (d) {
                  const t = nodeText(d);
                  if (oldNodeTexts.get(this) !== t) {
                    oldNodeTexts.set(this, t);
                    updated = true;
                  }
                  return t;
                });
              if (updated) {
                g.selectChildren('.node-shape')
                  .datum(d)
                  .transition()
                  .duration(500)
                  .ease(d3.easeSinIn)
                  .attr('rx', d => nodeShapeImpl(d) === 'rect' ? 0 : radius)
                  .attr('fill', nodeFillNewImpl as any);
              } else {
                g.selectChildren('.node-shape')
                  .datum(d)
                  .transition()
                  .duration(500)
                  .ease(d3.easeSinOut)
                  .attr('rx', d => nodeShapeImpl(d) === 'rect' ? 0 : radius)
                  .attr('fill', nodeFillImpl as any);
              }
            } else {
              g.selectChildren('.node-shape')
                .datum(d)
                .transition()
                .duration(500)
                .ease(d3.easeSinOut)
                .attr('rx', d => nodeShapeImpl(d) === 'rect' ? 0 : radius)
                .attr('fill', nodeFillImpl as any);
            }
          });

          return update;
        },
      )
      .call(drag(simulation) as any);

    linkRef.current = linkRef.current!
      .data(newLinks, d => linkId(d))
      .join(
        enter => {
          if (enter.size() > 0) {
            hasEnteredLink = true;
          }

          const g = enter.append('g')
            .attr('opacity', 0)
            .attr('fill', linkStrokeNewImpl)
            .attr('stroke', linkStrokeNewImpl);

          g.transition()
            .duration(500)
            .ease(d3.easeSinIn)
            .attr('opacity', 1);

          if (linkText !== undefined) {
            g.append('text')
              .each(function(d) {
                oldLinkTexts.set(this, linkText(d));
              })
              .text(linkText)
              .classed('link-text', true)
              .attr('text-anchor', 'middle')
              .clone(true).lower()
              .classed('link-text', false)
              .classed('link-text-shade', true)
              .attr('stroke', 'white')
              .attr('stroke-width', 4);
          }

          g.append('path')
            .attr('fill', 'transparent')
            .attr('stroke-width', 1.5)
            .attr('stroke-linecap', 'round')
            .attr('marker-end', 'url(#arrow-new)')
            .lower();

          return g;
        },
        update => {
          update.each(function (d) {
            const g = d3.select(this).datum(d);
            g.transition()
              .duration(500)
              .ease(d3.easeSinIn)
              .attr('opacity', 1);

            if (linkText !== undefined) {
              let updated = false;

              g.selectChildren<SVGTextElement, N>('text.link-text')
                .datum(d)
                .text(function (d) {
                  const t = linkText(d);
                  if (oldLinkTexts.get(this) !== t) {
                    oldLinkTexts.set(this, t);
                    updated = true;
                  }
                  return t;
                });

              if (updated) {
                g.transition()
                  .duration(500)
                  .ease(d3.easeSinIn)
                  .attr('stroke', linkStrokeNewImpl as any);

                g.select('path')
                  .attr('marker-end', 'url(#arrow-new)');
              } else {
                if (linkActive !== undefined && linkActive(d)) {
                  g.transition()
                    .duration(500)
                    .ease(d3.easeSinIn)
                    .attr('stroke', linkStrokeActiveImpl as any);

                  g.select('path')
                    .attr('marker-end', 'url(#arrow-new)');
                } else {
                  g.transition()
                    .duration(500)
                    .ease(d3.easeSinOut)
                    .attr('stroke', linkStrokeImpl);

                  g.select('path')
                    .attr('marker-end', 'url(#arrow)');
                }
              }
            } else {
              if (linkActive !== undefined && linkActive(d)) {
                g.transition()
                  .duration(500)
                  .ease(d3.easeSinOut)
                  .attr('stroke', linkStrokeActiveImpl as any);

                g.select('path')
                  .attr('marker-end', 'url(#arrow-new)');
              } else {
                g.transition()
                  .duration(500)
                  .ease(d3.easeSinOut)
                  .attr('stroke', linkStrokeImpl);

                g.select('path')
                  .attr('marker-end', 'url(#arrow)');
              }
            }
          });

          return update;
        },
      );

    simulation.nodes(newNodes);
    (simulation.force('link') as d3.ForceLink<N, L>)
      .links(newLinks);
    if (hasEnteredNode || hasEnteredLink) {
      simulation.alpha(1).restart();
    } else {
      simulation.alpha(0);
    }
  }, /* eslint-disable-line react-hooks/exhaustive-deps */ [ref, nodes, links, ...deps]);
}

function radian(ux: number, uy: number, vx: number, vy: number) {
  var dot = ux * vx + uy * vy;
  var mod = Math.sqrt((ux * ux + uy * uy) * (vx * vx + vy * vy));
  var rad = Math.acos(dot / mod);
  if (ux * vy - uy * vx < 0.0) {
    rad = -rad;
  }
  return rad;
}

function svgArcToCenterParam(x1: number, y1: number, rx: number, ry: number, phi: number, fA: boolean, fS: boolean, x2: number, y2: number) {
  var cx, cy, startAngle, deltaAngle, endAngle;
  var PIx2 = Math.PI * 2.0;

  if (rx < 0) {
    rx = -rx;
  }
  if (ry < 0) {
    ry = -ry;
  }
  if (rx === 0.0 || ry === 0.0) {
    throw Error('rx and ry can not be 0');
  }

  var s_phi = Math.sin(phi);
  var c_phi = Math.cos(phi);
  var hd_x = (x1 - x2) / 2.0;
  var hd_y = (y1 - y2) / 2.0;
  var hs_x = (x1 + x2) / 2.0;
  var hs_y = (y1 + y2) / 2.0;

  var x1_ = c_phi * hd_x + s_phi * hd_y;
  var y1_ = c_phi * hd_y - s_phi * hd_x;

  var lambda = (x1_ * x1_) / (rx * rx) + (y1_ * y1_) / (ry * ry);
  if (lambda > 1) {
    rx = rx * Math.sqrt(lambda);
    ry = ry * Math.sqrt(lambda);
  }

  var rxry = rx * ry;
  var rxy1_ = rx * y1_;
  var ryx1_ = ry * x1_;
  var sum_of_sq = rxy1_ * rxy1_ + ryx1_ * ryx1_;
  if (!sum_of_sq) {
    throw Error('start point can not be same as end point');
  }
  var coe = Math.sqrt(Math.abs((rxry * rxry - sum_of_sq) / sum_of_sq));
  if (fA === fS) { coe = -coe; }

  var cx_ = coe * rxy1_ / ry;
  var cy_ = -coe * ryx1_ / rx;

  cx = c_phi * cx_ - s_phi * cy_ + hs_x;
  cy = s_phi * cx_ + c_phi * cy_ + hs_y;

  var xcr1 = (x1_ - cx_) / rx;
  var xcr2 = (x1_ + cx_) / rx;
  var ycr1 = (y1_ - cy_) / ry;
  var ycr2 = (y1_ + cy_) / ry;

  startAngle = radian(1.0, 0.0, xcr1, ycr1);

  deltaAngle = radian(xcr1, ycr1, -xcr2, -ycr2);
  while (deltaAngle > PIx2) { deltaAngle -= PIx2; }
  while (deltaAngle < 0.0) { deltaAngle += PIx2; }
  if (fS === false) { deltaAngle -= PIx2; }
  endAngle = startAngle + deltaAngle;
  while (endAngle > PIx2) { endAngle -= PIx2; }
  while (endAngle < 0.0) { endAngle += PIx2; }

  var outputObj = {
    cx: cx,
    cy: cy,
    startAngle: startAngle,
    deltaAngle: deltaAngle,
    endAngle: endAngle,
    clockwise: (fS === true)
  }

  return outputObj;
}
