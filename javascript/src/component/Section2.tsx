import React from 'react';

import Example1 from './Example1';
import Example2 from './Example2';
import Example3 from './Example3';
import Preprocess from './Preprocess';

const Section2: React.FunctionComponent = () => {
  return (
    <section className='Section Section2'>
      <h2>Your Focus Determines Your Reality… or Your Solvers</h2>
      <p><span className='initial'>I</span>t is often tedious to encode a problem entirely in boolean logic, and sometimes there is even a domain-specific solvers that is much more efficient than SAT solvers for some parts of your problem, in which case paying SAT encoding cost and introducing NP-complete procedure for the part can slow down the solution search process significantly. This is why many applications of SAT solver often uses Satisfiability "Modulo Theory" (referred as "SMT" below) solvers instead. In this article, we introduce a Quantifier-free integer difference logic ("QFIDL") and its solver shortly, and describes the details of the integration of such "external" solvers with SAT solvers.</p>
      <section className='Subsection Subsection2-1'>
        <h3>Quantifier Free Integer Difference Logic (QFIDL)</h3>
        <p>
          QFIDL is a system that can solves the following question:
        </p>
        <blockquote>
          Suppose that we have integer variables: <var>x<sub>1</sub></var>, <var>x<sub>2</sub></var>, …, <var>x<sub>n</sub></var> and their constraints in a form of <var>x<sub>i</sub></var> - <var>x<sub>j</sub></var> <var>{'['}op{']'}</var> <var>c</var> or <var>x<sub>i</sub></var> <var>{'['}op{']'}</var> <var>c</var>, where <var>c</var> is a real number constant and <var>{'['}op{']'}</var> is one of <var>{'<'}</var>, <var>{'≤'}</var>, <var>{'>'}</var>, <var>{'≥'}</var>. Will there be an assignment for these integer variables so that they satisfies all the given constraints?
        </blockquote>
        <p className='indent'>
          Actually, we can encode these constraints in an even simpler form:
        </p>
        <blockquote>
          Suppose that we have integer variables: <var>x<sub>1</sub></var>, <var>x<sub>2</sub></var>, …, <var>x<sub>n</sub></var> and their constraints in a form of <var>x<sub>i</sub></var> - <var>x<sub>j</sub></var> <var>≤</var> <var>c</var>, where <var>c</var> is an integer number constant. Will there be an assignment for these integer variables so that they satisfies all the given constraints?
        </blockquote>
        <p>
          Here, we removed the constraints only with one variable, restrict <var>c</var> to be an integer, and limits the operator to the "less than or equal to" operator. This form is what QFIDL really solves internally.
        </p>
        <p className='indent'>
          How this simplification is possible? Let{'\''}s check it in its graphical form:
        </p>
        <Preprocess underSAT={false} />
        <p>
          As you may have noticed, the area that covered by simplified form is not exactly the same. However, since the variables are actually <em>integer</em> variables, we need to focus on integer lattice points covered by those areas, which faithfully agree with each other.
        </p>
        <p className='indent'>
          One thing worth noting that is how we removed one variable constraints. We introduce an auxiliary variable <var>z</var> representing 0, make one variable constraints to two variable constraints with <var>z</var>. We can add this auxiliary variable as (simple version of) QFIDL always works on a difference of two variables. Thus, once we have an satisfying assignment for integer variables, we can add or substract the same amount from all variables to get another satisfying assignment. By adjusting the assigned value for <var>z</var> to be 0, we can derive a solution for "complex" form of constraints.
        </p>
        <p className='indent'>
          When we use QFIDL alongside SAT solver, we have conjunctions and disjunctions. This allow us to encode more operators. In the above problem, we can change the list of <var>{'['}op{']'}</var> into:
        </p>
        <blockquote>
          … <var>{'['}op{']'}</var> is one of <var>{'<'}</var>, <var>{'≤'}</var>, <var>{'>'}</var>, <var>{'≥'}</var>, <strong><var>{'='}</var></strong>, <strong><var>{'≠'}</var></strong>. …
        </blockquote>
        <p>
          The simplification of these new operators can also be depicted as a graph:
        </p>
        <Preprocess underSAT={true} />
      </section>
      <section className='Subsection Subsection2-2'>
        <h3>QFIDL Solver</h3>
        <p>
          How can we solve QFIDL problems? Suppose that we have
        </p>
        <blockquote className='math'>
          <var>x<sub>1</sub></var> - <var>x<sub>2</sub></var> ≤ 4
          <br />
          <var>x<sub>2</sub></var> - <var>x<sub>3</sub></var> ≤ 5
          <br />
          <var>x<sub>3</sub></var> - <var>x<sub>1</sub></var> ≤ -10
        </blockquote>
        <p>
          We can easily show that there is no satisfying assignment for <var>x<sub>1</sub></var>, <var>x<sub>2</sub></var>, and <var>x<sub>3</sub></var>:
        </p>
        <blockquote className='math'>
          <var>x<sub>1</sub></var> ≤ <var>x<sub>2</sub></var> + 4 ≤ <var>x<sub>3</sub></var> + 9 ≤ <var>x<sub>1</sub></var> - 1
        </blockquote>
        <p>
          i.e., the integer <var>x<sub>1</sub></var> should be smaller than its predecessor, and it is impossible. This procedure can be generalized to any QFIDL problems. We first construct a graph:
        </p>
        <Example1 />
        <p>
          Here, each node represents a variable, and the special <var>min</var> node represents the minimum variable among all. Each edge represents a difference inequality between two end nodes. For example, when there is an inequality <var>x<sub>1</sub></var> - <var>x<sub>2</sub></var> ≤ 4, we put a edge from the node <var>x<sub>1</sub></var> to the node <var>x<sub>1</sub></var> with the weight of 4.
        </p>
        <p className='indent'>
          Now, we will find shortest path to each node from <var>min</var> node. We used <a href='https://en.wikipedia.org/wiki/Bellman%E2%80%93Ford_algorithm'>Bellman-Ford Algorithm</a> to find the shortest path.
        </p>
        <Example2 />
        <p>
          After traversing all edges twice, the algorithm finds that there is a negative cycle: <var>x<sub>1</sub></var>-<var>x<sub>2</sub></var>-<var>x<sub>3</sub></var>-<var>x<sub>1</sub></var>. Note that this negative cycle is exactly the sequence we saw in this unsatisfiabilty proof:
        </p>
        <blockquote className='math'>
          <var>x<sub>1</sub></var> ≤ <var>x<sub>2</sub></var> + 4 ≤ <var>x<sub>3</sub></var> + 9 ≤ <var>x<sub>1</sub></var> - 1
        </blockquote>
        <p>
          This is because a negative cycle of the graph encoded in the above-mentioned way appears only when there is such an unsatisfiabilty proof, or rather, it is a direct encoding of an unsatisfiabilty proof. Moreover, this graph method gives a concrete satisfying assignment if there is no negative cycle. Suppose that <var>x<sub>1</sub></var> and <var>x<sub>2</sub></var> have the following constraint:
        </p>
        <blockquote className='math'>
          <var>x<sub>1</sub></var> - <var>x<sub>2</sub></var> ≤ <var>c</var>
        </blockquote>
        <p>
          If <var>d<sub>1</sub></var> and <var>d<sub>2</sub></var> are the shortest path distance to <var>x<sub>1</sub></var> and <var>x<sub>2</sub></var> from <var>min</var>, they should satisfy
        </p>
        <blockquote className='math'>
          - <var>d<sub>1</sub></var> + <var>d<sub>2</sub></var> ≤ <var>c</var>
        </blockquote>
        <p>
          Otherwise, <var>d<sub>1</sub></var> + <var>c</var> is smaller than <var>d<sub>2</sub></var>, and <var>d<sub>2</sub></var> is not the shortest path distance to <var>x<sub>2</sub></var>. In other words, <var>x<sub>1</sub></var> = - <var>d<sub>1</sub></var> and <var>x<sub>2</sub></var> = - <var>d<sub>2</sub></var> should be a satisfying assignment if they are indeed the shortest path distances from <var>min</var>. Here is a satisfiable example:
        </p>
        <blockquote className='math'>
          <var>x<sub>1</sub></var> - <var>x<sub>4</sub></var> ≤ 2
          <br />
          <var>x<sub>1</sub></var> - <var>x<sub>2</sub></var> ≤ 4
          <br />
          <var>x<sub>2</sub></var> - <var>x<sub>1</sub></var> ≤ -4
          <br />
          <var>x<sub>2</sub></var> - <var>x<sub>3</sub></var> ≤ 5
          <br />
          <var>x<sub>3</sub></var> - <var>x<sub>4</sub></var> ≤ -1
        </blockquote>
        <Example3 />
        <p>
          As this example gives the distance -4, 0, 0, -2, the satisfying assignment we can derive from the graph is <var>x<sub>1</sub></var> = 4, <var>x<sub>2</sub></var> = 0, <var>x<sub>3</sub></var> = 0, and <var>x<sub>4</sub></var> = 2. We can check that all the above conditions are met:
        </p>
        <blockquote className='math'>
          <var>x<sub>1</sub></var> - <var>x<sub>4</sub></var> = 4 - 2 ≤ 2
          <br />
          <var>x<sub>1</sub></var> - <var>x<sub>2</sub></var> = 4 - 0 ≤ 4
          <br />
          <var>x<sub>2</sub></var> - <var>x<sub>1</sub></var> = 0 - 4 ≤ -4
          <br />
          <var>x<sub>2</sub></var> - <var>x<sub>3</sub></var> = 0 - 0 ≤ 5
          <br />
          <var>x<sub>3</sub></var> - <var>x<sub>4</sub></var> = 0 - 2 ≤ -1
        </blockquote>
      </section>
    </section>
  );
};

export default Section2;
