import React from 'react';

import Example4 from './Example4';
import Example5 from './Example5';
import Example6 from './Example6';
import Example7 from './Example7';
import Example8 from './Example8';
import Example9 from './Example9';

const Section3: React.FunctionComponent = () => {
  return (
    <section className='Section Section3'>
      <h2>You{'\''}ve got a plan, a plan to mix them all</h2>
      <p><span className='initial'>N</span>ow the question is this: how can we allow QFIDL solver to work with a disjunction of contraints? In other words, how can we combine a SAT solver (boolean solver) and a QFIDL solver into a single solver? There are two simple ways to achieve this integration:</p>
      <ul>
        <li>Pick "true" constraints by SAT solver first, and apply QFIDL solver on those constraints, until QFIDL solver finds a solution or SAT solver exhausts the possibilities</li>
        <li>While executing SAT solver, use QFIDL as a conflict-finding algorithm.</li>
      </ul>
      <p className='indent'>We will show these two approaches in turn, and describe how they are different with the following <a href='https://en.wikipedia.org/wiki/Job-shop_scheduling'>job-shop scheduling problem</a>:</p>
      <blockquote>
        We have two manufacturing machines <var>A</var> and <var>B</var>, each of which can perform one task at a time.<br/>
        Also, there are three people <var>X</var>, <var>Y</var>, and <var>Z</var> want to make their products, and each product requires lanes of machines for different time periods.<br/>
        <ul>
          <li>The product of <var>X</var> requires <var>A</var> for 5 hours and <var>B</var> for 2 hours.</li>
          <li>The product of <var>Y</var> requires <var>B</var> for 1 hours and <var>A</var> for 4 hours.</li>
          <li>The product of <var>Z</var> requires <var>A</var> for 4 hours.</li>
        </ul>
        Is there a time schedule in which all of them can get their product within 13 hours?
      </blockquote>
      <p>We need to encode this into a QFIDL SMT problem first. Since there are 5 required tasks and each needs a variable for the task start time, we will use 5 variables:</p>
      <blockquote className='math'>
        <var>X</var>{'\''}s <var>A</var>: starts at <var>x<sub>1</sub></var>
        <br />
        <var>X</var>{'\''}s <var>B</var>: starts at <var>x<sub>2</sub></var>
        <br />
        <var>Y</var>{'\''}s <var>B</var>: starts at <var>x<sub>3</sub></var>
        <br />
        <var>Y</var>{'\''}s <var>A</var>: starts at <var>x<sub>4</sub></var>
        <br />
        <var>Z</var>{'\''}s <var>A</var>: starts at <var>x<sub>5</sub></var>
      </blockquote>
      <p>Each task should start after schedule begins and finish before the deadline.</p>
      <blockquote className='math'>
        <var>x<sub>1</sub></var> ≥ 0 ∧ <var>x<sub>1</sub></var> ≤ 13 - 5 = {13 - 5}
        <br />
        <var>x<sub>2</sub></var> ≥ 0 ∧ <var>x<sub>2</sub></var> ≤ 13 - 2 = {13 - 2}
        <br />
        <var>x<sub>3</sub></var> ≥ 0 ∧ <var>x<sub>3</sub></var> ≤ 13 - 1 = {13 - 1}
        <br />
        <var>x<sub>4</sub></var> ≥ 0 ∧ <var>x<sub>4</sub></var> ≤ 13 - 4 = {13 - 4}
        <br />
        <var>x<sub>5</sub></var> ≥ 0 ∧ <var>x<sub>5</sub></var> ≤ 13 - 4 = {13 - 4}
      </blockquote>
      <p>Now, we need to encode that no machine can execute two task at once. For any task using machine <var>A</var>, no task using <var>A</var> is not overwrapped with the task, and likewise for the tasks using machine <var>B</var>.</p>
      <blockquote className='math'>
        <var>x<sub>1</sub></var> - <var>x<sub>4</sub></var> ≥ 4 ∨ <var>x<sub>4</sub></var> - <var>x<sub>1</sub></var> ≥ 5
        <br />
        <var>x<sub>4</sub></var> - <var>x<sub>5</sub></var> ≥ 4 ∨ <var>x<sub>5</sub></var> - <var>x<sub>4</sub></var> ≥ 4
        <br />
        <var>x<sub>5</sub></var> - <var>x<sub>1</sub></var> ≥ 5 ∨ <var>x<sub>1</sub></var> - <var>x<sub>5</sub></var> ≥ 4
        <br />
        <var>x<sub>2</sub></var> - <var>x<sub>3</sub></var> ≥ 1 ∨ <var>x<sub>3</sub></var> - <var>x<sub>2</sub></var> ≥ 2
      </blockquote>
      <p>We don{'\''}t need any condition for <var>B</var>, as there are only 2 tasks using <var>B</var>. If we apply the above mentioned transformation to these contraints, we get</p>
      <blockquote className='math'>
        <Example4 />
      </blockquote>
      <p>In order to pass this QFIDL/SAT mixture to a SAT solver, we need to hide QFIDL details:</p>
      <Example5 />
      <p>These boolean variables represents single QFIDL constraint. The conversion table is like this (you can click each boolean variable names to check which boolean value is mapped to which QFIDL expression):</p>
      <Example6 />
      <p className='indent'>However, as all values for <var>b9</var> to <var>b18</var> are already chosen as <var>c4</var> to <var>c13</var> are all unit clause, we need to deal with only</p>
      <Example7 />
      <p>Now, let{'\''}s solve this problem with each approach!</p>
      <section className='Subsection Subsection3-1'>
        <h3>Naive approach: one after the other</h3>
        <p>Let{'\''}s first solve SAT problem for the above CNF using SAT solver, and translate the result assignment into QFIDL contraints, and then solve them with QFIDL solver:</p>
        <Example8 />
        <p>This approach requires two backtrackings. How about the more clever approach?</p>
      </section>
      <section className='Subsection Subsection3-1'>
        <h3>More clever approach: one into the other</h3>
        <p>We start with the SAT solver as before. However, after propagating boolean constraints for a single decision level, we immediately executes QFIDL solver with that partial assignment to check whether the decision is acceptable from the QFIDL{'\''}s viewpoint.</p>
        <Example9 />
      <p>This approach also requires two backtracks. Does that mean these two approaches are equivalent? No! note that each backtrace is more local (since a conflict is at the same decision level) and thus less expensive in terms of backtracking cost. For bigger example with more variables, this cost different becomes more eminent and makes this approach more preferable.</p>
      </section>
    </section>
  );
};

export default Section3;
