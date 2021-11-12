import React from 'react';

const Section3: React.FunctionComponent = () => {
  return (
    <section className='Section Section3'>
      <h2>You{'\''}ve got a plan, a plan to mix them all</h2>
      <p><span className='initial'>N</span>ow the question is this: how can we allow QFIDL solver to work with a disjunction of contraints? In other words, how can we combine a SAT solver (boolean solver) and a QFIDL solver into a single solver? There are two simple ways to achieve this integration:</p>
      <ul>
        <li>Pick "true" constraints by SAT solver first, and apply QFIDL solver on those constraints, until QFIDL solver finds a solution or SAT solver exhausts the possibilities</li>
        <li>While executing SAT solver, use QFIDL as a conflict-finding algorithm.</li>
      </ul>
      <p>We will show these two approaches in turn, and describe how they are different.</p>
    </section>
  );
};

export default Section3;
