import React from 'react';

const Section4: React.FunctionComponent = () => {
  return (
    <section className='Section Section4'>
      <h2>To the Infinity, and Beyond!</h2>
      <p><span className='initial'>I</span>n this article we demonstrated a SMT solver with a simple theory solver (QFIDL solver), and described how it works. However, there{'\''}s infinite possibilities to explore, research, and improve. There are many other complex yet interesting solvers out there, including Non-linear Integer Arithmetic (NIA) which is even undecidable<a className='footnote' href='#ref-4'>4</a>. Integration of multiple theory solvers into a single solver is also an interesting topic, and SMT{'\''}s applications including formal verification keep expanding. It is now your choice to go beyond the infinity.</p>
    </section>
  );
};

export default Section4;
