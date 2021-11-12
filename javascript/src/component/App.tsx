import React from 'react';

import Section1 from './Section1';
import Section2 from './Section2';
import Section3 from './Section3';
import References from './References';

import './App.css';

const App: React.FunctionComponent = () => {
  return (
    <article className='App' lang='en'>
      <h1>Boolean meets Theories: SMT Solvers</h1>
      <hr />
      <Section1 />
      <hr />
      <Section2 />
      <hr />
      <Section3 />
      <hr />
      <References />
    </article>
  );
};

export default App;
