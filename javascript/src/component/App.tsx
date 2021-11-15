import React from 'react';

import Section1 from './Section1';
import Section2 from './Section2';
import Section3 from './Section3';
import Section4 from './Section4';
import References from './References';

import './App.css';

const App: React.FunctionComponent = () => {
  return (
    <article className='App' lang='en'>
      <header>
        <h1>Boolean meets Theories: SMT Solvers</h1>
        <h3 style={{ marginTop: 0, marginBottom: 0, textAlign: 'right' }}>Junyoung "Clare" Jang</h3>
        <h4 style={{ marginTop: 0, textAlign: 'right' }}>2021/11/15</h4>
      </header>
      <hr />
      <body>
        <Section1 />
        <hr />
        <Section2 />
        <hr />
        <Section3 />
        <hr />
        <Section4 />
      </body>
      <hr />
      <footer>
        <References />
      </footer>
    </article>
  );
};

export default App;
