import React from 'react';

const Section1: React.FunctionComponent = () => {
  return (
    <section className='Section Section1'>
      <h2>Prelude: Boolean Constraints and Their Solvers</h2>
      <p><a href='https://en.wikipedia.org/wiki/Boolean_satisfiability_problem'><span className='initial'>S</span>atifiability of boolean constraints</a> (often referred as "SAT") is a central topic of computational complexity. It is NP-complete, so it can encode any nondeterministic polynomial-time problems with at most polynomially larger cost, which includes integer factorization, <a href='https://en.wikipedia.org/wiki/Subset_sum_problem'>subset sum problem</a>, <a href='https://en.wikipedia.org/wiki/Travelling_salesman_problem'>travelling sales man problem</a>, <a href='https://en.wikipedia.org/wiki/Sudoku'>Sudoku</a>, and so on. These nondeterministic polynomial-time problems are often found in more realistic applications, for example, Automated Deduction, Computer-Aided Design (CAD), and Artificial Intelligence. With its importance, SAT solving algorithm has been discussed for a long time since Davis and Putnam{ '\'' }s work on 1960<a className='footnote' href='#ref-1'>1</a>. Modern SAT solvers, including <a href='http://minisat.se/'>MiniSAT</a> and <a href='https://www.labri.fr/perso/lsimon/glucose/'>Glucose</a> show incredible performance even on large problems with millions of boolean variables on many industrial uses, and there are still researches going on to improve the performance even further.</p>
      <section className='Subsection Subsection1-1'>
      <h3>What SAT Solver Actually Solves?</h3>
      <p>The question of interest is like the following:
      </p>
      <blockquote>
        Suppose that we have boolean variables: <var>b<sub>1</sub></var>, <var>b<sub>2</sub></var>, … , <var>b<sub>n</sub></var>, and their constraints obtained by applying logical and (logical conjunction, ∧), logical or (logical disjunction, ∨), and logical negation (¬) operations on them. Will there be an assignment for those boolean variables so that they satisfy all the given constraints?
      </blockquote>
      <p>
        SAT solvers can answer whether there is such an assignment or not, and actually construct the assignment if it exists.
      </p>
      <section className='side-node'>
        <h4>Side note:</h4>
        <p className='indent'>
          In many SAT solvers, the requirements on constraints are often more strict: The conjunction of all constraints should be in the <a href='https://en.wikipedia.org/wiki/Conjunctive_normal_form'>Conjunctive Normal Form (CNF)</a>. For example,
        </p>
        <blockquote>
          (<var>b<sub>1</sub></var> ∨ <var>b<sub>2</sub></var>) ∧ <var>b<sub>3</sub></var>
          <br />
          <var>b<sub>2</sub></var> ∧ (¬<var>b<sub>3</sub></var> ∨ <var>b<sub>4</sub></var>)
        </blockquote>
        <p>is a valid set of constraints, but</p>
        <blockquote>
          <var>b<sub>1</sub></var> ∨ (<var>b<sub>2</sub></var> ∧ <var>b<sub>3</sub></var>)
          <br />
          ¬(<var>b<sub>2</sub></var> ∨ (<var>b<sub>3</sub></var> ∧ <var>b<sub>4</sub></var>))
        </blockquote>
        <p>
          is not because there is a disjunction (∨) that contains a conjunction (∧) and a negation (¬) is applied on a non-variable expression. However, there is <a href='https://en.wikipedia.org/wiki/Tseytin_transformation'>an efficient translation between non-CNF constraints and CNF constraints</a>, so this is more about how one encodes their problems.
        </p>
        </section>
      </section>
      <section className='Subsection Subsection1-2'>
      <h3>How SAT Solver Solves SAT Problems Efficiently?</h3>
      One of the important algorithm that allows SAT solvers to "learn" from its trial-and-error is <a href='https://en.wikipedia.org/wiki/Conflict-driven_clause_learning'>Conflict Deriven Clause Learning (CDCL)</a>. <a href='https://cse442-17f.github.io/Conflict-Driven-Clause-Learning/'>This interactive tutorial</a> explains what CDCL is and how SAT solvers use them. (This article is also deeply inspired from the tutorial!) In this article, we will not use any complex optimization other than CDCL, but modern SAT solvers employ many other optimizations, including 2 watched literals<a className='footnote' href='#ref-2'>2</a>, Variable State Independent Decaying Sum<a className='footnote' href='#ref-2'>2</a>, and Restart<a className='footnote' href='#ref-3'>3</a>.
      </section>
    </section>
  );
};

export default Section1;
