export { };

declare global {
  type Operator = '::<?' | '::<=?' | '::>?' | '::>=?' | '::=?' | '::<>?';
  type Expressible = [number, Operator, number] | [number, number, Operator, number];
  type Expressed = [number, number, number];

  type Literal = number;
  type Clause = ClauseLike<Literal>;
  type Formula = FormulaLike<Literal>;

  type ClauseLike<T> = T[];
  type FormulaLike<T> = ClauseLike<T>[];

  interface SatyrosAPI {
    readonly expressedFormula: FormulaLike<Expressed>;
    readonly conversionTable: SatyrosConversionTable;
    readonly getFormula(callback: (f: Formula) => void): void;
    readonly getImplicationGraph(callback: (vs: SatyrosImplicationVertex[], es: SatyrosImplicationEdge[]) => void): void;
    readonly getBellmanFordGraph(callback: (vs: SatyrosBellmanFordVertex[], es: SatyrosBellmanFordEdge[]) => void): void;
    readonly assignment: SatyrosAssignmentAPI;
    readonly step(callback: SatyrosEffectCallback): void;
    readonly undo(callback: SatyrosEffectCallback): void;
    readonly reset(): void;
  }

  interface SatyrosEffectCallback {
    Start(repeated: boolean): void;
    NegativeCyclePass(): void;
    NegativeCycleFind(): void;
    NegativeCycleCheck(): void;
    PropagationEnd(): void;
    PropagationNth(): void;
    PropagationFindShorter(): void;
    PropagationCheck(): void;
    BacktraceComplete(): void;
    BacktraceExhaustion(): void;
    DecisionComplete(): void;
    DecisionResult(): void;
    BCPConflictDrivenClause(): void;
    BCPConflict(): void;
    BCPComplete(): void;
    BCPUnitClause(): void;
    Finish(result: boolean): void
  }

  interface SatyrosImplicationVertex {
    readonly variable: number;
    readonly value: boolean;
    readonly isDecision: boolean;
    readonly level: number;
  }

  interface SatyrosImplicationEdge {
    readonly startVertex: number;
    readonly endVertex: number;
    readonly clauseIndex: number;
    readonly levelDiff: number;
  }

  interface SatyrosBellmanFordVertex {
    readonly variable: number | null;
    readonly distance: number;
  }

  interface SatyrosBellmanFordEdge {
    readonly startVertex: number | null;
    readonly endVertex: number | null;
    readonly weight: number;
    readonly lastActive: boolean;
  }

  interface SatyrosConversionTable {
    readonly variableToExpressed: [number, Expressed][];
    readonly expressedToLiteral: [Expressed, Literal][];
  }

  interface SatyrosAssignmentAPI {
    readonly getValue(x: number, callback: (a: [boolean, Clause] | null) => void): void;
    readonly getValueOfClause(i: number, callback: (v: boolean | null) => void): void;
    readonly getValueOfFormula(callback: (v: boolean | null) => void): void;
  }

  interface Window {
    readonly makeSatyrosAPI: (f: FormulaLike<Expressible>) => SatyrosAPI;
  }
}
