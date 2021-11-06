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
    expressedFormula: FormulaLike<Expressed>;
    conversionTable: SatyrosConversionTable;
    assignment: SatyrosAssignmentAPI;
    step(callback: (r?: boolean) => void): void;
  }

  interface SatyrosConversionTable {
    variableToExpressed: [[number, Expressed]];
    expressedToLiteral: [[Expressed, Literal]];
  }

  interface SatyrosAssignmentAPI {
    getValue(x: number, callback: (a: [boolean, Clause]) => void): void;
    setValue(x: number, v: [boolean, Clause]): void;
  }

  interface Window {
    makeSatyrosAPI: (f: FormulaLike<Expressible>) => SatyrosAPI;
  }
}
