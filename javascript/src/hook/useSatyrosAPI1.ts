import React from "react";

export const useSatyrosAPI1 = (f : FormulaLike<Expressible>) => {
  const [satyrosAPI1, setSatyrosAPI1] = React.useState<SatyrosAPI | undefined>(undefined);

  React.useEffect(
    () => {
      setTimeout(() => {
        setSatyrosAPI1(window.makeSatyrosAPI1(f));
      }, 500);
    },
    // eslint-disable-next-line
    []
  );

  return satyrosAPI1;
};
