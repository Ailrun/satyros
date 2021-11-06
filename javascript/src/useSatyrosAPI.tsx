import React from "react";

export const useSatyrosAPI = (f : FormulaLike<Expressible>) => {
  const [satyrosAPI, setSatyrosAPI] = React.useState<SatyrosAPI | undefined>(undefined);

  React.useEffect(
    () => {
      setTimeout(() => {
        console.log(window.makeSatyrosAPI);
        setSatyrosAPI(window.makeSatyrosAPI(f));
      }, 500);
    },
    // eslint-disable-next-line
    []
  );

  return satyrosAPI;
};
