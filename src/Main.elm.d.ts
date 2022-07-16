export const Elm: ElmType;

export type ElmType = {
  Main: {
    init: (config: { node: HTMLElement; flags: {} }) => MainInstance;
  };
};

export type MainInstance = {
  ports: {
    saveToLocalStorage: {
      subscribe: (callback: (userMappings: any) => void) => void;
    };
    onBlob: {
      send: (blob: any) => void;
    };
  };
};
