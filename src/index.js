import "elm-canvas";

import { Elm } from "./Main.elm";

Elm.Main.init({
  node: document.getElementById("main"),
  flags: {
    devMode: import.meta.env.DEV,
  },
});
