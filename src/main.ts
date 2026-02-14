import "elm-canvas";

import { Elm } from "./Main.elm";

const DEV_MODE = process.env.MODE == "development";

Elm.Main.init({
  node: document.getElementById("main"),
  flags: {
    devMode: DEV_MODE,
  },
});
