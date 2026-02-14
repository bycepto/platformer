const esbuild = require("esbuild");
const elmPlugin = require("esbuild-plugin-elm");
const postCssPlugin = require("esbuild-style-plugin");

const args = process.argv.slice(2);
const watch = args.includes("--watch");
const deploy = args.includes("--deploy");

const loader = {
  ".svg": "file",
  ".png": "file",
  ".jpg": "file",
};

const plugins = [
  elmPlugin({
    debug: !deploy && watch,
    optimize: deploy,
  }),
  postCssPlugin({
    postcss: {
      plugins: [require("@tailwindcss/postcss"), require("autoprefixer")],
    },
  }),
];

let opts = {
  entryPoints: [
    "src/main.ts",
    "src/css.css",
    "src/images/*.svg",
    "src/images/*.png",
    "src/images/*.jpg",
  ],
  bundle: true,
  logLevel: "info",
  target: "es2017",
  outdir: "public/dist",
  external: ["*.css", "*.svg", "*.png", "*.jpg"],
  loader: loader,
  plugins: plugins,
};

if (deploy) {
  opts = {
    ...opts,
    minify: true,
    define: {
      "process.env.MODE": '"production"',
    },
  };
}

if (watch) {
  opts = {
    ...opts,
    sourcemap: "inline",
    define: {
      "process.env.MODE": '"development"',
    },
  };
  esbuild
    .context(opts)
    .then((ctx) => {
      ctx.watch();
      ctx.serve({ port: 3002, servedir: "./public" });
    })
    .catch((_error) => {
      process.exit(1);
    });
} else {
  esbuild.build(opts);
}
