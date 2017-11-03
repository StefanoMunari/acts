const path = require('path');
const webpack = require('webpack');
const root = __dirname;
const depsPath = path.join(root, 'deps');
const nodeModulesPath = path.join(root, 'node_modules');
const privStaticPath = path.resolve(root, 'priv', 'static');
const buildPath = path.join(privStaticPath, 'bundles');
const mainPath = path.resolve('web', 'modules', 'main');
const mainEntryFile = path.join(mainPath, 'main.ts');

module.exports = {
  entry: {
    main: mainEntryFile
  },
  output: {
    path: buildPath,
    filename: '[name].js'
  },
  devtool: 'source-map',
  module: {
    loaders: [
      { test: /.(png|woff(2)?|eot|ttf|svg)(\?[a-z0-9=\.]+)?$/, loader: 'url-loader?limit=100000' },
      { test: /\.html$/, loaders: ['raw-loader'] },
      { test: /\.css$/, loaders: ['style-loader', 'css-loader', 'resolve-url-loader'] },
      { test: /\.scss$/, loaders: ['style-loader', 'css-loader', 'resolve-url-loader', 'sass-loader?sourceMap'], exclude: [mainPath] },
      { test: /\.scss$/, loaders: ['css-to-string-loader', 'css-loader', 'resolve-url-loader', 'sass-loader?sourceMap'], include: [mainPath] },
      { test: /\.ts$/, loader: 'awesome-typescript-loader' }
    ]
  },
  resolve: {
    extensions: ['.ts', '.js'],
    alias: {}
  },

  // https://github.com/AngularClass/angular2-webpack-starter/issues/993
  plugins: [new webpack.ContextReplacementPlugin(
      /angular(\\|\/)core(\\|\/)@angular/,
      __dirname
    )]
};
