const path = require("path")
const { CheckerPlugin } = require("awesome-typescript-loader")
const HtmlWebpackPlugin = require("html-webpack-plugin")

module.exports = {
  devtool: "source-map",
  entry: "./src/index.tsx",
  externals: {
    "react": "React"
  },
  module: {
    loaders: [
      {
        test: /\.tsx?$/,
        loader: "awesome-typescript-loader"
      },
      {
        enforce: "pre",
        test: /\.js$/,
        loader: "source-map-loader"
      }
    ]
  },
  output: {
    path: path.resolve(__dirname, "dist"),
    filename: "app.js"
  },
  plugins: [
    new CheckerPlugin(),
    new HtmlWebpackPlugin()
  ],
  resolve: {
    extensions: [".ts", ".tsx", ".js", ".jsx"]
  }
}
