const path = require("path")
const { CheckerPlugin } = require("awesome-typescript-loader")
const HtmlWebpackPlugin = require("html-webpack-plugin")
const CopyPlugin = require("copy-webpack-plugin");
const HtmlWebpackTagsPlugin = require('html-webpack-tags-plugin');

module.exports = {
  devtool: "source-map",
  entry: "./src/index.tsx",
  /*
  externals: {
    "react": "React"
  },
  */
  mode: "development",
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        use: [
          {
            loader: "awesome-typescript-loader"
          }
        ]
      },
      {
        enforce: "pre",
        test: /\.js$/,
        use: [
          {
            loader: "source-map-loader"
          }
        ]
      }
    ]
  },
  output: {
    path: path.resolve(__dirname, "dist"),
    filename: "static/app.js",
    publicPath: "/"
  },
  plugins: [
    new CheckerPlugin(),
    new HtmlWebpackPlugin(
      { title: "Dayta"
      }
    ),
    new CopyPlugin({patterns: [{ from: "src/index.css", to: "static/index.css" }]}),
    new HtmlWebpackTagsPlugin({ tags: "static/index.css", append: true })
  ],
  resolve: {
    extensions: [".ts", ".tsx", ".js", ".jsx"]
  }
}
