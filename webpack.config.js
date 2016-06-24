var path              = require( 'path' );
var webpack           = require( 'webpack' );
var merge             = require( 'webpack-merge' );
var autoprefixer      = require( 'autoprefixer' );
var ExtractTextPlugin = require( 'extract-text-webpack-plugin' );
var CopyWebpackPlugin = require( 'copy-webpack-plugin' );

// detemine build env
var TARGET_ENV = process.env.npm_lifecycle_event === 'build' ? 'production' : 'development';

// common webpack config
var commonConfig = {

  output: {
    path:       path.resolve( __dirname, 'dist/js/' ),
    filename: 'app.bundle.js',
  },

  resolve: {
    modulesDirectories: ['node_modules'],
    extensions:         ['', '.js', '.elm']
  },

  module: {
    noParse: /\.elm$/,
    loaders: [
      { test: /\.less$/, loader: 'style!css!less' },
    ]
  },

  plugins: [],

  postcss: [ autoprefixer( { browsers: ['last 2 versions'] } ) ],

}

// additional webpack settings for local env (when invoked by 'npm start')
if ( TARGET_ENV === 'development' ) {
  console.log( 'Serving locally...');

  module.exports = merge( commonConfig, {

    entry: [
      'webpack-dev-server/client?http://localhost:8080',
      path.join( __dirname, 'src/index.js' )
    ],

    devServer: {
      inline:   true,
      progress: true
    },

    module: {
      loaders: [
        {
          test:    /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          loader:  'elm-hot!elm-webpack?verbose=true&warn=true'
        }
      ]
    }

  });
}

// additional webpack settings for prod env (when invoked via 'npm run build')
if ( TARGET_ENV === 'production' ) {
  console.log( 'Building for prod...');

  module.exports = merge( commonConfig, {

    entry: path.join( __dirname, 'src/index.js' ),

    module: {
      loaders: [
        {
          test:    /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          loader:  'elm-webpack'
        }
      ]
    },

    plugins: [
      new CopyWebpackPlugin([
        {
          from: 'src/img/',
          to:   'img/'
        },
        {
          from: 'src/favicon.ico'
        },
      ]),

      new webpack.optimize.OccurenceOrderPlugin(),

      // extract CSS into a separate file
      new ExtractTextPlugin( './[hash].css', { allChunks: true } ),

      // minify & mangle JS/CSS
      new webpack.optimize.UglifyJsPlugin({
          minimize:   true,
          compressor: { warnings: false }
          // mangle:  true
      })
    ]

  });
}
