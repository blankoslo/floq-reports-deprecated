require( './styles/main.less' );

var Elm = require( './Main' );
var app = Elm.Main.embed(document.getElementById('app'), {
  token: window.apiToken,
  apiUrl: window.config.apiUri
});
