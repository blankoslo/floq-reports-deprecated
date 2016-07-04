require( './styles/main.less' );
require('whatwg-fetch');

var Elm = require( './Main' );
var app = Elm.Main.embed(document.getElementById('app'), {
  token: window.apiToken,
  apiUrl: window.config.apiUri
});

app.ports.fetchFile.subscribe(function(args) {
  url = args[0];
  jwt = args[1];
  filename = args[2];

fetch(url, {
  method: 'GET',
  headers: {
    'Authorization': 'Bearer ' + jwt
  }
})
.then(function(response) {
  return response.arrayBuffer();
})
.then(function(data) {
  // create a blob url representing the data
  var blob = new Blob([data]);
  var url = window.URL.createObjectURL(blob);

  // attach blob url to anchor element with download attribute
  var anchor = document.createElement('a');
  anchor.setAttribute('href', url);
  anchor.setAttribute('download', filename);
  anchor.click();
  window.URL.revokeObjectURL(url);
});
});
