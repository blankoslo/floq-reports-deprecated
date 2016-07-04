require( './styles/main.less' );
var axios = require('axios');

var Elm = require( './Main' );
var app = Elm.Main.embed(document.getElementById('app'), {
  token: window.apiToken,
  apiUrl: window.config.apiUri
});

app.ports.fetchFile.subscribe(function(args) {
  url = args[0];
  jwt = args[1];
  filename = args[2];

  axios.request({
    url: url,
    method: 'get',
    headers: {
      'Accept': 'text/csv',
      'Authorization': 'Bearer ' + jwt
    }
  })
  .then(function(response) {
    var data = response.data;

    // transform the response into a file
    var blob = new Blob([data], { type: response.headers['content-type'] });
    var url = window.URL.createObjectURL(blob);

    // attach blob url to anchor element with download attribute
    var anchor = document.createElement('a');
    anchor.setAttribute('href', url);
    anchor.setAttribute('download', filename);
    anchor.click();
    window.URL.revokeObjectURL(url);
  });
});
