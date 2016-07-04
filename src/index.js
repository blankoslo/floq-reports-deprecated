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
    responseType: 'blob',
    headers: {
      'Accept': 'text/csv',
      'Authorization': 'Bearer ' + jwt
    }
  })
  .then(function(response) {
    // create a blob url representing the data
    var url = window.URL.createObjectURL(response.data);

    // attach blob url to anchor element with download attribute
    var anchor = document.createElement('a');
    anchor.setAttribute('href', url);
    anchor.setAttribute('download', filename);
    anchor.click();
    window.URL.revokeObjectURL(url);
  });
});
