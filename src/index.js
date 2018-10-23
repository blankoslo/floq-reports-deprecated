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
  let payload = args[3];
  payload = payload ? {
    employee_id: parseInt(payload.split(',')[0]),
    from_date: payload.split(',')[1],
    to_date: payload.split(',')[2],
  } : undefined
  fetch(url, {
    method: payload ? 'POST' : 'GET',
    headers: {
      'Authorization': 'Bearer ' + jwt,
      'Content-Type': 'application/json',
      'Accept': 'text/csv',
    },
    body: payload ? JSON.stringify(payload) : undefined,
  })
    .then(function(response) {
      if (!response.ok) {
        return response.json()
          .then( err => Promise.reject(err) )
      }

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
      document.body.appendChild(anchor);
      anchor.click();
      document.body.removeChild(anchor);
      window.URL.revokeObjectURL(url);
    })
    .catch( err => console.log(err) )
});
