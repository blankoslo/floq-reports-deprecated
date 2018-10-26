require( './styles/main.less' );
require('whatwg-fetch');
const csvExporter = require('export-to-csv')
const XLSX = require('xlsx')

var Elm = require( './Main' );
var app = Elm.Main.embed(document.getElementById('app'), {
  token: window.apiToken,
  apiUrl: window.config.apiUri
});

const getFetchConfig = ( payload, accept = 'text/csv' ) => ({
  method: payload ? 'POST' : 'GET',
  headers: {
    'Authorization': 'Bearer ' + window.apiToken,
    'Content-Type': 'application/json',
    'Accept': accept,
  },
  body: payload ? JSON.stringify(payload) : undefined,
})

app.ports.fetchFile.subscribe(function(args) {
  let [url, jwt, filename] = args
  
  fetch(url, getFetchConfig())
    .then(function(response) {
      if (!response.ok) {
        return response.json()
          .then( err => Promise.reject(err) )
      }

      return response.arrayBuffer();
    })
    .then( data => downloadDataAsFile(data, filename) )
    .catch( err => console.log(err) )
});

const downloadDataAsFile = ( data, filename ) => {
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
}

const createPayloadFromString = ( payload ) => {
  return payload = payload
  ? {
      employee_id: parseInt( payload.split(',')[0] ),
      from_date: payload.split( ',' )[1],
      to_date: payload.split( ',' )[2],
    } 
  : undefined 
}

const fetchEmployeeHours = ( payload ) => {
  return fetch( `${window.config.apiUri}/rpc/entries_sums_for_employee_with_project`, getFetchConfig(createPayloadFromString(payload), 'application/json') )
    .then( res => res.json() )
    .then( groupDataOnDates )
    .catch( err => console.log(err) )
}

const getAllDates = ( data ) => {
  return Array.from( new Set(data.map(element => element.work_date)) )
    .sort( (a, b) => new Date(a).getTime() - new Date(b).getTime() )
}

const emptyArrayOfLength = ( projects ) => projects.map( e => 0 )

const createProjectColumn = ( project, hours, dates ) => {
  const projectRow = { project }
  dates.forEach( (work_date, index) => { projectRow[work_date] = hours[index] } )
  return projectRow
}

const groupDataOnDates = ( timeEntries ) => {
  const dates = getAllDates( timeEntries )
  const hoursGroupedOnProjects = {}
  timeEntries
    .filter( entry => entry.project ) /* Dates with no hours will have project 'null' */
    .forEach( ({ hours, work_date, project }) => {
      const row = hoursGroupedOnProjects[project] || emptyArrayOfLength( dates )
      row[dates.indexOf( work_date )] = hours
      hoursGroupedOnProjects[project] = row
    })
  Object.keys(hoursGroupedOnProjects)
    .forEach( dateKey => {
      if (hoursGroupedOnProjects[dateKey].every( num => !num )) {
        delete hoursGroupedOnProjects[dateKey]
      }
    })
    
  timeEntries.forEach( ({ hours, work_date }) => {
    const row = hoursGroupedOnProjects.total || emptyArrayOfLength( dates )
    row[dates.indexOf( work_date )] +=  hours
    hoursGroupedOnProjects.total = row
  })
  return Object.keys( hoursGroupedOnProjects )
    .map( project => createProjectColumn(project, hoursGroupedOnProjects[project], dates) )
}

const convertToCsv = ( jsonData, fileName ) => {
  const options = {
    filename: fileName,
    fieldSeparator: '\t',
    quoteStrings: '"',
    decimalseparator: ',',
    showLabels: true,
    useBom: true,
    useKeysAsHeaders: true,
  };
  const csvConverter = new csvExporter.ExportToCsv( options )
  const csvData = csvConverter.generateCsv( jsonData, true )
  const workBook = XLSX.read( csvData, { type: 'binary' } )
  const file = XLSX.writeFile( workBook, fileName + '.csv' , { type: 'string', workBook: 'txt' } )
}

app.ports.fetchEmployeeHoursFile.subscribe( function(args) {
  const [filename, payload] = args
  fetchEmployeeHours(payload)
    .then( jsonData => convertToCsv(jsonData, filename) )
})