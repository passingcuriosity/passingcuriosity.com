'use strict';

const http = require('http');

exports.handler = (event, context, callback) => {
  const request = event.Records[0].cf.request;

  let prefixPath; // needed for 2nd condition

  if (request.uri.match('.+/$')) {
    request.uri += 'index.html';
    callback(null, request);
  } else if (prefixPath = request.uri.match('(.+)/index.html')) {
    const response = {
      status: '301',
      statusDescription: 'Found',
      headers: {
        location: [{
          key: 'Location', value: prefixPath[1] + '/',
        }],
      }
    };
    callback(null, response);
  } else if (request.uri.match('/[^/.]+$')) {
    const response = {
      status: '301',
      statusDescription: 'Found',
      headers: {
        location: [{
          key: 'Location', value: request.uri + '/',
        }],
      }
    };
    callback(null, response);
  } else {
    callback(null, request);
  }
}