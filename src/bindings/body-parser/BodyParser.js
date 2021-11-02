"use strict"
const bodyParser = require("body-parser")

exports.bodyParserJson = bodyParser.json ()

exports.bodyParserUrl = bodyParser.urlencoded({
  extended: true
})