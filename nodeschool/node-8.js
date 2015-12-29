var http = require('http')
var _ = require('underscore')
const url1 = process.argv[2]
const url2 = process.argv[3]
const url3 = process.argv[4]

const urls = [url1, url2, url3]


function httpGetAllUrls(urls, next) {
  if (_.isEmpty(urls)) next([])
  else
    http.get(_.first(urls),
             function (resultFirst) {
               httpGetAllUrls(_.rest(urls), function (resultRest) {
                 next([resultFirst].concat(resultRest))
               })
             }
            )
}
             
httpGetAllUrls(urls, function (rest) {
  console.log(rest)
})
