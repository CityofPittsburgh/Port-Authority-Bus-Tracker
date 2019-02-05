require(jsonlite)

key <- fromJSON("key.json")$key

baseUrl <- "http://realtime.portauthority.org/bustime/api/v3/"

getRealTime <- function(endpoint, params, response) {
  if (missing(params)) {
    url <- paste0(baseUrl, endpoint, "?format=json&key=", key)
  } else if (typeof(params) == "list") {
    params_text <- paste0(names(params), "=", params, collapse = "&")
    url <- paste0(baseUrl, endpoint, "?format=json&key=", key, "&", params_text)
  }
  json <- fromJSON(url)$`bustime-response`[[response]]
}

feeds <- fromJSON(paste0(baseUrl, "getrtpidatafeeds?format=json&key=", key))$`bustime-response`$rtpidatafeeds

feed_names <- gsub(" ", "%20", feeds$displayname)

routes <- getRealTime("getroutes", list(rtpidatafeed = feed_names[2]), "routes")
  
for (i in seq(from = 1, to = nrow(routes), by = 10)) {
  j <- i + 9
  if (i == 1) {
    vehicles <- getRealTime("getvehicles", list(rtpidatafeeds = feed_names[2], rt = paste(routes$rt[i:j], collapse =",")), "vehicle")
  } else {
    vehicles <- rbind(vehicles, getRealTime("getvehicles", list(rtpidatafeed = feed_names[2], rt = paste(routes$rt[i:j], collapse =",")), "vehicle"))
  }
}