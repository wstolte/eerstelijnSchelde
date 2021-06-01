
require(httr)
require(sf)


getSMdata = function(startyear, endyear, parID) {
  
  urllist <- structure(list(scheme = "http", hostname = "geo.vliz.be", port = NULL, 
                            path = "geoserver/wfs/ows", 
                            query = list(service = "WFS", 
                                         version = "1.1.0", 
                                         request = "GetFeature", 
                                         typeName = "Dataportal:abiotic_observations", 
                                         resultType = "results", 
                                         viewParams = "placeholder", 
                                         propertyName = "stationname,longitude,latitude,datetime,depth,parametername,valuesign,value,dataprovider,datasettitle", 
                                         outputFormat = "application/json"), 
                            params = NULL, 
                            fragment = NULL, 
                            username = NULL, password = NULL), class = "url")
  
  viewParams <- paste("where:obs.context+&&+ARRAY[1]+AND+standardparameterid+IN+(",
  stringr::str_replace_all(paste(parID, collapse = ","), ",", "\\\\,"),
  ")+",
  "AND+imisdatasetid+IN+(588\\,500\\,479\\,135\\,1527\\,476)+", # all RWS datasets
  "AND+((datetime_search+BETWEEN+'", startyear, "-01-01'+AND+'", endyear, "-01-31'+))",
  ";context:0001", sep = "")

  urllist$query$viewParams <- viewParams
  # replace "+" signs with whitespace to be placed in url
  # text with "+" is copied from webservice url
  urllist$query$viewParams  <- stringr::str_replace_all(urllist$query$viewParams, '\\+', ' ')
  url = httr::build_url(urllist)
  result <- sf::st_read(url) %>%
    mutate(value = na_if(value, "999999999999")) %>%
    mutate(value = as.numeric(value))
  return(result)
}


