# Example how i used FMI WFS interface for fetching ten years of daily snow depth
# You can change the place, the parameter you fetch, time or anything
# Get your personal apikey from here: http://en.ilmatieteenlaitos.fi/open-data-manual 

install.packages("httr")
install.packages("xml2") 
install.packages("dplyr")
library(httr)
library(xml2)
library(dplyr)

#create days of the year for the final dataframe, anyt year except leap year will do
sequence_of_dates<-seq(as.Date("2010/1/1"), by = "day", length.out = 365)
date_column<-data.frame(format(as.Date(sequence_of_dates, "%Y/%m/%d"), "%m-%d"))
colnames(date_column) <- c("onlydates")
snow<-date_column

# Fetch daily snow depth for last ten whole years
for (year in 2007:2016){
  
  #beginning of the url
  url<-parse_url("http://data.fmi.fi/fmi-apikey/insert your api key here/wfs?")
  url$query <- list(request = "getFeature", 
                    storedquery_id = "fmi::observations::weather::daily::timevaluepair", 
                    place = "insert you place here", 
                    parameters = "snow",
                    starttime = paste(year,"-01-01T00:00:00Z", sep = ""), 
                    endtime = paste(year,"-12-31T23:00:00Z", sep = ""))
  
  #url with all parameters
  completeurl<-build_url(url)
  
  #read xml response
  response <- read_xml(completeurl)
  
  #get needed information from xml response, dates and values
  daterows <- xml_find_all(response, "//wml2:time")
  valuerows <- xml_find_all(response, "//wml2:value")
  
  #remove stuff around dates and values
  dates <- strptime(trimws(xml_text(daterows)), "%Y-%m-%d") 
  onlydates<-format(as.Date(dates, "%Y-%m-%d"), "%m-%d")
  values <- as.numeric(trimws(xml_text(valuerows)))
  
  #add to same dataframe
  df<-data.frame(onlydates,values)
  colnames(df)[2] <- year
  
  #merge the year's values, without getting extra rows from leap years but keep all the original rows even there is NA values
  snow<-merge(x= snow, y= df, by.y='onlydates', all.x=TRUE)
}

#calculate the ten year average for each day
snow$Mean <- round(rowMeans(snow[,2:11], na.rm=TRUE),0)

# i continued by calculating rolling mean with zoo packet for the snow year (no gap in the newyear). What you will do? 