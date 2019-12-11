# This code calculates the permanent snow cover start date for Muonio for 30-year period.
# Permanent snow cover is defined as the longest continuous snow cover duration of the year.


install.packages("httr",dependencies = TRUE) # needed for parsing the API request
install.packages("xml2", dependencies = TRUE) # for parsing the API XML response
install.packages("dplyr", dependencies = TRUE) # for handling tables
install.packages("ggplot2", dependencies = TRUE) # drawing


library(httr)
library(xml2)
library(dplyr)
library(ggplot2)


# First create the 365-column data.frame where the daily snow measurements will be later added. 
# I can use any normal year for creating the dates sequence, the year is removed, only month and day are needed. 
# In this work, the leap years are omitted, because leap day is not a potential snow season start date at Muonio.


sequence_of_dates<-seq(as.Date("2010/1/1"), by = "day", length.out = 365)
snow<-data.frame(format(as.Date(sequence_of_dates, "%Y/%m/%d"), "%m-%d"))
colnames(snow) <- c("onlydates")



# Fetch the snow measurement every morning at Muonio from FMI API for 31 years (to get 30 full winters)
# FMI (FInnish Meteorological Institute provides an WFS APi for queries, personal api-key is free and data CC-licenced)

for (year in 1988:2018){
  
  #parse the request URL
  #beginning of the URL
  url<-parse_url("http://data.fmi.fi/fmi-apikey/01b32588-41af-44d2-8a8f-03fa9c2a8991/wfs?")
  url$query <- list(request = "getFeature", 
                    storedquery_id = "fmi::observations::weather::daily::timevaluepair", 
                    place = "Muonio", 
                    parameters = "snow",
                    starttime = paste(year,"-01-01T00:00:00Z", sep = ""), 
                    endtime = paste(year,"-12-31T23:00:00Z", sep = ""))
  
  #Complete the URL with all parameters
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
  
  #add to a temporary data.frame
  df<-data.frame(onlydates,values)
  colnames(df)[2] <- year
  
  #merge this year's data to the perviously created data.frame
  snow<-merge(x= snow, y= df, by.y='onlydates', all.x=TRUE)
  
}

#Now data-frame snow contains the 31 years of daily snow depth(cm) measurements, it can be printed.
# -1 value means no snow measurement, NA is broken device.
write.csv(snow, file = "c:\\tyot\\snow.csv", row.names = FALSE)


# Permanent snow is calculated for the winter, not for a calender year. 
# I will split the table between june 30th and july 1st, when in Muonio, typically permanent snow season is not ongoing.
# Day oerder numbers are used.
snow1<-snow[1:181,]
snow2<-snow[182:365,]

#The winter season is consisting from one year's end and next year's beginning
#Before joining the snow1 and snow2 dataframes, they need to be shifted.
#I create filler dataframes to fill the end of year 1987 and beginning of year 2019, which are not fetched 

#filler for the beginning part
filler1 <- data.frame(matrix(0, ncol = 1, nrow = 181))
colnames(filler1)[1] <- "2019"
snow1 <- cbind(snow1, filler1)

# new column names are created, which will be form of (winter) 2000 - 2001
for(i in 2:33){
  secondyear <- as.integer( colnames(snow1)[i])
  newname<- paste(secondyear-1 , secondyear, sep=" - ")
  colnames(snow1)[i] <- newname
}



#filler dataframe for end-of-the-year part
filler2 <- data.frame(matrix(0, ncol = 1, nrow = 184))
colnames(filler2)[1] <- "1987"
snow2 <- cbind(filler2, snow2)
snow2 <- snow2[ , c(2, 1, 3:33)]


#changing the column names in snow2 similarly
for(i in 2:33){
  firstyear <- as.integer( colnames(snow2)[i])
  newname<- paste(firstyear, firstyear+1, sep=" - ")
  colnames(snow2)[i] <- newname
}

#now these two data.frames can be merged (they have the same column names)
snowseasons<-rbind(snow2, snow1)
write.csv(snowseasons, file = "c:\\tyot\\snowseasons.csv", row.names = FALSE)


# For convenience (and easier checking), change values to binary: 
# 0 if no snow, 1 if there is equal or more than 1cm of snow.
snowseasons[2:33] <- +(snowseasons[2:33] >= 1)

# change also NA's to zeroes for easier comparison
# Beware: this may cause wrong results, if the result date is weird in any year, check the periods of NA's
snowseasons[, 2:33][is.na(snowseasons[, 2:33])] <- 0

write.csv(snowseasons, file = "c:\\tyot\\snowseasons_binary.csv", row.names = FALSE)

#this is used for checking the snow season length in one for year at time, not needed in final results
#diff(unique(rev_cumsum(testi == 1)[testi != 1])) 


# a new data.frame is created where the snow season start dates will be stored,
# for convenience, i just take the a copy of the previous data.frame and use the needed columns (2000-2001 etc)
permanent_snow_dates <- subset(snowseasons[1,3:33])

#but for printing with ggplot, this is better to be transposed so that dares form a column, not a row
permanent_snow_dates <- as.data.frame(t(permanent_snow_dates))
colnames(permanent_snow_dates) <- c("date")


# algorithm for calculatign the permanent snow season start date. 
# in case there are several snow seasons, e.g. the snow stays for 3 days and melts away, 
# we need to find the longest season and save it's start date
# this is based on 4 cases; find patters of "00", "01", "10" and "11" in following days snow existence and act accordingly

#for loop for the winter season, 
# i already omitted the first season where we had only a filler zeroes in the end of the first year 1987 
# first column has the dates, so 3rd column is the place to start
for(i in 3:33){ 
  date_candidate_current <- 0
  date_candidate_ex <- 0
  snow_period_ex <- 0
  snow_period_current <- 0
  
  #days of the year in each row, starting from the second day as i check the previous day in the algorithm
  for (j in 2:365){
    #case 00, do nothing
    if(snowseasons[j,i] == 0 & snowseasons[j-1, i] == 0 ){}
    #case 11, increase the snow period counter by one
    else if(snowseasons[j,i] == 1 & snowseasons[j-1, i] == 1 ){
      snow_period_current <- snow_period_current +1
    }
    #case 01, this is the first day of a potential snow season, save the date and increase the counter
    else if(snowseasons[j,i] == 1 & snowseasons[j-1, i] == 0){
      snow_period_current <- snow_period_current +1
      date_candidate_current <- snowseasons[j, 1]
    }
    #case 10, this is the last day of the snow season, check if this was longer than any possible previous seaons and save the longer period
    else if(snowseasons[j,i] == 0 & snowseasons[j-1, i] == 1 ){
      if (snow_period_current > snow_period_ex){
        snow_period_ex <- snow_period_current
        snow_period_current <- 0
        date_candidate_ex <- date_candidate_current
        date_candidate_current <- 0
      }
      else {
        snow_period_current <- 0
        date_candidate_current <-0
      }
    }
  }
  #after each year, save the date in date format
  permanent_snow_dates[i-2, 1] <- format(as.Date(date_candidate_ex, "%m-%d"), format = "%m-%d")
  
}

#print the dates
write.csv(permanent_snow_dates, file = "c:\\tyot\\permanent_snow_dates.csv", row.names = FALSE)



#now print the dates
# development ideas for the plot:
#y-axis scaling should be fixed
# fittinng a curve to the points could help seeing if there is a trend.

ggplot(data = permanent_snow_dates) +
  geom_point(aes(x = rownames(permanent_snow_dates), y = date), 
            color = "#09557f",
            alpha = 0.6,
            size = 1) +
  labs(x = "Season", 
       y = "Permanent snow date",
       title = "Permanent Snow arrival dates in Muonio") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


# print the plot
ggsave("permnent_snow.png", width = 30, height = 20, units = "cm")

