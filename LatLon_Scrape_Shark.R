# scraping web data
library(rvest)
library(dplyr)
library(ggplot2)
library(reshape2)
library(lubridate)
options(stringsAsFactors = F)
setwd("C:/Kemp Solutions/Dataquest/Articles/SharkWeek")
ds <- read.csv("shark.csv")
ds$dt <- gsub("Reported ", "", ds$Date)
ds$dt <- dmy(ds$dt)
url1 <- "http://www.geonames.org/advanced-search.html?q="
url2 <- "&country=&featureClass=A&continentCode=&fuzzy=0.8"
country <- unique(ds$Country)
country <- country[nchar(country) > 0]
area <- unique(ds$Area)
area <- area[nchar(area) > 0]

urls <- paste0(url1, country, url2)
urls2 <- paste0(url1, area, url2)
df <- data.frame()


# create function to pull
getData <- function(url){
      if(nchar(url) > 2){
      print(substr(url, 48, nchar(url) - 49))
      pga.ref <- read_html(url)
      pga.ds <- pga.ref %>%
        html_nodes(xpath ='//td')
      if(length(pga.ds) >= 11){
        temp <- data.frame(str = html_text(pga.ds[[11]]))
      }
      #output <- pga.ds[[1]]
      if(nrow(temp) > 0){
        temp$country <- substr(url, 48, nchar(url) - 49)
        df <<- rbind(df, temp)
      }
  }
}

dk <- lapply(X = urls2, FUN = getData)
  
getLL <- function(str){       
  out <- substr(str, gregexpr("\\.\\.\\.", str)[[1]][1] + 3, nchar(df$str[14]))
  return(out)
}

lst <- as.list(df$str)
latLong <- unlist(lapply(X = lst, FUN = getLL))
df$LatLong_raw3 <- latLong
 
aa <- data.frame(table(ds$Area))
df <- merge(df, aa, by.x = "country", by.y = "Var1", all.x = T)
df.country <- df

write.csv(df, file = "Areas_latLong.csv")
aLL <- read.csv("Areas_latLong.csv")
aLL <- aLL %>% filter(!is.na(Lat)) %>% select(country, Lat, Long)

ds <- filter(ds, dt >= "1969-07-28")
ds <- merge(ds, aLL, by.x = "Area", by.y = "country", all.x = T)
need <- ds[is.na(ds$Lat),]

save.image("sharks.RData")

