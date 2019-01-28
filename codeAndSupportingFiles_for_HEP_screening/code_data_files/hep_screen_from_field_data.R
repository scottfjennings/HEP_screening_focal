
#===========================================================
## THIS IS NEEDED FOR SOME 2015 and 2016 DATA THAT AREN'T YET IN THE ACCESS DB

library(plyr)
library(tidyverse)
library(lubridate)
library(hms)

sub_sites <- read_csv("C:/Users/scott.jennings/Documents/HEP_screening/code_data_files/sub_sites.csv")

names(sub_sites) <- tolower(names(sub_sites))

##----------------------------------

nest_csv_import_fixer <- function(year){

# year = "2015"  
  zfile = paste("C:/Users/scott.jennings/Documents/HEP_screening/field_data/", year, "/nests/", sep = "")
file.list <- list.files(path = zfile, pattern='*.csv', full.names=T)

import=function(filename){
  sheet.x=read_csv(filename)
  sheet.x$site_name=filename
  names(sheet.x) <- tolower(names(sheet.x))
  return(sheet.x)
}

foo <- ldply(file.list, import)

foo_cleaned <- foo %>% 
  mutate(date = mdy(date),
         adults = as.numeric(ifelse(is.na(adults), 0, adults)),
         stage = ifelse(is.na(stage), 0, stage),
         chicks = ifelse(is.na(chicks), 0, chicks),
         confidence = as.logical(ifelse(is.na(confidence), FALSE, TRUE)),
         site_name = gsub(".csv", "", site_name),
         site_name = gsub(zfile, "", site_name)) 

foo_cleaned2 <- left_join(foo_cleaned, sub_sites) %>% 
  select(-site_name, -site)
return(foo_cleaned2)
}

#nests2015 <- nest_csv_import_fixer("2015")

years <- c("2015", "2016")

csv_nests <- ldply(years, nest_csv_import_fixer)
write_csv(csv_nests, "C:/Users/scott.jennings/Documents/HEP_screening/code_data_files/2015_2016_nests.csv")


##----------------------------------


visits_csv_import_fixer <- function(year){
  
 #  year = "2015"  
  zfile = paste("C:/Users/scott.jennings/Documents/HEP_screening/field_data/", year, "/visits/", sep = "")
  file.list <- list.files(path = zfile, pattern='*.csv', full.names=T)
  
  import=function(filename){
    sheet.x=read_csv(filename)
    sheet.x$site_name=filename
    names(sheet.x) <- tolower(names(sheet.x))
    return(sheet.x)
  }
  
  foo <- ldply(file.list, import)
  
  foo_cleaned <- foo %>% 
    mutate(date = mdy(date),
           site_name = gsub("_visits.csv", "", site_name),
           site_name = gsub(zfile, "", site_name),
           #starttime = ifelse(is.na(starttime), "0000", starttime),
           starttime = paste(substr(starttime, 1,2), ":", substr(starttime, 3, 4), ":00", sep = ""),
           starttime = as.hms(starttime),
           #-
           #endtime = ifelse(is.na(endtime), "0000", endtime),
           endtime = paste(substr(endtime, 1,2), ":", substr(endtime, 3, 4), ":00", sep = ""),
           endtime = as.hms(endtime),
           #-
           possiblyactive = as.integer(possiblyactive))
  
  foo_cleaned2 <- left_join(foo_cleaned, sub_sites) %>% 
    select(speciescode = species, everything(), -site_name, -site)
  return(foo_cleaned2)
}

#nests2015 <- nest_csv_import_fixer("2015")

years <- c("2015", "2016")

csv_visits <- ldply(years, visits_csv_import_fixer)

write_csv(csv_visits, "C:/Users/scott.jennings/Documents/HEP_screening/code_data_files/2015_2016_visits.csv")


