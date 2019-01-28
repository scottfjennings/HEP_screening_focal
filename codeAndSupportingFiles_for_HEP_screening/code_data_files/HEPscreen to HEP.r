

## code to transfer data from the HEP screening format to the HEP access database format
#####################################

#########################
library(readxl)
library(plyr)
library(tidyverse)
library(lubridate)


####!!!!!!!!!!!!!!!!CHANGE HERE!!!!!!!!!!!!!!!!####
seas="2017"
####!!!!!!!!!!!!!!!!CHANGE HERE!!!!!!!!!!!!!!!!####
setwd("C:/Users/scott.jennings/Documents/HEP_screening/scoring/2017") 


file.list <- list.files(pattern='*.xlsx', full.names=T)



import=function(filename){
sheet.x=read_xlsx(filename, sheet=sheet)
sheet.x$site_name=substr(filename, 3, nchar(filename)-13)
return(sheet.x)
}

### number of survey visits
sheet="visits"
visits<- ldply(file.list, import) %>% 
  select(SPECIES = Species, NUMBERVISITS = days, TOTALHOURS = hours, site_name) %>% 
  mutate(YEAR = seas)


### peak number of active nests
sheet="peak_active"
peakact<- ldply(file.list, import) %>% 
  select(SPECIES = Species, PEAKACTVNSTS, site_name) %>% 
  mutate(YEAR = seas) %>% 
  na.omit(SPECIES)


### number of focal nests and focal nest failures
sheet="foc"
foc<- ldply(file.list, import)%>%
  group_by(site_name, Species) %>%  
  mutate(focal = as.numeric(focal),
         focal = ifelse(is.na(focal), 0, focal),
         focal_fail = as.numeric(focal_fail),
         focal_fail = ifelse(is.na(focal_fail), 0, focal_fail))%>%
  summarise(FOCALNESTS = sum(as.numeric(focal)),
            FOCFAILURE = sum(as.numeric(focal_fail))) %>% 
  select(SPECIES = Species, everything()) %>% 
  mutate(YEAR = seas)


### stage 4 brood sizes
sheet="stg4"
stg4<- ldply(file.list, import) %>% 
  data.frame() %>% 
  select(SPECIES = Species, site_name, everything()) 
stg4[is.na(stg4)] <- 0






### get number of nests in each stage for the HEP survey periods
## first import sheet with number of nests in each stage on each date
import.nest.num.stg=function(filename){
  sheet.x=read_xlsx(filename, sheet=sheet)
  sheet.x$site_name=substr(filename, 3, nchar(filename)-13)
  sheet.x[sheet.x==""]<-"0"
  sheet.x[is.na(sheet.x)] <- "0" 
  #sheet.y <- melt(sheet.x, id.vars = c("Species", "Stage", "site_name"))
  sheet.y <- gather(sheet.x, md, NESTS, -Species, -Stage, -site_name) 
  
  return(sheet.y)
}
sheet="nest_num_stage"
nest_num_stage<- ldply(file.list, import.nest.num.stg) %>% 
  mutate(Date = as.Date(paste(md, seas, sep="-"), format= "%m-%d-%Y"))%>% 
  select(site_name, SPECIES = Species, Date, Stage, NESTS)
         




## get one survey date per species per site
sites.visits.spp <- distinct(nest_num_stage, site_name, Date, SPECIES) %>% 
    select(site_name, SPECIES, Date)

## define the mid-point for the survey periods
early.mar <- as.Date(paste(seas, "3-10", sep="-")) # day options: 10-12
early.apr <- as.Date(paste(seas, "4-11", sep="-")) # day options: 10-12
early.may <- as.Date(paste(seas, "5-11", sep="-")) # day options: 10-12
early.jun <- as.Date(paste(seas, "6-3", sep="-")) # day options: 3-5
late.jun <- as.Date(paste(seas, "6-20", sep="-")) # day options: 20/22
july <- as.Date(paste(seas, "7-11", sep="-")) 


make.month.stages <- function(surv.per, per.header){
 surveys <- sites.visits.spp %>% 
    group_by(site_name, SPECIES) %>% 
    summarize(surv.per.date = Date[which(abs(Date-surv.per) == min(abs(Date - surv.per)))])

  surv.per.data <- inner_join(nest_num_stage, surveys, by=c("site_name", "SPECIES", "Date" = "surv.per.date"))  %>% 
    filter(Stage > 0)%>% 
    mutate(Stage = paste(per.header, "STAGE", Stage, sep=""),
           NESTS = as.numeric(as.character(NESTS)))
  surv.per.data.wide <- spread(surv.per.data, Stage, NESTS)
  surv.per.data.wide[is.na(surv.per.data.wide)] <- "0" 
  names(surv.per.data.wide)[names(surv.per.data.wide)=="Date"] <- paste(per.header, "STGEDATE", sep="")
  
  return(surv.per.data.wide)
}

e.mar<-make.month.stages(early.mar, "MAR")
e.apr<-make.month.stages(early.apr, "APR")
e.may<-make.month.stages(early.may, "MAY")
e.jun<-make.month.stages(early.jun, "JUN")
l.jun<-make.month.stages(late.jun, "LTJUN")
jul<-make.month.stages(july, "JUL") 

## if you get this error for any of these:
## Error in summarise_impl(.data, dots) : expecting a single value
## or
## Error in summarise_impl(.data, dots) : Column `surv.per.date` must be length 1 (a summary value), not 2
## it means the surv.per date was equidistant between 2 actual surveys
## the fix is to adust the surv.per for that period by 1 day

## getting the message
## Using NESTS as value column: use value.var to override.
## is fine


##------------------------------------------------
total.spp <- visits %>% 
  group_by(site_name) %>% 
  summarise(TOTALSPECIES = n())


##------------------------------------------------
new.hep=join_all(list(visits, total.spp, peakact, foc, stg4, e.mar, e.apr, e.may, e.jun, l.jun, jul))



site.names=read_csv("C:/Users/scott.jennings/Documents/HEP_screening/code_data_files/sub_sites.csv") 


new.hep2 = inner_join(new.hep, site.names)  %>% 
  select(CODE, SITE, SPECIES, NUMBERVISITS, TOTALHOURS, YEAR, PEAKACTVNSTS, FOCALNESTS, FOCFAILURE, everything(), -site_name) %>% 
  mutate(CODE=as.factor(CODE),
		NUMBERVISITS=as.integer(NUMBERVISITS),
		YEAR=as.integer(YEAR),
		PEAKACTVNSTS=as.integer(PEAKACTVNSTS),
		FOCALNESTS=as.integer(FOCALNESTS),
		FOCFAILURE=as.integer(FOCFAILURE),
		
		LTJUN.JUL.same = ifelse(as.character(JULSTGEDATE) == as.character(LTJUNSTGEDATE), TRUE, FALSE),
		JULSTAGE1 = ifelse(LTJUN.JUL.same=="TRUE", "", JULSTAGE1),
		JULSTAGE2 = ifelse(LTJUN.JUL.same=="TRUE", "", JULSTAGE2),
		JULSTAGE3 = ifelse(LTJUN.JUL.same=="TRUE", "", JULSTAGE3),
		JULSTAGE4 = ifelse(LTJUN.JUL.same=="TRUE", "", JULSTAGE4),
		JULSTAGE5 = ifelse(LTJUN.JUL.same=="TRUE", "", JULSTAGE5),
		JULSTGEDATE = as.Date(ifelse(LTJUN.JUL.same=="FALSE", as.character(JULSTGEDATE), NA)) 
		
		)%>% 
  select(CODE, SITE, SPECIES, TOTALSPECIES, NUMBERVISITS, TOTALHOURS, YEAR, PEAKACTVNSTS, FOCALNESTS, FOCFAILURE, everything(), -LTJUN.JUL.same)


write_csv(new.hep2, paste("C:/Users/scott.jennings/Documents/Projects/HEP/hep_generated_", seas, ".csv", sep=""))


####!!!!!!!!!!!!!!!!CHANGE HERE!!!!!!!!!!!!!!!!####
#hep_generated_2016 <- new.hep2









