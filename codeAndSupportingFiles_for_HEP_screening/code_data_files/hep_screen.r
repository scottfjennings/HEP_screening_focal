

## this code takes the raw HEP site visit data from the hep_raw access database, and creates a multi-sheet excel file that allows the manual screening of nests
## the sheets created are:
## 1. The scoring sheet: Either 1 sheet, named 'nests' (for colonies with few nests or species), or a sheet for each species (for large and multi-species colonies) that is equivalent to the handwritten scoring sheet, plus some fields generated by the nest screening algorithm that serve as helpers for the manual screening; the user determines whether 1 or multiple sheets are generated. This sheet has the following fields:
    # species code
    # nest ID
    # columns for each day the nest was checked showing the number of adults, status, number of chicks, and confidence in chick count
    # 'focal' =  yes/no (1/0) for whether the nest meets the basic requirements for being focal
    # 'focal fail' = yes/no (1/0) for whether a nested classified as focal failed
    # 'fledged = yes/no (1/0) for nests likely to have fledged (reached min fledge age in either stage 4 or 5), 
    # 'days_short_of_fledging' = for nests that didn't clearly fail but were observed innactive earlier than expected, the number of days from the last date it was observed active until the expecteed fledge date; 
    # 'active_last_day' = yes/no (1/0) indicating if nest was still active on the last day it was checked
    # 'max_stage' = maximum stage teh nest was observed in
    # 'num_spp' = number of species assigned to this nest ID- helps find where observer might have been confused about which nest was which, or true species turnovers
    # 'stg4_brood' = maximum confidently observed stage 4 brood size

## 2. peak_active: shows the date, julian date, and number of nests for the peak active date for each species

## 3. stg4: number of stage 4 nests of each brood size for each species

## 4. nest_num_stage: number of nests of each species in each stage on each date the colony was checked

## 5. num_active: total number of active nests for each species on each date the colony was checked
#--------------------

## once the libraries are loaded and the colony and season info have been set, then everything down to 'RUN TO HERE FIRST' around line 415 can be run

## then the decision about how many scoring sheets to write can be made, and the final xls file can be written back to the working directory (be sure to check the file path there too)

#--------------------
## load the required packages
library(plyr)
library(tidyverse)
library(reshape2)
library(lubridate)
library(xlsx)
library(readxl)
library(hms)

library(daff)

#-----------
#specify which colony and season you want to work with
col="W9th" # this needs to match the site_name field in the code_data_files/sub_sites.csv file; if the colony you're working on doesn't have a site_name yet, you can create one that makes sense (a shorter version of the colony name, with no spaces for best use as a file name); be sure to add this new site_name to the sub_sites file.
col.code="103"
seas="2018"


#-----------
####  STARTING WITH THE VISITS DATA
## gets written to final xls
hep.visits <- read_xlsx("codeAndSupportingFiles_for_HEP_screening/queried_from_access/HEP_visits Query.xlsx", "HEP_visits_Query")

#hep.visits <- read_csv("HEP_screening/queried_from_access/HEP_visits Query.csv")
names(hep.visits) <- tolower(names(hep.visits))
hep.visits <- hep.visits %>% 
  mutate(date = as.Date(date, format = "%d-%b-%y"),
         endtime = as.hms(endtime),
         starttime = as.hms(starttime))

## if working with 2015 or 2016 data...
#csv_visits <- read.csv("code_data_files/2015_2016_visits.csv")
#csv_visits <- csv_visits  %>% 
#  mutate(date = as.character(date),
#         date = ymd(date),
#         endtime = as.character(endtime),
#         endtime = as.hms(endtime),
#         starttime = as.character(starttime),
#         starttime = as.hms(starttime),
#        obsinitial = as.character(obsinitial),
#         speciescode = as.character(speciescode)) %>% 
#  select(code, date, obsinitial, starttime, endtime, speciescode, active, possiblyactive)

#hep.visits <- bind_rows(hep.visits, csv_visits)
######

hep.visits.sub <- hep.visits %>% 
  filter(code==col.code, year(date) == seas) %>%
  droplevels()

make_visits <- function(){

hep.visits.sub <- hep.visits.sub %>% 
  mutate(#e_time = as.hms(endtime),
         #s_time = as.hms(starttime),
         surveytime = (endtime-starttime)/60/60) %>% 
  select(species = speciescode, everything())

totobsdays=hep.visits.sub %>% 
  group_by(species) %>%
  summarize(days = length(starttime))	

totobshours=hep.visits.sub %>% 
  group_by(species) %>%
  summarize(hours = sum(surveytime)) %>% 
  mutate(hours = round(hours, 1))

visits <- full_join(totobsdays, totobshours)
return(visits)

}
visits <- make_visits()

# extract the number of active nests counted in the field
counted.active <- hep.visits.sub %>% 
  select(date, species = speciescode, counted.active = active)


#-----------
##importing from hep_raw access database
## (in access, run HEP_individual nests Query, export to xls, then save as csv)

hep_all=read.csv("codeAndSupportingFiles_for_HEP_screening/queried_from_access/HEP_individual nests Query.csv") 

names(hep_all) <- tolower(names(hep_all))


#-----------
## data management
hep_all <- hep_all %>% 
  mutate(nest = as.character(nest), ## to allow non-numerical nest IDs
         date = mdy(date), # fix format of date field
         stage = ifelse(is.na(stage), 9, stage), # fill in missing data
         adults = ifelse(is.na(adults), 9, adults), 
         chicks = ifelse(is.na(chicks), 9, chicks), 
         confidence = ifelse(is.na(confidence), 9, confidence)) %>% 
  select(species = speciescode, everything())


## here can add in 2015 and 2016 data from csvs (single object generated with 'hep_screen_from_field_data.R' and with a copy saved at HEP_screening/code_data_files/2015_2016_nests.csv)
## be sure to edit the file path for your computer
#csv_nests <- read_csv("codeAndSupportingFiles_for_HEP_screening/code_data_files/2015_2016_nests.csv", col_types = cols(code = col_character())) %>% 
#  mutate(code = as.numeric(code))

#hep_all <- bind_rows(hep_all, csv_nests)


## double check which years and colonies are present
#hep_checker <- hep_all %>% 
#  mutate(Year = year(date)) %>% 
#  distinct(code, Year)



#-----------
## filter to just the colony and season you want to work with
hep <- hep_all %>% 
  filter(code==col.code, year(date) == seas) %>%
  droplevels() %>% 
  arrange(species, nest, date)
#-----------



###############
## these 2 together fill in days where a nest was missed but other nests for that species were checked
spp.dates <- hep %>% 
  distinct(species, date) %>%
  mutate(sp.surveyed = "Y")

hep.breaks <- hep %>% 
  select(species, nest, date, stage) %>% 
  arrange(species, nest, date)%>% 
  spread(date, stage) %>% 
  gather(date, stage, -species, -nest) %>% 
  mutate(date = as.Date(date))%>% 
  full_join(., spp.dates) %>% 
  filter(!is.na(sp.surveyed)) %>% 
  arrange(species, date)

# identify where a nest was missed and the checks before and after had stage > 0
id.breaks <- hep.breaks %>% 
  arrange(species, nest, date) %>% 
  group_by(species, nest) %>% 
  mutate(prev.stage = lag(stage),
         next.stage = lead(stage),
         inf.status = NA,
         inf.status = ifelse(is.na(stage) & 
                               (prev.stage > 0 & prev.stage < 8) & 
                               (next.stage > 0 & next.stage < 8), "A", inf.status))

inf.status.only <- id.breaks %>% 
  select(species, nest, date, inf.status) %>% 
  filter(!is.na(inf.status)) %>% 
  droplevels() %>% 
  ungroup()

# add those records back to the original data
hep.inf.status <- hep %>% 
  mutate(inf.status = "") %>% 
  bind_rows(., inf.status.only) %>% 
  mutate(inf.status = ifelse(month(date) < 4 & inf.status == "A" & stage == 0 & adults < 2, "P", inf.status))

## and make a few new fields
hep <- hep.inf.status %>% 
  mutate(jdate = yday(date), # create Julian date
         md=paste(month(date), day(date), sep = "-"), # create a fiedld for Month-Day
# this next one creates the field to be displayed in the nest Scoring Form
# combining number of adults, stage, number of chicks, and a * if confidence was checked,
# or just status and adults if the nest was called something other than active
          ad.stg.chx.conf=ifelse(status=="A", 
                                 paste(adults, paste(stage, chicks, sep="/"), sep=" "),
                                 paste(status, adults, sep = "-")),
          ad.stg.chx.conf = ifelse(confidence == 1, paste(ad.stg.chx.conf, "*", sep = ""),
                         ad.stg.chx.conf),
          ad.stg.chx.conf = ifelse(inf.status == "A", "A!", ad.stg.chx.conf),
          ad.stg.chx.conf = ifelse(inf.status == "P", paste("P!", adults, sep = "-"), ad.stg.chx.conf))

hep <- hep %>% 
  arrange(species, nest, date)


#-----------
## for each visit, make table with the number of nests in each stage
## gets written to final xls
nest_num_stage = hep %>% 
	filter(status == "A") %>%
    group_by(species, date, stage) %>%	
	summarise(num_nests = length(nest)) %>% 
  spread(date, num_nests)
names(nest_num_stage) <- gsub(paste(seas, "-", sep = ""),"",names(nest_num_stage))
nest_num_stage[is.na(nest_num_stage)] <- "-"


#-----------
## count how many nests were present for each species on each day
## num_active is whichever is greatest between # active nests recorded on front of sheet and number of nests from back of sheet
nests_per_day <- hep %>% 
  filter((status == "A" | inf.status == "A") & inf.status != "P") %>%
  group_by(species, date) %>%	
  summarise(calc_active = length(nest)) %>% 
  full_join(., counted.active) %>% 
  mutate(num_active = ifelse(calc_active > counted.active, calc_active, counted.active)) 

## make wide version of the number of nests of all status for each species on each visit	
## gets written to final xls
num_active_wide <- nests_per_day %>% 
  select(date, species, num_active) %>%  
  spread(date, num_active) 
names(num_active_wide) <- gsub(paste(seas, "-", sep = ""),"",names(num_active_wide))
num_active_wide[is.na(num_active_wide)] <- "-"

#-----------
## determine the date with the peak number of active nests, and PEAKACTVNSTS
## gets written to final xls
peak_active <- nests_per_day %>% 
  group_by(species) %>%
  mutate(num_active = replace_na(num_active, 0)) %>% 
	filter(num_active == max(num_active)) %>%
	filter(date == min(date)) %>% 
  mutate(peak_active_jdate = yday(date)) %>% 
  select(species, peak_active_date = date, peak_active_jdate, PEAKACTVNSTS = num_active)


#-----------
## make the df used by the nest screening algorithm
make_nests4screening <- function(){

# create df with the minimum fledging age and duration of incubation for each species
  sp_stats1=data.frame(species=c('BCNH', 'SNEG', 'CAEG', 'GREG', 'GBHE', 'DCCO'),
                       min_fl=as.numeric(c('40', '34', '38', '77', '84', NA)),
                       inc_dur=as.numeric(c('25', '22', '24', 	'28', '28', NA)))
  

# make several fields that will be used for automated screening 

# determine the latest date of observations for each species
spp_max_date=hep %>% 
	filter(status=="A") %>%
    group_by(species) %>%
	summarise(spp_max_date = max(date)) 

# create a df with the general species info (min_fl and inc_dur) and the colony-specific species info (peak_active and spp_max_jdate)
spp_stats <- full_join(peak_active, spp_max_date) %>% 
                  inner_join(., sp_stats1)

# create unique list of each species+nest combo
spp.nests2=unique(hep[c("species", "nest")])

# merge unique nest list with the species info
spp.nests1 <- full_join(spp.nests2, spp_stats)
#----------
# ID nests that were assigned to more than 1 species
multi_spp_nests <- hep %>% 
	filter(status == "A") %>%
    group_by(nest) %>%
    summarise(num_spp = n_distinct(species))	
#----------
# determin the maximum stage each nest was observed in	
max_stage <- hep %>% 
	filter(status == "A" & stage < 8) %>%
    group_by(species, nest) %>%	
	summarise(max_stage = max(stage)) 



#----------	
# determin the minimum stage each nest was observed in		
min_stage <- hep %>% 
	filter(status == "A") %>%
    group_by(species, nest) %>%	
	summarise(min_stage = min(stage)) 
#----------	
# determin the minimum stage each nest was observed with chick		
min_stage_chx <- hep %>% 
	filter(status == "A", stage > 1 & stage < 8) %>%
    group_by(species, nest) %>%	
	summarise(min_stage_chx = min(stage)) 
#----------
# determin the latest date each nest was observed	
latest_date <- hep %>% 
	filter(status == "A") %>%
    group_by(species, nest) %>%	
	summarise(latest_date = max(date)) 
#----------
# determin the earliest date each nest was observed	
earliest_date <- hep %>% 
	filter(status == "A") %>%
    group_by(species, nest) %>%	
	summarise(earliest_date = min(date)) 
#----------
# determin the latest date each nest was observed in stage 1	
latest_date_stage1 <- hep %>% 
	filter(status == "A") %>%
    group_by(species, nest) %>%
	filter(stage == 1) %>%
	summarise(latest_date_stage1 = max(date)) 
#----------
# determin the latest date each nest was observed in stage 4 or later
latest_date_unguarded <- hep %>% 
	filter(status == "A") %>%
    group_by(species, nest) %>%
	filter(stage > 3) %>%
	summarise(latest_date_unguarded = max(date)) 
#----------
# determin the earliest date each nest was observed in stage 1
earliest_date_stage1 <- hep %>% 
	filter(status == "A") %>%
    group_by(species, nest) %>%
	filter(stage == 1) %>%
	summarise(earliest_date_stage1 = min(date))	
#----------
# determin the earliest date each nest was observed with chicks (later than stage 1)
earliest_date_chx <- hep %>% 
	filter(status == "A") %>%
    group_by(species, nest) %>%
	filter(stage > 1 & stage < 8) %>%
	summarise(earliest_date_chx = min(date))	
#----------
# determin the stage 4 brood size of each nests (where knowable)

stg4_brood <- hep %>% 
	filter(status == "A") %>%
  group_by(species, nest) %>%	
	filter(stage == 4, confidence == 1) %>%
	filter(date == max(date)) %>%
	select(species, nest, stg4_brood = chicks) 


#----------
#determin the earliest stage a nest was observed in	
earliest_stage=hep %>% 
	filter(status == "A") %>%
    group_by(species, nest) %>%	
	filter(date == min(date)) %>%
	select(species, nest, earliest_stage = stage) 
	
## bind all those created objects
nests1 <- join_all(list(spp.nests1, earliest_date_chx, earliest_stage, min_stage_chx, earliest_date_stage1, latest_date_stage1, latest_date_unguarded, max_stage, latest_date, earliest_date, stg4_brood), type = 'full')
nests <- join_all(list(nests1, multi_spp_nests), type = 'full')



return(nests)
}
nests4screening <- make_nests4screening()


#-----------
# make automated screening calculations
screened_nests <- nests4screening %>%  
  mutate(
	# was the nest still active on the last day of nest checks for this species?
	active_last_day = ifelse(latest_date==spp_max_date, 1, 0),
	#meets criteria for focal nest 
	focal = ifelse(# nest was observed active before peak colony size
	              earliest_date <= peak_active_date &
	             # nest was observed before hatching
								earliest_stage < 2 & 
							 # nest actually reached incubation or brooding
								max_stage > 0 & 
								  # nest was first observed during the first 2 weeks of incubation or earlier
									(((earliest_date <= round(earliest_date_chx - (inc_dur - 14), 0)) & min_stage_chx < 4 ) | 
									# or the first day chicks were observed they were still being guarded
										(earliest_date <= round(latest_date_stage1 - (inc_dur - 14),0))), 1, 0),	   
																							   # | means or
											   
	# did the nest reach the minimum fledging age
	fledged = ifelse(latest_date_unguarded-earliest_date_stage1 > min_fl, 1, 0),
	fledged = ifelse(is.na(fledged), 0, fledged),
	

	# was it a focal nest that failed?
	focal_fail = ifelse(focal == 1 & active_last_day == 0 & fledged == 0, 1, 0),  

	#count number of days short of fledging the nest was observed
	days_short_of_fledging = ifelse(max_stage > 3 & fledged == 0,(latest_date_unguarded-earliest_date_stage1) - min_fl, "")
  )


#-----------
# count focal fails detected on each visit
make_foc_fails <- function(){
# first determin which dates each species was surveyed
survey_dates=hep %>% 
	distinct(species,date) 
	
# then determine the final day observed for each focal nest that failed
focal_fail_dates = screened_nests %>% 
  filter(focal_fail==1) %>% 
  group_by(species, latest_date) %>%
	summarize(num_fails = length(nest)) %>% 
  select(date = latest_date, everything()) 
# join 	
zdates <- full_join(focal_fail_dates, survey_dates)
# this finds the next day after the last day a failed focal nest was observed active, which is the first day it was observed failed,
# then makes it into wide format for output to the xls
focal_fail_dates_wide <- zdates %>%
  arrange(species, date) %>% 
  mutate(next_date = lead(date),
         next_md = paste(month(next_date), day(next_date), sep="-"),
         next_md = ifelse(next_md=="NA-NA", "", next_md)) %>% 
  select(species, num_fails, next_md) %>% 
  filter(num_fails>0) %>% 
  spread(next_md, num_fails)
return(focal_fail_dates_wide)
}
focal_fail_dates_wide <- make_foc_fails()	


#foo <- filter(hep, species == "GREG", nest == "56") %>% 
#  select(date, everything(), -md, -jdate, -code, - sheetnum, -back_id, -inf.status, -ad.stg.chx.conf) %>% 
#  arrange(date)


#foo2 <- edit(foo)
## getting this error is OK; it just means there were no focal failures
#  Error in enc2utf8(col_names(col_labels, sep = sep)) : 
#   argument is not a character vector


#-----------
# make the nest Scoring Form
make_nest_scoring <- function(){
  # extract just the fields from 'nests' needed to export
  nests.sub <- screened_nests %>% 
    select(species, nest, focal, focal_fail, fledged, days_short_of_fledging, active_last_day, max_stage, num_spp, stg4_brood)
  
hep.wide <- hep %>% 
  arrange(species, date) %>% 
  select(date, species, nest, ad.stg.chx.conf) %>%
  unique() %>% 
  spread(date, ad.stg.chx.conf)
names(hep.wide) <- gsub(paste(seas, "-", sep = ""),"",names(hep.wide))

hep.wide1=full_join(hep.wide, nests.sub)
return(hep.wide1)
}
hep.wide1 <- make_nest_scoring()


#-----------
# Number of stage 4 nests of each brood size
## gets written to final xls
stg4_wide = hep.wide1 %>%
	group_by(species, stg4_brood) %>%
	summarize(num_nests = length(nest)) %>% 
  filter(stg4_brood>0) %>% 
  mutate(stg4_brood = paste("BRD", stg4_brood, sep = "")) %>% 
  spread(stg4_brood, num_nests) 
stg4_wide[is.na(stg4_wide)] = 0
 ## getting to following error is fine, it just means there were no stage 4 brood sizes in the original data
# Error in enc2utf8(col_names(col_labels, sep = sep)) : 
#  argument is not a character vector



##----------
## replace all the NAs with blank to make the xls output tidier
hep.wide1[is.na(hep.wide1)] <- ""
focal_fail_dates_wide[is.na(focal_fail_dates_wide)] <- ""
## getting this error is OK; it just means there were no focal failures
# Error in focal_fail_dates_wide[is.na(focal_fail_dates_wide)] <- "" : 
# object 'focal_fail_dates_wide' not found
nest_num_stage[is.na(nest_num_stage)] <- ""
num_active_wide[is.na(num_active_wide)] <- ""
stg4_wide[is.na(stg4_wide)] <- ""



#################		RUN TO HERE FIRST		################

### look at numer of nests for each species to decide whether final xlsx should have spp lumped or in different sheets.
### then run export.hep()  and desired appender functions below
hep %>%
		group_by(species, nest) %>%
		select(species, nest) %>%
		unique() %>% 
  group_by(species) %>%
  summarise(num_nests = n()) %>% 
  data.frame()

### look at numer of multi-species nests to decide whether final xlsx should have sheet for those.
hep.wide1 %>% filter(num_spp>1) %>% nrow()

##-------------------------------------


# need to make all the tbls into dfs for write.xlsx  
hep.wide1 <- as.data.frame(hep.wide1)
peak_active <- as.data.frame(peak_active)
stg4_wide <- as.data.frame(stg4_wide)
nest_num_stage <- as.data.frame(nest_num_stage)
num_active_wide <- as.data.frame(num_active_wide)
visits <- as.data.frame(visits) 
focal_fail_dates_wide <- as.data.frame(focal_fail_dates_wide)
# set path for file to be written to
#zfile <- paste("S:/Databases/HEP_site_visits/codeAndSupportingFiles_for_HEP_screening/scoring_files/", paste(seas, col, sep="/"), "_scoring.xlsx", sep="")
zfile <- paste("codeAndSupportingFiles_for_HEP_screening/scoring_files/", paste(seas, col, sep="/"), seas, "_scoring_TEST20190716.xlsx", sep="")



### load these funtions to write the scoring file with the base sheets (export.hep()) and then the appender functions
## the functions are actually called (executed) below
export.peak_active=function(){
write.xlsx(peak_active, file=zfile, sheetName="peak_active", append=TRUE, row.names=FALSE)
}
export.stg4=function(){
  write.xlsx(stg4_wide, file=zfile, sheetName="stg4", append=TRUE, row.names=FALSE)
}  
export.nest_num_stage=function(){
  write.xlsx(nest_num_stage, file=zfile, sheetName="nest_num_stage", append=TRUE, row.names=FALSE)
}
export.num_active=function(){
  write.xlsx(num_active_wide, file=zfile, sheetName="num_active", append=TRUE, row.names=FALSE)
}

export.all.spp=function(){
  write.xlsx(hep.wide1, file=zfile, sheetName="nests", append=TRUE, row.names=FALSE)
  }
#---
export.1.spp = function(zspp){
  #zspp = "GREG"
  hep.wide1spp <- hep.wide1 %>% 
    filter(species == zspp)
  hep.wide1spp2 <- hep.wide1spp[, 3:(ncol(hep.wide1spp)-8)]
  hep.wide1spp2noempty <- hep.wide1spp2[, colSums(hep.wide1spp2 != "") != 0]
  hep.wide1spp3 <- cbind(hep.wide1spp[,1:2], 
                         hep.wide1spp2noempty,
                         hep.wide1spp[, (ncol(hep.wide1spp)-7):ncol(hep.wide1spp)])
  
  write.xlsx(hep.wide1spp3, file=zfile, sheetName= zspp, append=TRUE, row.names=FALSE)
  }
#---
export.multi.spp=function(){ 
  write.xlsx(hep.wide1[hep.wide1$num_spp>1,], file=zfile, sheetName="multiSpp", append=TRUE, row.names=FALSE)
  }
export.focfail=function(){
  write.xlsx(focal_fail_dates_wide, file=zfile, sheetName="focal_fail_dates", append=TRUE, row.names=FALSE)
}
export.visits=function(){
  write.xlsx(visits, file=zfile, sheetName="visits", append=TRUE, row.names=FALSE)
}


## !!!! important!! pause briefly between calling each of these functions, otherwise some 
## now export the base scoring file
export.peak_active()
export.stg4()
export.nest_num_stage()
export.num_active()
## and append the desired nest sheets to it
export.1.spp("BCNH")
export.1.spp("SNEG")
export.1.spp("GREG")
export.1.spp("GBHE")
export.1.spp("CAEG")
export.1.spp("DCCO")
export.multi.spp()
export.focfail()
export.visits()


## YOU SHOULD BE DONE HERE




export.bcnh=function(){
  write.xlsx(hep.wide1[hep.wide1$species=="BCNH",], file=zfile, sheetName="BCNH", append=TRUE, row.names=FALSE)
  }
export.caeg=function(){
  write.xlsx(hep.wide1[hep.wide1$species=="CAEG",], file=zfile, sheetName="CAEG", append=TRUE, row.names=FALSE)
  }
export.greg=function(){
  write.xlsx(hep.wide1[hep.wide1$species=="GREG",], file=zfile, sheetName="GREG", append=TRUE, row.names=FALSE)
  }
export.sneg=function(){
  write.xlsx(hep.wide1[hep.wide1$species=="SNEG",], file=zfile, sheetName="SNEG", append=TRUE, row.names=FALSE)
  }
export.gbhe=function(){
  write.xlsx(hep.wide1[hep.wide1$species=="GBHE",], file=zfile, sheetName="GBHE", append=TRUE, row.names=FALSE)
  }
export.dcco=function(){
  write.xlsx(hep.wide1[hep.wide1$species=="DCCO",], file=zfile, sheetName="DCCO", append=TRUE, row.names=FALSE)
  }

export.all.spp()
export.bcnh()
export.caeg()
export.greg()
export.sneg()
export.gbhe()
export.dcco()


############################################################################################################

### these generally won't be needed, but these are some functions for appending single worksheets to a file that already exists
## you'll have to delete that sheet if it already exists in the file
append.visits=function(seas, col){
  visits <- as.data.frame(visits)
  write.xlsx(visits, file=zfile, sheetName="visits", append=TRUE, row.names=FALSE)
  }
append.visits(seas, col)

append.focfail=function(seas, col){
  focal_fail_dates_wide <- as.data.frame(focal_fail_dates_wide)
  write.xlsx(focal_fail_dates_wide, file=zfile, sheetName="focal_fail_dates", append=TRUE, row.names=FALSE)
  }
append.focfail(seas, col)

append.num.stage=function(seas, col){
  nest_num_stage <- as.data.frame(nest_num_stage)
  write.xlsx(nest_num_stage, file=zfile, sheetName="nest_num_stage", append=TRUE, row.names=FALSE)
  }
append.num.stage(seas, col)


  ########