
## for hep_screening v2
## this file will just create worksheets for:
# 1) a scoring sheet for each species, 
# 2) multi-species scoring sheet
# 3) screening.log

# 1 --------------------
## load the required packages
library(plyr)
library(tidyverse)
library(reshape2)
library(lubridate)
library(xlsx)
library(readxl)
library(hms)

library(daff)

# 2 -----------
#specify which colony and season you want to work with
# requires - nothing
col = "Bolinas" # used by 16, 20
# col needs to match the site_name field in the code_data_files/sub_sites.csv file; if the colony you're working on doesn't have a site_name yet, you can create one that makes sense (a shorter version of the colony name, with no spaces for best use as a file name); be sure to add this new site_name to the sub_sites file.
col.code = "53" # used by 3.2, 4.2
seas = "2019" # used by 3.2, 4.2, 5, 7, 12, 16, 20


# 3 -----------
####  STARTING WITH THE VISITS DATA
hep_visits <- read_xlsx("codeAndSupportingFiles_for_HEP_screening/queried_from_access/HEP_visits Query.xlsx", "HEP_visits_Query") # used by 3.1, 3.2
#hep_visits <- read_csv("HEP_screening/queried_from_access/HEP_visits Query.csv")

names(hep_visits) <- tolower(names(hep_visits))
hep_visits <- hep_visits %>% 
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

#hep_visits <- bind_rows(hep_visits, csv_visits)
# used by 3.1
# 3.1 ----------------------
# filter to colony and year
# uses hep_visits from 3
hep_visits_sub <- hep_visits %>% 
  filter(code == col.code, year(date) == seas) %>%
  droplevels()
# used by 3.2, 3.3

# 3.3 ------------
# extract the number of active nests counted in the field
# uses hep_visits_sub from 3.2
counted_active <- hep_visits_sub %>% 
  select(date, species = speciescode, counted.active = active)
# used in 6
# 4 -----------
##importing from hep_raw access database and prelim data managment
## (in access, run HEP_individual nests Query, export to xls, then save as csv)
hep_all <- read.csv("codeAndSupportingFiles_for_HEP_screening/queried_from_access/HEP_individual nests Query.csv") 
names(hep_all) <- tolower(names(hep_all))
## data management
# options for 2015 and 2016 data
# uses hep_all from 4
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
# 4.1 -----------
## filter to just the colony and season you want to work with
hep <- hep_all %>% 
  filter(code == col.code, year(date) == seas) %>%
  droplevels() %>% 
  arrange(species, nest, date)
# used in 4.2, 4.3, 4.4, 5, 6, 9.2, 9.4, 9.6-9.19, 11, 12, 15 
# 4.2 ---------------
## these 2 together fill in days where a nest was missed but other nests for that species were checked
# uses hep from 4.1
spp_dates <- hep %>% 
  distinct(species, date) %>%
  mutate(sp.surveyed = "Y")
# only used in 4.2
hep_breaks <- hep %>% 
  select(species, nest, date, stage) %>% 
  arrange(species, nest, date)%>% 
  spread(date, stage) %>% 
  gather(date, stage, -species, -nest) %>% 
  mutate(date = as.Date(date))%>% 
  full_join(., spp_dates) %>% 
  filter(!is.na(sp.surveyed)) %>% 
  arrange(species, date)
# only used in 4.2
# identify where a nest was missed and the checks before and after had stage > 0
id_breaks <- hep_breaks %>% 
  arrange(species, nest, date) %>% 
  group_by(species, nest) %>% 
  mutate(prev.stage = lag(stage),
         next.stage = lead(stage),
         inf.status = NA,
         inf.status = ifelse(is.na(stage) & 
                               (prev.stage > 0 & prev.stage < 8) & 
                               (next.stage > 0 & next.stage < 8), "A", inf.status))
# only used in 4.2
inf_status_only <- id_breaks %>% 
  select(species, nest, date, inf.status) %>% 
  filter(!is.na(inf.status)) %>% 
  droplevels() %>% 
  ungroup()
# used by 4.3
# 4.3 ------------------
# add those records back to the original data
hep_inf_status <- hep %>% 
  mutate(inf.status = "") %>% 
  bind_rows(., inf_status_only) %>% 
  mutate(inf.status = ifelse(month(date) < 4 & inf.status == "A" & stage == 0 & adults < 2, "P", inf.status))
# 4.4 ---------------------
## and make a few new fields
# uses hep_inf_status from 4.2
hep <- hep_inf_status %>% 
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
# used in 5, 6, 9.2, 9.4, 9.6-9.19, 11, 12, 15

# 6 -----------
## count how many nests were present for each species on each day
## num.active is whichever is greatest between # active nests recorded on front of sheet and number of nests from back of sheet
nests_per_day <- hep %>% 
  filter((status == "A" | inf.status == "A") & inf.status != "P") %>%
  group_by(species, date) %>%	
  summarise(calc.active = length(nest)) %>% 
  full_join(., counted.active) %>% 
  mutate(num.active = ifelse(calc.active > counted.active, calc.active, counted.active)) 

# 8 -----------
## determine the date with the peak number of active nests, and PEAKACTVNSTS
## gets written to final xls
peak_active <- nests_per_day %>% 
  group_by(species) %>%
  mutate(num.active = replace_na(num.active, 0)) %>% 
	filter(num.active == max(num.active)) %>%
	filter(date == min(date)) %>% 
  mutate(peak.active.jdate = yday(date)) %>% 
  select(species, peak.active.date = date, peak.active.jdate, PEAKACTVNSTS = num.active)
# used in 9.3
# 9 -----------
## make the df used by the nest screening algorithm
make_nests4screening <- function(){
# all "9.x" code chunks are inside this function
# 9.1 --------
# create df with the minimum fledging age and duration of incubation for each species
  sp_stats1 = data.frame(species = c('BCNH', 'SNEG', 'CAEG', 'GREG', 'GBHE', 'DCCO'),
                       min.fl = as.numeric(c('40', '34', '38', '77', '84', NA)),
                       inc.dur = as.numeric(c('25', '22', '24', 	'28', '28', NA)),
                       min.stg4 = as.numeric(c('35', '32', '34', 	'49', '49', NA)))
  # goes to 9.3

# make several fields that will be used for automated screening 
# 9.2 -------
# determine the latest date of observations for each species
  # uses hep from 4.4
spp_max_date = hep %>% 
	filter(status=="A") %>%
    group_by(species) %>%
	summarise(spp.max.date = max(date)) 
# goes to 9.3
# 9.3 ------
# create a df with the general species info (min.fl and inc.dur) and the colony-specific species info (peak_active and spp_max_jdate)
# uses peak_active from 8, spp_max_date from 9.2
spp_stats <- full_join(peak_active, spp_max_date) %>% 
                  inner_join(., sp_stats1)
# goes to 9.5
# 9.4 -------
# create unique list of each species+nest combo
  # uses hep from 4.4
spp_nests2 = unique(hep[c("species", "nest")])
# goes to 9.5
# 9.5 -----
#  merge unique nest list with the species info
spp_nests1 <- full_join(spp_nests2, spp_stats)
# goes to 9.20 - end
# 9.6 ------
# ID nests that were assigned to more than 1 species
multi_spp_nests <- hep %>% 
	filter(status == "A") %>%
    group_by(nest) %>%
    summarise(num.spp = n_distinct(species))	
# goes to 9.20 - end
# 9.7 ----
# determin the maximum stage each nest was observed in	
max_stage <- hep %>% 
	filter(status == "A" & stage < 8) %>%
    group_by(species, nest) %>%	
	summarise(max.stage = max(stage)) 	
# goes to 9.20 - end
# 9.8 ----------	
# determin the minimum stage each nest was observed in		
min_stage <- hep %>% 
	filter(status == "A") %>%
    group_by(species, nest) %>%	
	summarise(min.stage = min(stage)) 	
# goes to 9.20 - end
# 9.9 ----------	
# determin the minimum stage each nest was observed with chick		
min_stage_chx <- hep %>% 
	filter(status == "A", stage > 1 & stage < 8) %>%
    group_by(species, nest) %>%	
	summarise(min.stage.chx = min(stage)) 	
# goes to 9.20 - end
# 9.10 ----------
# determin the latest date each nest was observed	
latest_date <- hep %>% 
	filter(status == "A") %>%
    group_by(species, nest) %>%	
	summarise(latest.date = max(date)) 	
# goes to 9.20 - end
# 9.11 ----------
# determin the earliest date each nest was observed	
earliest_date <- hep %>% 
	filter(status == "A") %>%
    group_by(species, nest) %>%	
	summarise(earliest.date = min(date)) 
# 9.12 ----------
# determin the latest date each nest was observed in stage 1	
latest_date_stage1 <- hep %>% 
	filter(status == "A") %>%
    group_by(species, nest) %>%
	filter(stage == 1) %>%
	summarise(latest.date.stage1 = max(date)) 
# 9.13 ----------
# determin the latest date each nest was observed in stage 4 or later
latest_date_unguarded <- hep %>% 
	filter(status == "A") %>%
    group_by(species, nest) %>%
	filter(stage > 3) %>%
	summarise(latest.date.unguarded = max(date)) 
# 9.14 ----------
# determin the earliest date each nest was observed in stage 1
earliest_date_stage1 <- hep %>% 
	filter(status == "A") %>%
    group_by(species, nest) %>%
	filter(stage == 1) %>%
	summarise(earliest.date.stage1 = min(date))	
# 9.15 ----------
# determin the earliest date each nest was observed with chicks (later than stage 1)
earliest_date_chx <- hep %>% 
	filter(status == "A") %>%
    group_by(species, nest) %>%
	filter(stage > 1 & stage < 8) %>%
	summarise(earliest.date.chx = min(date))	
# 9.16 ----------
# determin the stage 4 brood size of each nests (where knowable)
stg4_brood <- hep %>% 
	filter(status == "A") %>%
  group_by(species, nest) %>%	
	filter(stage == 4, confidence == 1) %>%
	filter(date == max(date)) %>%
	select(species, nest, stg4.brood = chicks) 
# 9.17 ----------
# determin the earliest stage a nest was observed in	
earliest_stage = hep %>% 
	filter(status == "A") %>%
    group_by(species, nest) %>%	
	filter(date == min(date)) %>%
	select(species, nest, earliest.stage = stage) 
# 9.18 ---------
# determine first date observed failed or fledged (first date status != A following date with status == A)
spp_date_full_matrix <- hep %>% 
  select(species, nest, date) %>% 
  mutate(checked = "Y") %>% 
  spread(date, checked) %>% 
  replace(., is.na(.), "N")

spp_date_full_matrix_long <- spp_date_full_matrix %>% 
  gather(date, checked, -species, -nest) %>% 
  arrange(species, nest, date) %>% 
  mutate(date = as.Date(date))

nest_checked <- hep %>% 
  select(species, nest, date, status) %>% 
  full_join(., spp_date_full_matrix_long) %>% 
  arrange(species, nest, date) 

fl_fa_date <- nest_checked %>% 
  group_by(species, nest) %>% 
  mutate(next.check = lead(date),
         checked.next.check = lead(checked)) %>% 
  filter(status == "A") %>% 
  filter(date == max(date)) %>% 
  select(species, nest, first.check.fl.fa = next.check)
# 9.19 ----------	
# determine first day stage == 4
earliest_stg4 <- hep %>% 
  filter(stage == 4) %>% 
  group_by(species, nest) %>% 
  filter(date == min(date)) %>% 
  select(species, nest, earliest.stg4 = date)
# 9.20 ----------	
## bind all those created objects
nests1 <- join_all(list(spp_nests1, earliest_date_chx, earliest_stage, min_stage_chx, earliest_date_stage1, latest_date_stage1, latest_date_unguarded, max_stage, latest_date, earliest_date, stg4_brood, fl_fa_date, earliest_stg4), type = 'full')
nests <- join_all(list(nests1, multi_spp_nests), type = 'full')

  
#----------
return(nests)
}
nests4screening <- make_nests4screening()

# 10 -----------
# make automated screening calculations
# uses nests4screeing from 9
screened_nests <- nests4screening %>%  
  mutate(active.last.day = ifelse(latest.date == spp.max.date, 1, 0), # was the nest still active on the last day of nest checks for this species?
	#meets criteria for focal nest 
	focal = ifelse(earliest.date <= peak.active.date & # nest was observed active before peak colony size
								earliest.stage < 2 & # nest was observed before hatching
								max.stage > 0 & # nest actually reached incubation or brooding
									(((earliest.date <= round(earliest.date.chx - (inc.dur - 14), 0)) & min.stage.chx < 4 ) | # nest was first observed during the first 2 weeks of incubation or earlier
										(earliest.date <= round(latest.date.stage1 - (inc.dur - 14),0))), 1, 0),# or the first day chicks were observed they were still being guarded
	fledged = ifelse(latest.date.unguarded - earliest.date.stage1 > min.fl, 1, 0), # did the nest reach the minimum fledging age
	fledged = ifelse(is.na(fledged), 0, fledged),
	focal.fail = ifelse(focal == 1 & active.last.day == 0 & fledged == 0, 1, 0), # was it a focal nest that failed?
	days.short.of.fledging = ifelse(max.stage > 3 & fledged == 0,(latest.date.unguarded-earliest.date.stage1) - min.fl, ""),#count number of days short of fledging the nest was observed
	inc.start.by.stg4 = earliest.stg4 - min.stg4) # conservative date of start of incubation, assuming first day stage 4 was observed was first day nest was stage 4



# 12 -----------
# make the nest Scoring Form
make_nest_scoring <- function(){
  # extract just the fields from 'nests' needed to export
  nests.sub <- screened_nests %>% 
    select(species, nest, focal, focal.fail, fledged, days.short.of.fledging, active.last.day, max.stage, num.spp, stg4.brood, first.check.fl.fa, inc.start.by.stg4)
  
hep.wide <- hep %>% 
  arrange(species, date) %>% 
  select(date, species, nest, ad.stg.chx.conf) %>%
  unique() %>% 
  spread(date, ad.stg.chx.conf)
names(hep.wide) <- gsub(paste(seas, "-", sep = ""), "", names(hep.wide)) # remove year from fields so they fit better with only month-day

hep.wide1=full_join(hep.wide, nests.sub) %>% 
  mutate(use.for.count = ifelse(focal.fail > 0 | fledged > 0 | max.stage > 0, 1, 0),
         first.check.fl.fa = gsub(paste(seas, "-", sep = ""),"", first.check.fl.fa),
         inc.start.by.stg4 = gsub(paste(seas, "-", sep = ""),"", inc.start.by.stg4))

  hep.wide1.left <- hep.wide1[, 1:(ncol(hep.wide1)-11)]
  hep.wide1.right <- hep.wide1[, (ncol(hep.wide1)-10):ncol(hep.wide1)] %>% 
    select(focal, focal.fail, fledged, use.for.count, stg4.brood, everything())
  hep.wide2 <- cbind(hep.wide1.left, hep.wide1.right)

return(hep.wide2)
}
hep.wide1 <- make_nest_scoring()





## 14 ----------
## replace all the NAs with blank to make the xls output tidier
hep.wide1[is.na(hep.wide1)] <- ""
focal_fail_dates_wide[is.na(focal_fail_dates_wide)] <- ""
## getting this error is OK; it just means there were no focal failures
# Error in focal_fail_dates_wide[is.na(focal_fail_dates_wide)] <- "" : 
# object 'focal_fail_dates_wide' not found
nest_num_stage[is.na(nest_num_stage)] <- ""
num_active_wide[is.na(num_active_wide)] <- ""
stg4_wide[is.na(stg4_wide)] <- ""




# 16 -------------------
# make all the tbls into dfs for write.xlsx  
hep.wide1 <- as.data.frame(hep.wide1)

# set path for file to be written to
#zfile <- paste("S:/Databases/HEP_site_visits/codeAndSupportingFiles_for_HEP_screening/scoring_files/", paste(seas, col, sep="/"), "_scoring.xlsx", sep="")
zfile <- paste("codeAndSupportingFiles_for_HEP_screening/scoring_files/", paste(seas, col, sep="/"), seas, "_scoring.xlsx", sep="")



# 17 -------------
### load these funtions to write the scoring file with the base sheets (export.hep()) and then the appender functions
## the functions are actually called (executed) below
export.1.spp = function(zspp){
  #zspp = "GREG"
  hep.wide1spp <- hep.wide1 %>% 
    filter(species == zspp)
  hep.wide1spp2 <- hep.wide1spp[, 3:(ncol(hep.wide1spp)-11)]
  hep.wide1spp2noempty <- hep.wide1spp2[, colSums(hep.wide1spp2 != "") != 0]
  hep.wide1spp3 <- cbind(hep.wide1spp[,1:2], 
                         hep.wide1spp2noempty,
                         hep.wide1spp[, (ncol(hep.wide1spp)-10):ncol(hep.wide1spp)])
  
  write.xlsx(hep.wide1spp3, file=zfile, sheetName= zspp, append=TRUE, row.names=FALSE)
  }
#---
export.multi.spp=function(){ 
    hep.wide1mspp <- hep.wide1 %>% 
      filter(num.spp > 1) %>% 
      select(everything(), -first.check.fl.fa, inc.start.by.stg4, use.for.count) %>% 
      arrange(nest, species)
  write.xlsx(hep.wide1mspp, file=zfile, sheetName="multiSpp", append=TRUE, row.names=FALSE)
}
#---
export.screen.log=function(){ 
    screen.log <- data.frame(screen.date = "",
                             screen.initials = "",
                             screen.notes = "")
  write.xlsx(screen.log, file=zfile, sheetName="screen_log", append=TRUE, row.names=FALSE)
}

# 18 ------
# call functions to export to xlsx
export.screen.log()
export.1.spp("BCNH")
export.1.spp("SNEG")
export.1.spp("GREG")
export.1.spp("GBHE")
export.1.spp("CAEG")
export.1.spp("DCCO")
export.multi.spp()



