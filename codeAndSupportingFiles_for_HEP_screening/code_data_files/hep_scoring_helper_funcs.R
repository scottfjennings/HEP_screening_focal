

library(tidyverse)
library(lubridate)
library(hms)
code_version = 1.0

# 2 define colony, year -----------
#specify which colony and season you want to work with
# requires - nothing
col = "Alcatraz" # used by 16, 20
# col needs to match the site_name field in the code_data_files/sub_sites.csv file; if the colony you're working on doesn't have a site_name yet, you can create one that makes sense (a shorter version of the colony name, with no spaces for best use as a file name); be sure to add this new site_name to the sub_sites file.
col.code = "70" # used by 3.2, 4.2
seas = "2017" # used by 3.2, 4.2, 5, 7, 12, 16, 20

# 4 import site_visit data -----------
##importing from hep_raw access database and prelim data managment
## (in access, run HEP_individual nests Query, export to xls, then save as csv)
hep_all <- read.csv("codeAndSupportingFiles_for_HEP_screening/queried_from_access/HEP_individual nests Query.csv") 
hep_all=read.csv("alcatraz_hep/Alcatraz_ready4screening/alcatraz2016_4screening_codeV1.5_20190909.csv") %>% 
  rename(SpeciesCode = spp) %>% 
  mutate(code = col.code,
         date = ymd(as.character(date)),
         date = paste(month(date), day(date), year(date), sep = "/"))

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


# 4.1 colony + season filter for site_visit -----------
## filter to just the colony and season you want to work with
hep <- hep_all %>% 
  filter(code == col.code, year(date) == seas) %>%
  droplevels() %>% 
  arrange(species, nest, date)
# used in 4.2, 4.3, 4.4, 5, 6, 9.2, 9.4, 9.6-9.19, 11, 12, 15 
# 4.2 generate inferred status ---------------
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
# 4.3 combine observed and inferred status ------------------
# add those records back to the original data
hep_inf_status <- hep %>% 
  mutate(inf.status = "") %>% 
  bind_rows(., inf_status_only) %>% 
  mutate(inf.status = ifelse(month(date) < 4 & inf.status == "A" & stage == 0 & adults < 2, "P", inf.status))
# 4.4 ad.stg.chx.conf ---------------------
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


#################################
## look at long-format check data for a single nest
# znest.exact = TRUE returns an exact match of the nest number; FALSE returns partial string match, so e.g. if znest = 203, would return 203 and 203A

nest_viewer <- function(zspp, znest, znest.exact = FALSE) {
  if(znest.exact == FALSE) {
  hep %>% 
    filter(species == zspp & grepl(znest, nest)) %>% 
    arrange(date) %>% 
      select(species, nest, md, everything(), -code, -inf.status, -jdate, -date, -ad.stg.chx.conf)
  } else {
      hep %>% 
    filter(species == zspp, nest == znest) %>% 
    arrange(date) %>% 
      select(species, nest, md, everything(), -code, -inf.status, -jdate, -date, -ad.stg.chx.conf)
  }
}
nest_viewer("SNEG", "762")


#---
## check difference between 2 dates against certain species-specific period lengths
date_ranger <- function(zspp, date1, date2) {
  # create df with the minimum fledging age and duration of incubation for each species
  sp_stats1 = data.frame(species = c('BCNH', 'SNEG', 'CAEG', 'GREG', 'GBHE', 'DCCO'),
                       inc2min.fl = as.numeric(c('40', '34', '38', '77', '84', NA)),
                       inc.dur = as.numeric(c('25', '22', '24', 	'28', '28', NA)),
                       inc2min.stg4 = as.numeric(c('35', '32', '34', 	'49', '49', NA)),
                       hatch2min.fl = as.numeric(c("15", "14", "14", "49", "56", NA)))
  sp_stats2 <- filter(sp_stats1, species == zspp)
  foo <- as.Date(paste(seas, date2, sep = "-")) - as.Date(paste(seas, date1, sep = "-"))
  foo2 <- data.frame(date.span = foo,
                     diff.from.inc2min.fl = foo - sp_stats2$inc2min.fl,
                     diff.from.inc.dur = foo - sp_stats2$inc.dur,
                     diff.from.inc2min.stg4 = foo - sp_stats2$inc2min.stg4,
                     diff.from.hatch2min.fl = foo - sp_stats2$hatch2min.fl)
  return(foo2)
}
date_ranger("SNEG", "4-27", "6-18")

#----


mid_pointer <- function(zspp, last.inc, fist.brood, last.brood, first.inactive) {
  # create df with the minimum fledging age and duration of incubation for each species
  sp_stats1 = data.frame(species = c('BCNH', 'SNEG', 'CAEG', 'GREG', 'GBHE', 'DCCO'),
                       inc2min.fl = as.numeric(c('40', '34', '38', '77', '84', NA)),
                       inc.dur = as.numeric(c('25', '22', '24', 	'28', '28', NA)),
                       inc2min.stg4 = as.numeric(c('35', '32', '34', 	'49', '49', NA)),
                       hatch2min.fl = as.numeric(c("15", "14", "14", "49", "56", NA)))
  
  sp_stats2 <- filter(sp_stats1, species == zspp)

  hatch.span <- as.Date(paste(seas, fist.brood, sep = "-")) - as.Date(paste(seas, last.inc, sep = "-"))
  mid.hatch <- as.Date(paste(seas, last.inc, sep = "-")) + floor(hatch.span/2)
  
  fl.fa.span <- as.Date(paste(seas, first.inactive, sep = "-")) - as.Date(paste(seas, last.brood, sep = "-"))
  mid.fl.fa <- as.Date(paste(seas, last.brood, sep = "-")) + floor(fl.fa.span/2)
  mids.span <- mid.fl.fa - mid.hatch
  foo2 <- mids.span - sp_stats2$hatch2min.fl
 
  zoutcome <- (
    if(mids.span < sp_stats2$hatch2min.fl) {
    paste("Based on midpoints nest failed ", abs(foo2), " days short of fledging.", sep = "")  
    } else {
     paste("Based on midpoints nest reached ", foo2, " days past fledging.", sep = "") 
    }
  )
   foo3 <- data.frame(Estimated.hatch = mid.hatch,
                     Estimated.fledge.fail = mid.fl.fa,
                     Days.between.mids = mids.span,
                     outcome = zoutcome)
  return(foo3)
}
mid_pointer(zspp = "BCNH",
            last.inc = "5-18",
            fist.brood = "5-25",
            last.brood = "6-8",
            first.inactive = "6-8")
