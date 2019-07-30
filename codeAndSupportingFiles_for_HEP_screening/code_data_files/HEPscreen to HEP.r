

## code to transfer data from the HEP screening format to the HEP access database format
#####################################

#########################
library(readxl)
library(plyr)
library(tidyverse)
library(lubridate)


####!!!!!!!!!!!!!!!!CHANGE HERE!!!!!!!!!!!!!!!!####
seas="2018"
####!!!!!!!!!!!!!!!!CHANGE HERE!!!!!!!!!!!!!!!!####
get_col_name <- function(filename) {
colony <- filename %>% 
  gsub(paste(file.loc, "/", sep = ""), "", .) %>% 
  gsub(seas, "", .) %>% 
  gsub("_scoring.xlsx", "", .) %>% 
  gsub("_scored.xlsx", "", .)
return(colony)
}
file.loc <- paste("codeAndSupportingFiles_for_HEP_screening/scoring_files/", seas, "/Scored", sep = "")
file.list <- list.files(file.loc, pattern='*.xlsx', full.names=T, include.dirs = FALSE)

# check sheets in each file -----
get_sheetz_in_file <- function(filename) {
colony <- get_col_name(filename)
file_sheets <- excel_sheets(filename) %>% 
  data.frame() %>% 
  rename(sheet = 1) %>% 
  mutate(in.file = "Y",
         site.name = colony)
return(file_sheets)
}
sheets_in_files <- map_df(file.list, get_sheetz_in_file)
sheets_in_files_wide <- sheets_in_files %>% 
  spread(sheet, in.file) %>% 
  select(site.name, GBHE, GREG, SNEG, BCNH, CAEG, DCCO, everything())
# basic import --------
import = function(filename){
colony <- get_col_name(filename)

if (sheet %in% excel_sheets(filename)) {
  sheet.x = read_xlsx(filename, sheet = sheet) 
  sheet.x <- sheet.x %>% 
    mutate(site.name = colony) %>% 
    mutate_all(as.character)
} 
}

# visits ------
sheet = "visits"
zvisits <- map_df(file.list, import)
visits <- zvisits %>% 
  rename(NUMBERVISITS = days, TOTALHOURS = hours)
# stg4 ------
sheet = "stg4"
stg4 <- map_df(file.list, import)

# peak_active ------
sheet = "peak_active"
peak_active <- map_df(file.list, import)

# nest_num_stage ------
import_nest_num_stage = function(filename){
colony <- get_col_name(filename)

if (sheet %in% excel_sheets(filename)) {
  sheet_x = read_xlsx(filename, sheet = "nest_num_stage") 
  sheet_x <- sheet_x %>% 
    mutate(stage = as.character(stage))
  sheet_x_long <- sheet_x %>% 
    gather(md, NESTS, -species, -stage) %>% 
    mutate(date1 = ifelse(!grepl("-", md), md, NA),
           date1 = as.numeric(date1),
           date2 = as.Date(date1, origin ="1899-12-30"),
           date3 = ifelse(is.na(date1), 
                         as.Date(paste(seas, md, sep = "-")),
                         date2),
           date = as.Date(date3, origin = "1970-01-01")) %>% 
    select(-date1, -date2, -date3) %>% 
    filter(NESTS > 0) %>% 
    filter(stage > 0) %>% 
    arrange(species, stage, date) %>% 
    mutate(site.name = colony) %>% 
    mutate_all(as.character)
} 
}
sheet = "nest_num_stage"
znest_num_stage <- map_df(file.list, import_nest_num_stage)
nest_num_stage <- znest_num_stage %>% 
  mutate(date = as.Date(date))
rm(znest_num_stage)
# num_active ------
import_num_active = function(filename){
colony <- get_col_name(filename)

if (sheet %in% excel_sheets(filename)) {
  sheet_x = read_xlsx(filename, sheet = "num_active")
  sheet_x_long <- sheet_x %>% 
    gather(md, NESTS, -species)%>%
    mutate(date1 = ifelse(!grepl("-", md), md, NA),
           date1 = as.numeric(date1),
           date2 = as.Date(date1, origin ="1899-12-30"),
           date3 = ifelse(grepl("-", md), md, NA),
           date4 = ifelse(is.na(date1), paste(seas, date3, sep = "-"), NA),
           date4 = as.Date(date4),
           date = ifelse(is.na(date1), date4, date2),
           date = as.Date(date, origin = "1970-01-01")) %>% 
    select(-date1, -date2, -date3, -date4) %>% 
    mutate(site.name = colony) %>% 
    filter(NESTS > 0) %>% 
    arrange(site.name, species, date) %>% 
    mutate_all(as.character)
} 
}
sheet = "num_active"
znum_active <- map_df(file.list, import_num_active)
num_active <- znum_active %>% 
  mutate(date = as.Date(date))
rm(znum_active)
# focal_fail_dates -- maybe not needed??? ------
import_focal_fail_dates = function(filename){
colony <- get_col_name(filename)
if (sheet %in% excel_sheets(filename)) {
  sheet_x = read_xlsx(filename, sheet = "focal_fail_dates")
  sheet_x_long <- sheet_x %>% 
    gather(md, NESTS, -species)%>%
    mutate(date1 = ifelse(!grepl("-", md), md, NA),
           date1 = as.numeric(date1),
           date2 = as.Date(date1, origin ="1899-12-30"),
           date3 = ifelse(grepl("-", md), md, NA),
           date4 = ifelse(is.na(date1), paste(seas, date3, sep = "-"), NA),
           date4 = as.Date(date4),
           date = ifelse(is.na(date1), date4, date2),
           date = as.Date(date, origin = "1970-01-01")) %>% 
    select(-date1, -date2, -date3, -date4) %>% 
    mutate(site.name = colony) %>% 
    filter(NESTS > 0) %>% 
    arrange(site.name, species, date) %>% 
    mutate_all(as.character)
  
  sheet_x_long2 <- sheet_x_long %>% 
    mutate(date = ifelse(year(date) == seas,
                          date,
                          paste(seas, month(date), day(date), sep = "-")),
           date = as.Date(date))
} 
}
sheet = "focal_fail_dates"
zfocal_fail_dates <- map_df(file.list, import_focal_fail_dates)
focal_fail_dates <- zfocal_fail_dates %>% 
  mutate(date = as.Date(date))
rm(zfocal_fail_dates)
# matching data to standard survey periods ------

## get one survey date per species per site
sites.visits.spp <- nest_num_stage %>% 
  distinct(site.name, date, species) %>%
  select(site.name, species, date) %>% 
  mutate(date = as.Date(date))

make_survey_periods <- function() {
## define the mid-point for the survey periods
early.mar <- as.Date(paste(seas, "3-10", sep="-")) # day options: 10-12
early.apr <- as.Date(paste(seas, "4-11", sep="-")) # day options: 10-12
early.may <- as.Date(paste(seas, "5-11", sep="-")) # day options: 10-12
early.jun <- as.Date(paste(seas, "6-3", sep="-")) # day options: 3-5
late.jun <- as.Date(paste(seas, "6-20", sep="-")) # day options: 20/22
july <- as.Date(paste(seas, "7-11", sep="-")) 

survey_periods <- data.frame(surv.per.date = c(early.mar, early.apr, early.may, early.jun, late.jun, july),
                             per.name = c("MAR", "APR", "MAY", "JUN", "LTJUN", "JUL"))

survey_periods <- survey_periods %>% 
  mutate(prev.date = lag(surv.per.date),
         next.date = lead(surv.per.date),
         prev.date = as.Date(ifelse(is.na(prev.date), 
                                    surv.per.date - 30, 
                                    prev.date), 
                             origin ="1970-01-01"),
         next.date = as.Date(ifelse(is.na(next.date), 
                                    surv.per.date + 30, 
                                    next.date), 
                             origin ="1970-01-01"),
         filt.start = surv.per.date - (surv.per.date - prev.date)/2,
         filt.end = surv.per.date + (next.date - surv.per.date)/2) %>% 
  select(per.name, surv.per.date, filt.start, filt.end)
return(survey_periods)
}
survey_periods <- make_survey_periods()
make.month.stages <- function(zper.name){
  perz <- survey_periods %>% 
    filter(per.name == zper.name)
  zper.column.name <- ifelse(perz$per.name == "LTJUN", 
                             paste(perz$per.name, "STGDATE", sep = ""),
                             paste(perz$per.name, "STGEDATE", sep = ""))
  
 surveys <- sites.visits.spp %>% 
   filter(date >= perz$filt.start & date < perz$filt.end) %>% 
    group_by(site.name, species) %>% 
    mutate(diff.surv.per.date = abs(date - perz$surv.per.date)) %>% 
   filter(diff.surv.per.date == min(diff.surv.per.date))

  surv.per.data <- inner_join(nest_num_stage, surveys, by=c("site.name", "species", "date"))  %>% 
    mutate(stage = paste(perz$per.name, "STAGE", stage, sep=""),
           NESTS = as.numeric(as.character(NESTS)))
  surv.per.data.wide <- spread(surv.per.data, stage, NESTS) %>% 
    rename(!!zper.column.name := date) %>% 
    select(-md, -diff.surv.per.date) %>% 
    arrange(site.name, species)
  surv.per.data.wide[is.na(surv.per.data.wide)] <- "0" 
  return(surv.per.data.wide)
}

e.mar<-make.month.stages("MAR")
e.apr<-make.month.stages("APR")
e.may<-make.month.stages("MAY")
e.jun<-make.month.stages("JUN")
l.jun<-make.month.stages("LTJUN")
jul<-make.month.stages("JUL") 

## if you get this error for any of these:
## Error in summarise_impl(.data, dots) : expecting a single value
## or
## Error in summarise_impl(.data, dots) : Column `surv.per.date` must be length 1 (a summary value), not 2
## it means the surv.per date was equidistant between 2 actual surveys
## the fix is to adust the surv.per for that period by 1 day

## getting the message
## Using NESTS as value column: use value.var to override.
## is fine


# TOTALSPECIES ----------------
total.spp <- visits %>% 
  group_by(site.name) %>% 
  summarise(TOTALSPECIES = n())

# species sheets --------
import_spp_sheets = function(filename){
colony <- get_col_name(filename)

if (sheet %in% excel_sheets(filename)) {
  sheet.x = read_xlsx(filename, sheet = sheet) 
  sheet.x <- sheet.x %>% 
    select(species,	nest,	focal, focal_fail, fledged, num_spp, stg4_brood) %>% 
    mutate(site.name = colony) %>% 
    mutate_all(as.character) 
} 
}
sheet = "GBHE"
gbhe_scoring <- map_df(file.list, import_spp_sheets)
sheet = "GREG"
greg_scoring <- map_df(file.list, import_spp_sheets)
sheet = "SNEG"
sneg_scoring <- map_df(file.list, import_spp_sheets)
sheet = "BCNH"
bcnh_scoring <- map_df(file.list, import_spp_sheets)
sheet = "CAEG"
caeg_scoring <- map_df(file.list, import_spp_sheets)
sheet = "DCCO"
dcco_scoring <- map_df(file.list, import_spp_sheets)

spp_sheet <- rbind(gbhe_scoring, greg_scoring, sneg_scoring, bcnh_scoring, caeg_scoring, dcco_scoring) %>% 
  arrange(site.name, species, nest)

# tally focal, focal_fail, brdX -----

all_spp_focal = spp_sheet %>%
  filter(!is.na(species)) %>%
  filter(focal == 1)

foc_tally_spp_colony <- all_spp_focal %>% 
  group_by(site.name, species) %>% 
  summarise(FOCALNESTS = n())
foc_fail_tally_spp_colony <- all_spp_focal %>% 
  filter(focal_fail == 1) %>% 
  group_by(site.name, species) %>% 
  summarise(FOCFAILURE = n())

stg4_tallies <- spp_sheet %>% 
  filter(stg4_brood > 0) %>% 
  group_by(site.name, species, stg4_brood) %>% 
  summarise(num.nests = n())

##------------------------------------------------
sub_sites <- read.csv("codeAndSupportingFiles_for_HEP_screening/code_data_files/sub_sites.csv")

new_hep <- visits %>% 
  full_join(., foc_tally_spp_colony) %>% 
  full_join(., foc_fail_tally_spp_colony) %>% 
  full_join(., total.spp) %>% 
  full_join(., select(peak_active, site.name, species, PEAKACTVNSTS)) %>% 
  full_join(., stg4) %>% 
  full_join(., e.mar) %>% 
  full_join(., e.apr) %>% 
  full_join(., e.may) %>% 
  full_join(., e.jun) %>% 
  full_join(., l.jun) %>% 
  full_join(., jul) %>% 
  mutate(YEAR = seas) %>% 
  left_join(., sub_sites) %>% 
  select(site.name, CODE, YEAR, SITE, SPECIES = species, everything()) %>% 
  arrange(site.name, SPECIES)


write.csv(new_hep, paste("C:/Users/scott.jennings/Documents/Projects/HEP/HEP_data_work/HEP_data/HEP_", seas, "_from_screen_code.csv", sep = ""))

