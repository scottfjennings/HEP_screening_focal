

## code to transfer data from the HEP screening format to the HEP access database format
#####################################

#########################
library(readxl)
library(plyr)
library(tidyverse)
library(lubridate)


####!!!!!!!!!!!!!!!!CHANGE HERE!!!!!!!!!!!!!!!!####
seas="2019"
####!!!!!!!!!!!!!!!!CHANGE HERE!!!!!!!!!!!!!!!!####

file.loc <- paste("codeAndSupportingFiles_for_HEP_screening/scoring_files/", seas, sep = "")
file.list <- list.files(file.loc, pattern='*.xlsx', full.names=T, include.dirs = FALSE)

#codeAndSupportingFiles_for_HEP_screening/scoring_files/2019/BodegaBayFlat12019_scoring.xlsx

import_scoring = function(filename){
sheet.x = read_xlsx(filename, sheet = sheet) 
if ("use_for_count" %in% colnames(sheet.x) & "first_check_fl_fa" %in% colnames(sheet.x)) {
  sheet.x2 <- sheet.x %>% 
    select(species, nest, focal, focal_fail, fledged, use_for_count, stg4_brood, days_short_of_fledging, active_last_day, max_stage, num_spp, first_check_fl_fa) %>% 
    mutate_all(as.character)
} else {
  sheet.x2 <- sheet.x %>% 
    mutate(use_for_count = "",
           first_check_fl_fa = "",
           inc_start_by_stg4 = "") %>% 
    select(species, nest, focal, focal_fail, fledged, use_for_count, stg4_brood, days_short_of_fledging, active_last_day, max_stage, num_spp, first_check_fl_fa)  %>% 
    mutate_all(as.character)
}
sheet.x2$site_name=substr(filename, 61, nchar(filename)-17)
#sheet.x$site_name=filename
return(sheet.x2)
}
sheet <- "GBHE"
gbhe <- map_df(file.list, import_scoring)

######################################

import_checks = function(filename){
  
  filename = "codeAndSupportingFiles_for_HEP_screening/scoring_files/2019/Bolinas2019_scoring.xlsx"
sheet.x = read_xlsx(filename, sheet = sheet) 
if ("use_for_count" %in% colnames(sheet.x) & "first_check_fl_fa" %in% colnames(sheet.x)) {
  sheet.x2 <- sheet.x[, 1:(ncol(sheet.x)-11)]
} else {
sheet.x2 <- sheet.x[, 1:(ncol(sheet.x)-8)]
}

sheet.x2.wide <- sheet.x2 %>% 
  gather(date, check.conts, -species, -nest)

sheet.x2$site_name=substr(filename, 61, nchar(filename)-17)
#sheet.x$site_name=filename
return(sheet.x2)
}

sheet <- "GBHE"
gbhe.checks <- map_df(file.list, import_checks)

bodega_gbhe_checks <- import_checks("codeAndSupportingFiles_for_HEP_screening/scoring_files/2019/BodegaBayFlat12019_scoring.xlsx")

bo_gbhe_checks <- import_checks("codeAndSupportingFiles_for_HEP_screening/scoring_files/2019/Bolinas2019_scoring.xlsx")





