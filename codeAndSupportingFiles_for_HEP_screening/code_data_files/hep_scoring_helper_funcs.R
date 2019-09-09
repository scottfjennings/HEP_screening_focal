
# 4 import site_visit data -----------
##importing from hep_raw access database and prelim data managment
## (in access, run HEP_individual nests Query, export to xls, then save as csv)
hep_all_foo <- read.csv("codeAndSupportingFiles_for_HEP_screening/queried_from_access/HEP_individual nests Query.csv") 
hep_all=read.csv("alcatraz_hep/Alcatraz_ready4screening/alcatraz2018_4screening20190908.csv") %>% 
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
nest_viewer("BCNH", "119")


#---
## check difference between 2 dates against certain species-specific period lengths
date_ranger <- function(zspp, date1, date2) {
  # create df with the minimum fledging age and duration of incubation for each species
  sp_stats1 = data.frame(species = c('BCNH', 'SNEG', 'CAEG', 'GREG', 'GBHE', 'DCCO'),
                       min.fl = as.numeric(c('40', '34', '38', '77', '84', NA)),
                       inc.dur = as.numeric(c('25', '22', '24', 	'28', '28', NA)),
                       min.stg4 = as.numeric(c('35', '32', '34', 	'49', '49', NA)))
  sp_stats2 <- filter(sp_stats1, species == zspp)
  foo <- as.Date(date2) - as.Date(date1)
  foo2 <- data.frame(date.span = foo,
                     diff.from.min.fl = foo - sp_stats2$min.fl,
                     diff.from.inc.dur = foo - sp_stats2$inc.dur,
                     diff.from.min.stg4 = foo - sp_stats2$min.stg4)
  return(foo2)
}
date_ranger("BCNH", "2019-4-13", "2019-5-17")
