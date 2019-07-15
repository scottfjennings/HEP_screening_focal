hep_edit <- edit(hep)

hep_test <- diff_data(hep, hep_edit)

hep_orig_edit <- rbind(hep, hep_edit) %>% 
  distinct() %>% 
  arrange(species, nest, date)






#########################################
# determine if stage on current check is less than stage on prev check
# also count how many previous checks the nest had the same status as current check  
 check_seq <- hep %>% 
  arrange(species, nest, date) %>%
  group_by(species, nest) %>% 
  mutate(prev.stage = lag(stage),
         drop.in.stage = ifelse(stage < prev.stage, T, F),
         ended = ifelse(stage == 0 & drop.in.stage == T, T, NA),
         status.seq = sequence(rle(as.character(status))$lengths)) %>% 
   select(date, species, nest, status, status.seq, prev.stage, drop.in.stage, ended, adults, chicks, notes)

 check_seq <- check_seq %>% 
   select(date, species, nest, status, status.seq, everything())
   
#ID dates that a nest went from stage > 0 to stage = 0
end_dates <- hep %>% 
  arrange(species, nest, date) %>%
  group_by(species, nest) %>% 
  mutate(prev.stage = lag(stage),
         drop.in.stage = ifelse(stage < prev.stage, T, F),
         ended = ifelse(stage == 0 & drop.in.stage == T, T, NA))  %>% 
  arrange(species, nest, date) %>% 
  filter(!is.na(ended)) %>% 
  select(species, nest, date)

max_end_dates <- end_dates %>% 
  group_by(species, nest) %>% 
  summarise(max.end.date = max(date))

num_end_dates <- end_dates %>% 
  group_by(species, nest) %>% 
  summarise(num_end_dates = n())

end_dates_wide <- end_dates %>% 
  arrange(species, nest, date) %>% 
  mutate(id = paste("end_date_", row_number(), sep = "")) %>% 
  spread(id, date) %>% 
  full_join(., num_end_dates)
 
# extract the minimum date a nest went from stage > 0 to stage = 0
min_end_dates <- end_dates  %>% 
  group_by(species, nest) %>% 
  summarise(min.end.date = min(date))  

attempts <- hep %>%
  select(species, nest, date, stage) %>% 
  full_join(., min_end_dates) %>% 
  mutate(attempt = ifelse(date > min.end.date & stage > 0, "B", "A")) %>% 
  arrange(species, nest, date)

  