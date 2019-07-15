hep_edit <- edit(hep)

hep_test <- diff_data(hep, hep_edit)

hep_orig_edit <- rbind(hep, hep_edit) %>% 
  distinct() %>% 
  arrange(species, nest, date)