# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  WYETH Ca 2020Q4
# Purpose:      Result
# programmer:   Zhe Liu
# Date:         2021-02-26
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- History ----
wyeth.history <- read.xlsx('06_Deliveries/WYETH_Ca_CHC_2018Q1_2020Q3_Check_20201125.xlsx', 
                           sheet = 'Origin')


##---- Wyeth Ca CHC 2020Q4 ----
wyeth.2020q4 <- wyeth.history %>% 
  filter(Date == '2020Q3') %>% 
  left_join(growth.wyeth, by = c('City' = 'city', 'Pack_ID' = 'packid')) %>% 
  mutate(growth_value = if_else(is.na(growth_value), 1, growth_value), 
         growth_volume = if_else(is.na(growth_volume), 1, growth_volume), 
         growth_dosage = if_else(is.na(growth_dosage), 1, growth_dosage)) %>% 
  mutate(Date = '2020Q4', 
         Sales = Sales * growth_value, 
         Units = Units * growth_volume, 
         DosageUnits = DosageUnits * growth_dosage) %>% 
  select(-starts_with('growth'))


##---- Result ----
wyeth.delivery <- bind_rows(wyeth.history, wyeth.2020q4)

write.xlsx(wyeth.delivery, '03_Outputs/Wyeth_Ca_CHC_2018Q1_2020Q4.xlsx')
