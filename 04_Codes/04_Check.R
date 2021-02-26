# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  WYETH Ca 2020Q4
# Purpose:      Check
# programmer:   Zhe Liu
# Date:         2021-02-25
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Check SOP ----
## Corp
corp.comp <- wyeth.delivery %>% 
  distinct(Pack_ID, Corp_Desc_Ca = Corp_Desc) %>% 
  right_join(chpa.2020q4, by = 'Pack_ID') %>% 
  distinct(Corp_Desc, Corp_Desc_Ca) %>% 
  filter(!is.na(Corp_Desc_Ca))

## CHPA
ca.chpa <- chpa.2020q4 %>% 
  pivot_longer(cols = c(ends_with('UNIT'), ends_with('RENMINBI')), 
               names_to = 'quarter', 
               values_to = 'value') %>% 
  separate(quarter, c('quarter', 'measure'), sep = '_') %>% 
  pivot_wider(id_cols = c(Pack_ID, ATC3_Code, Molecule_Desc, Prd_desc, 
                          Pck_Desc, Corp_Desc, quarter), 
              names_from = measure, 
              values_from = value) %>% 
  filter(Pack_ID %in% wyeth.delivery$Pack_ID, 
         stri_sub(quarter, 1, 4) %in% c('2018', '2019', '2020')) %>% 
  left_join(corp.comp, by = 'Corp_Desc') %>% 
  mutate(MKT = 'CA', 
         Prd_desc = trimws(stri_sub(Prd_desc, 1, -4)), 
         Corp_Desc = if_else(is.na(Corp_Desc_Ca), Corp_Desc, Corp_Desc_Ca)) %>% 
  select(Pack_ID, Date = quarter, ATC3 = ATC3_Code, MKT, Molecule_Desc, 
         Prod_Desc_EN = Prd_desc, Pck_Desc, Corp_Desc, Units = UNIT, 
         Sales = RENMINBI)

write.xlsx(ca.chpa, '05_Internal_Review/Ca_CHPA_2018Q1_2020Q4.xlsx')
