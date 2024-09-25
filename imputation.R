rm(list = ls())

# loading packages -----------------------------------------------------------

pacman::p_load(tidyverse, sf, ggrepel, leaflet, mapview)

# loading datasets ----------------------------------------------------------

retired <- read_csv("retired.csv") %>% 
  rename(lsoa = 1)

hm <- read_csv("higher_managerial.csv") %>% 
  rename(lsoa = 1)

epc <- read_csv("epc_by_lsoa.csv") %>% 
  rename(lsoa = 1)

tenure <- read_csv("tenure.csv") %>% 
  rename(lsoa = 1)

occ <- read_csv("occupancy.csv") %>% 
  rename(lsoa = 1)

heat <- read_csv("central_heating.csv") %>% 
  rename(lsoa = 1)

age <- read_csv("voapropertyage.csv") %>% 
  rename(mnemonic = 1)

age <- age %>% 
  mutate(
    percent_pre_1919 = (BP_PRE_1900 + BP_1900_1918) / ALL_PROPERTIES,
    percent_1919_1944 = (BP_1919_1929 + BP_1930_1939) / ALL_PROPERTIES,
    percent_1945_1991 = (BP_1945_1954 + BP_1955_1964 + BP_1965_1972 + BP_1973_1982 + BP_1983_1992) / ALL_PROPERTIES
  ) %>% 
  select(mnemonic, starts_with("percent"))

# merging
full <- retired %>% 
  left_join(hm, by = c("lsoa","mnemonic")) %>% 
  left_join(epc, by = c("lsoa","mnemonic")) %>% 
  left_join(tenure, by = c("lsoa","mnemonic")) %>% 
  left_join(occ, by = c("lsoa","mnemonic")) %>% 
  left_join(heat, by = c("lsoa","mnemonic")) %>% 
  left_join(age, by = "mnemonic") %>% 
  filter(!str_detect(mnemonic,"W")) %>% 
  filter(!str_detect(lsoa,"Scilly"))

full %>% map_int(~sum(is.na(.)))

full <-  full %>% 
  mutate(
    percent_beds_below = percent_one_minus_beds + percent_two_minus_beds,
    percent_fg = percent_f + percent_g,
    percent_fixed_room = percent_no_central_heating + percent_wood + percent_solid_fuel,
  ) 

full_short <- full %>% na.omit()

# imputation -----------------------------------------------------------------------------

pacman::p_load(randomForest)

# lm 1919 -------------------------------------------------------------------------

lm_p19 <- lm(percent_pre_1919 ~ percent_retired +
               percent_higher_managerial + percent_beds_below + percent_at_beds +
               percent_la + percent_ha + percent_electric + percent_fixed_room +
               percent_e + percent_fg + percent_d + percent_c + percent_b + percent_prs,
             data = full_short)
summary(lm_p19)
preds_lm_p19 <- predict(lm_p19)
lm_p19_mse <- mean((preds_lm_p19 - full_short$percent_pre_1919)^2)

# lm 1919-1944 ----------------------------------------------------------------------

lm_1944 <- lm(percent_1919_1944 ~ percent_retired +
                percent_higher_managerial + percent_beds_below + percent_at_beds +
                percent_la + percent_ha + percent_electric + percent_fixed_room +
                percent_e + percent_fg + percent_d + percent_c + percent_b + percent_prs,
              data = full_short)
summary(lm_1944)
preds_lm_1944 <- predict(lm_1944)
lm_1944_mse <- mean((preds_lm_1944 - full_short$percent_1919_1944)^2)

# lm 1945 - 1991 ----------------------------------------------------------------------

lm_1991 <- lm(percent_1945_1991 ~ percent_retired +
                percent_higher_managerial + percent_beds_below + percent_at_beds +
                percent_la + percent_ha + percent_electric + percent_fixed_room +
                percent_e + percent_fg + percent_d + percent_c + percent_b + percent_prs,
              data = full_short)
summary(lm_1991)
preds_lm_1991 <- predict(lm_1991)
lm_1991_mse <- mean((preds_lm_1991 - full_short$percent_1945_1991)^2)

# random forest pre-1919 --------------------------------------------------------------

set.seed(123)
rf_p19 <- randomForest(percent_pre_1919 ~ percent_retired +
                         percent_higher_managerial + percent_beds_below + percent_at_beds +
                         percent_la + percent_ha + percent_electric + percent_fixed_room +
                         percent_e + percent_fg + percent_d + percent_c + percent_b + percent_prs,
                       data = full_short, mtry = 5, ntree = 250)
preds_rf_p19 <- predict(rf_p19)
rf_p19_mse <- mean((preds_rf_p19 - full_short$percent_pre_1919)^2)
lm_p19_mse
rf_p19_mse

# random forest 1919-1944 --------------------------------------------------------------

set.seed(123)
rf_1944 <- randomForest(percent_1919_1944 ~ percent_retired +
                         percent_higher_managerial + percent_beds_below + percent_at_beds +
                         percent_la + percent_ha + percent_electric + percent_fixed_room +
                         percent_e + percent_fg + percent_d + percent_c + percent_b + percent_prs,
                       data = full_short, mtry = 5, ntree = 250)
preds_rf_1944 <- predict(rf_1944)
rf_1944_mse <- mean((preds_rf_1944 - full_short$percent_1919_1944)^2)
lm_1944_mse
rf_1944_mse

save

# random forest 1945-1991 --------------------------------------------------------------

set.seed(123)
rf_1991 <- randomForest(percent_1945_1991 ~ percent_retired +
                          percent_higher_managerial + percent_beds_below + percent_at_beds +
                          percent_la + percent_ha + percent_electric + percent_fixed_room +
                          percent_e + percent_fg + percent_d + percent_c + percent_b + percent_prs,
                        data = full_short, mtry = 5, ntree = 250)
preds_rf_1991 <- predict(rf_1991)
rf_1991_mse <- mean((preds_rf_1991 - full_short$percent_1945_1991)^2)
lm_1991_mse
rf_1991_mse

# saving ------------------------------------------------------------------

save(rf_p19, file="rf_p19.RData")
save(rf_1944, file="rf_1944.RData")
save(rf_1991, file="rf_1991.RData")
