rm(list = ls())

# loading packages -----------------------------------------------------------

pacman::p_load(tidyverse, sf, ggrepel, leaflet, mapview, jtools, lme4, randomForest, caret, gam, patchwork, elasticnet, ggridges)

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

type <- read_csv("housing_type.csv") |> 
  rename(lsoa = 1)

birth_country <- read_csv("birth_country.csv") |> 
  rename(lsoa = 1)

age <- age %>% 
  mutate(
    percent_pre_1919 = (BP_PRE_1900 + BP_1900_1918) / ALL_PROPERTIES,
    percent_1919_1944 = (BP_1919_1929 + BP_1930_1939) / ALL_PROPERTIES,
    percent_1945_1991 = (BP_1945_1954 + BP_1955_1964 + BP_1965_1972 + BP_1973_1982 + BP_1983_1992) / ALL_PROPERTIES
  ) %>% 
  select(mnemonic, starts_with("percent"))

# merging
full <- retired  |>  
  left_join(hm, by = c("lsoa","mnemonic")) |>  
  left_join(epc, by = c("lsoa","mnemonic")) |>  
  left_join(tenure, by = c("lsoa","mnemonic"))  |>  
  left_join(occ, by = c("lsoa","mnemonic")) |>  
  left_join(heat, by = c("lsoa","mnemonic"))  |>  
  left_join(age, by = "mnemonic") |>  
  left_join(type, by = c("lsoa","mnemonic")) |> 
  left_join(birth_country, by = c("lsoa","mnemonic")) |> 
  filter(!str_detect(mnemonic,"W")) |>  
  filter(!str_detect(lsoa,"Scilly"))

full |>  map_int(~sum(is.na(.)))

full <-  full %>% 
  mutate(
    percent_beds_below = percent_one_minus_beds + percent_two_minus_beds,
    percent_fg = percent_f + percent_g,
    percent_fixed_room = percent_no_central_heating + percent_wood + percent_solid_fuel,
    percent_ab = percent_a + percent_b
  ) 

# imputation ----------------------------------------------------------------

la_lookup <- read_csv("lsoa_to_la_lookup.csv")

full <- full %>% 
  left_join(la_lookup %>% select(LSOA21CD, LAD23CD, LAD23NM),
            by = c("mnemonic" = "LSOA21CD"))

load("rf_p19.RData")
load("rf_1944.RData")
load("rf_1991.RData")

full$rf_p19_preds <- predict(rf_p19, newdata = full)
full$rf_1944_preds <- predict(rf_1944, newdata = full)
full$rf_1991_preds <- predict(rf_1991, newdata = full)

full <- full %>% 
  mutate(
    percent_pre_1919.b = ifelse(is.na(percent_pre_1919), rf_p19_preds, percent_pre_1919),
    percent_1919_1944.b = ifelse(is.na(percent_1919_1944), rf_1944_preds, percent_1919_1944),
    percent_1945_1991.b = ifelse(is.na(percent_1945_1991), rf_1991_preds, percent_1945_1991)
  )

full %>% map_int(~sum(is.na(.)))

# scaling function-----------------------------------------------------------

rescale01 <- function(x){
  out <- (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
  return(out)
}

# lsoa shapefile -----------------------------------------------------------------------

lsoas <- read_sf("LSOA_2021_EW_BSC_V4.shp")

lsoas <- lsoas %>% 
  filter(LSOA21CD %in% full$mnemonic)


###############################################################################
# ensemble model comparison --------------------------------------------------
###############################################################################

# loading models
load("lm_for_prs.RData")
load("rf_for_prs.RData")
load("nn_for_prs.RData")
load("ls_for_prs.RData")
load("gm_for_prs.RData")
load("ensemble_model_prs.RData")
predictors <- names(model_lm$trainingData)[names(model_lm$trainingData) != ".outcome"]

region <- read_csv("lsoa_to_region.csv")

region <- region %>% 
  rename(mnemonic = LSOA21CD) %>% 
  select(mnemonic, RGN22CD)

en_df <- full |> 
  left_join(region, by = "mnemonic") |> 
  mutate(
    E12000002 = ifelse(RGN22CD == "E12000002", 1, 0),
    E12000003 = ifelse(RGN22CD == "E12000003", 1, 0),
    E12000004 = ifelse(RGN22CD == "E12000004", 1, 0),
    E12000005 = ifelse(RGN22CD == "E12000005", 1, 0),
    E12000006 = ifelse(RGN22CD == "E12000006", 1, 0),
    E12000007 = ifelse(RGN22CD == "E12000007", 1, 0),
    E12000008 = ifelse(RGN22CD == "E12000008", 1, 0)
  ) |> 
  select(lsoa, LAD23CD, LAD23NM, mnemonic,
         all_of(predictors), starts_with("^E12"), 
         percent_pre_1919.b, percent_1919_1944.b) |> 
  select(-percent_pre_1919, -percent_1919_1944) |> 
  rename(percent_pre_1919 = percent_pre_1919.b,
         percent_1919_1944 = percent_1919_1944.b)

x_trans <- preProcess(en_df[,predictors])
en_df[,predictors] <- predict(x_trans, en_df[,predictors])

en_df$OOF_pred_rf <- predict(model_rf, newdata = en_df)
en_df$OOF_pred_lm <- predict(model_lm, newdata = en_df)
en_df$OOF_pred_nn <- predict(model_nn, newdata = en_df)
en_df$OOF_pred_ls <- predict(model_ls, newdata = en_df)
en_df$OOF_pred_gm <- predict(model_gm, newdata = en_df)
en_df$log_pct <- predict(model_elm, newdata = en_df)
en_df$non_decent_preds <- exp(en_df$log_pct)
range(en_df$non_decent_preds)
# en_df$non_decent_preds[en_df$non_decent_preds < 0] <- 0

# comparing with LA estimates ---------------------------------------------

dwellings_n <- tenure |> select(mnemonic, total_prs)

en_df <- en_df |> 
  left_join(dwellings_n, by = "mnemonic")

# predictions by LA
en_la <- en_df %>% 
  mutate(non_decent_preds_total = (non_decent_preds/100) * total_prs) |> 
  group_by(LAD23CD, LAD23NM) |>  
  summarise(non_decent_preds_total = sum(non_decent_preds_total, na.rm = T),
            all_dwellings = sum(total_prs, na.rm = T),
            .groups = "drop") |> 
  mutate(non_decent_preds = (non_decent_preds_total / all_dwellings) * 100)

# MHCLG estimates by LA
la_decency <- read_csv("non_decent_las_by_tenure.csv")

names(la_decency) <- c("la_code","la_name", "owner_nd_total",
                       "prs_nd_total", "sr_nd_total", "rented_nd_total",
                       "owner_nd_pct","prs_nd_pct","sr_nd_pct",
                       "rented_nd_pct")

full_las <- en_la %>% 
  left_join(la_decency[,c("la_code","prs_nd_pct","prs_nd_total")], 
            by = c("LAD23CD" = "la_code"))

full_las %>% 
  ggplot(aes(x = non_decent_preds, y = prs_nd_pct)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(x = "Non-decent PRS Predictions", y = "MHCLG % PRS Non-decent")

cor.test(full_las$non_decent_preds, full_las$prs_nd_pct)
(summary(lm(prs_nd_pct ~ non_decent_preds, data = full_las)))

la_map_sf <- lsoas %>% 
  left_join(en_df, by = c("LSOA21CD" = "mnemonic")) %>% 
  left_join(full_las, by = c("LAD23CD","LAD23NM"),
            suffix = c("_lsoa","_lad"))

scale_range <- range(c(la_map_sf$non_decent_preds_lad,la_map_sf$prs_nd_pct))

P11 <- la_map_sf %>% 
  ggplot(aes(fill = non_decent_preds_lad)) +
  geom_sf(colour = NA) +
  scale_fill_viridis_c(limits = scale_range,
                       oob = scales::squish) +
  theme_void() +
  labs(fill = "Non-Decent\nPredictions")

P12 <- la_map_sf %>% 
  ggplot(aes(fill = prs_nd_pct)) +
  geom_sf(colour = NA) +
  scale_fill_viridis_c(limits = scale_range,
                       oob = scales::squish) +
  theme_void() +
  labs(fill = "MHLCG\nNon-decent\n%")

P11 + P12

full_las |> 
  ggplot() +
  geom_density(aes(x = prs_nd_pct), fill = "lightblue", alpha = 0.5) +
  geom_density(aes(x = non_decent_preds), fill = "lightgrey", alpha = 0.5)

region <- read_csv("lsoa_to_region.csv") |> 
  select(LAD22CD, RGN22NM) |> 
  unique()

ridge_df <- full_las |> 
  left_join(region, by = c("LAD23CD" = "LAD22CD")) |>
  pivot_longer(non_decent_preds:prs_nd_pct,
               names_to = "measure",
               values_to = "value")
ridge_df$RGN22NM[ridge_df$LAD23NM == "Cumberland"] <- "North West"
ridge_df$RGN22NM[ridge_df$LAD23NM == "Westmorland and Furness"] <- "North West"
ridge_df$RGN22NM[ridge_df$LAD23NM == "North Yorkshire"] <- "Yorkshire and The Humber"
ridge_df$RGN22NM[ridge_df$LAD23NM == "Somerset"] <- "South West"

ridge_df |> 
  ggplot(aes(x = value, y = fct_reorder(RGN22NM, value), fill = measure)) +
  geom_density_ridges(alpha = 0.3, scale = 1) +
  scale_fill_viridis_d() +
  theme_minimal() +
  labs(y = NULL)

# multiplying factors ------------------------------------------

full_las <- full_las |> 
  mutate(multiplier = prs_nd_pct/non_decent_preds,
         reconcilation_pct = non_decent_preds * multiplier)

full_las_mults <- full_las |> 
  select(LAD23CD, multiplier)

en_df <- en_df |> 
  left_join(full_las_mults, by = "LAD23CD") |> 
  mutate(non_decent_preds_m = non_decent_preds * multiplier)

en_la2 <- en_df %>% 
  mutate(non_decent_preds_total = (non_decent_preds_m/100) * total_prs) |> 
  group_by(LAD23CD, LAD23NM) |>  
  summarise(non_decent_preds_total = sum(non_decent_preds_total, na.rm = T),
            all_dwellings = sum(total_prs, na.rm = T),
            .groups = "drop") |> 
  mutate(non_decent_preds = (non_decent_preds_total / all_dwellings) * 100)

full_las2 <- en_la2 %>% 
  left_join(la_decency[,c("la_code","prs_nd_pct")], 
            by = c("LAD23CD" = "la_code"))

full_las2 %>% 
  ggplot(aes(x = non_decent_preds, y = prs_nd_pct)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(x = "Non-decent PRS Predictions", y = "MHCLG % PRS Non-decent")

cor.test(full_las2$non_decent_preds, full_las$prs_nd_pct)

ridge_df2 <- full_las2 |> 
  left_join(region, by = c("LAD23CD" = "LAD22CD")) |>
  pivot_longer(non_decent_preds:prs_nd_pct,
               names_to = "measure",
               values_to = "value")
ridge_df2$RGN22NM[ridge_df2$LAD23NM == "Cumberland"] <- "North West"
ridge_df2$RGN22NM[ridge_df2$LAD23NM == "Westmorland and Furness"] <- "North West"
ridge_df2$RGN22NM[ridge_df2$LAD23NM == "North Yorkshire"] <- "Yorkshire and The Humber"
ridge_df2$RGN22NM[ridge_df2$LAD23NM == "Somerset"] <- "South West"

ridge_df2 |> 
  ggplot(aes(x = value, y = fct_reorder(RGN22NM, value), fill = measure)) +
  geom_density_ridges(alpha = 0.3, scale = 1) +
  scale_fill_viridis_d() +
  theme_minimal() +
  labs(y = NULL)

# visual validation -----------------------------------------------------

lsoa_range <- range(en_df$non_decent_preds_m)

# mapping sheffield
lsoas |> 
  left_join(en_df[,c("mnemonic","non_decent_preds_m")],
            by = c("LSOA21CD" = "mnemonic")) |>  
  filter(str_detect(LSOA21NM, "Sheffield")) |>  
  ggplot(aes(fill = non_decent_preds_m)) +
  geom_sf() +
  scale_fill_viridis_c(option = "turbo",
                       limits = lsoa_range,
                       oob = scales::squish) +
  theme_void() +
  labs(fill = "Non-Decent\nPredictions")

# mapping london
london <- read_csv("london_lsoas.csv")
lsoas |>  
  left_join(en_df[,c("mnemonic","non_decent_preds_m")], 
            by = c("LSOA21CD" = "mnemonic")) |>  
  filter(LSOA21CD %in% london$`LSOA code`) |> 
  ggplot(aes(fill = non_decent_preds_m)) +
  geom_sf() +
  scale_fill_viridis_c(option = "turbo",
                       limits = lsoa_range,
                       oob = scales::squish) +
  theme_void() +
  labs(fill = "Non-Decent\nPredictions")

# mapping manchester
gmca <- "Manchester|Salford|Bury|Bolton|Oldham|Rochdale|Stockport|Tameside|Trafford|Wigan"
lsoas |> 
  left_join(en_df[,c("mnemonic","non_decent_preds_m")], 
            by = c("LSOA21CD" = "mnemonic")) |>  
  filter(str_detect(LSOA21NM, gmca))  |> 
  ggplot(aes(fill = non_decent_preds_m)) +
  geom_sf() +
  scale_fill_viridis_c(option = "turbo",
                       limits = lsoa_range,
                       oob = scales::squish) +
  theme_void() +
  labs(fill = "Non-Decent\nPredictions")

# somerset west and taunton comparison
lsoas |>  
  left_join(en_df[,c("mnemonic","non_decent_preds_m")], 
            by = c("LSOA21CD" = "mnemonic")) |>
  filter(str_detect(LSOA21NM, "Somerset West and Taunton")) |> 
  ggplot(aes(fill = non_decent_preds_m)) +
  geom_sf() +
  scale_fill_viridis_c(option = "turbo",
                       limits = lsoa_range,
                       oob = scales::squish) +
  theme_void() +
  labs(fill = "Non-Decent\nPredictions")

# comparison of northumberland
lsoas |> 
  left_join(en_df[,c("mnemonic","non_decent_preds_m")], 
            by = c("LSOA21CD" = "mnemonic")) |>
  filter(str_detect(LSOA21NM, "Northumberland"))  |>  
  ggplot(aes(fill = non_decent_preds_m)) +
  geom_sf() +
  scale_fill_viridis_c(option = "turbo",
                       limits = lsoa_range,
                       oob = scales::squish) +
  theme_void() +
  labs(fill = "Non-Decent\nPredictions")

# non-decent predictions by high and low LAs ------------------------------

map_las <- function(la_string){
  lsoas |> 
    left_join(en_df[,c("mnemonic","non_decent_preds_m")], 
              by = c("LSOA21CD" = "mnemonic")) |> 
    filter(str_detect(LSOA21NM, la_string)) |>  
    ggplot(aes(fill = non_decent_preds_m)) +
    geom_sf() +
    scale_fill_viridis_c(option = "turbo",
                         limits = lsoa_range,
                         oob = scales::squish) +
    theme_void() +
    labs(title = la_string, fill = "ONS\nNon-decent\n%") +
    theme(plot.title = element_text(size = 9,
                                    hjust = 0.5))
}

P1 <- map_las("Derbyshire Dales")
P2 <- map_las("West Devon")
P3 <- map_las("Torridge")
P4 <- map_las("Mid Devon")
P5 <- map_las("Cornwall")

# low non-compliance local authorities

P6 <- map_las("Bracknell Forest")
P7 <- map_las("Wokingham")
P8 <- map_las("Milton Keynes")
P9 <- map_las("Surrey Heath")
P10 <- map_las("Eastleigh")

P1 + P2 + P3 + P4 + P5 + plot_layout(ncol = 5,
                                     guides = "collect")

P6 + P7 + P8 + P9 + P10 + plot_layout(ncol = 5,
                                      guides = "collect")

 
# saving predictions --------------------------------------------------------

to_save <- en_df |> 
  rename(non_decent_prs_preds = non_decent_preds_m) |> 
  select(lsoa, mnemonic, non_decent_prs_preds)

write.csv(to_save, file = "non_decent_prs_lsoa_predictions.csv")
