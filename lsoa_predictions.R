rm(list = ls())

# loading packages -----------------------------------------------------------

pacman::p_load(tidyverse, sf, ggrepel, leaflet, mapview, jtools, lme4, randomForest, caret, gam, elasticnet, ggridges, patchwork)

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

# mapping shapefile -----------------------------------------------------------------------

# lsoa shapefile
lsoas <- read_sf("LSOA_2021_EW_BSC_V4.shp")

lsoas <- lsoas %>% 
  filter(LSOA21CD %in% full$mnemonic)

###############################################################################
# ensemble model comparison --------------------------------------------------
###############################################################################

# loading models
load("lm_for_ensemble.RData")
load("rf_for_ensemble.RData")
load("nn_for_ensemble.RData")
load("gm_for_ensemble.RData")
load("ls_for_ensemble.RData")
load("ensemble_model.RData")
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
en_df$OOF_pred_gm <- predict(model_gm, newdata = en_df)
en_df$OOF_pred_ls <- predict(model_ls, newdata = en_df)
en_df$non_decent_preds <- predict(model_elm, newdata = en_df)
# en_df$non_decent_preds[en_df$non_decent_preds < 0] <- 0

# comparing with LA estimates ---------------------------------------------

la_decency <- read_csv("non_decent_las.csv")

dwellings_n <- read_csv("all_households.csv")

dwellings_n <- dwellings_n |> select(-lsoa_name)

en_df <- en_df |> 
  left_join(dwellings_n, by = "mnemonic")

en_la <- en_df %>% 
  mutate(non_decent_preds_total = (non_decent_preds/100) * total_households) |> 
  group_by(LAD23CD, LAD23NM) |>  
  summarise(non_decent_preds_total = sum(non_decent_preds_total, na.rm = T),
            all_dwellings = sum(total_households, na.rm = T),
            .groups = "drop") |> 
  mutate(non_decent_preds = (non_decent_preds_total / all_dwellings)*100)

full_las <- en_la %>% 
  left_join(la_decency, by = c("LAD23CD" = "ons_code"))

full_las %>% 
  ggplot(aes(x = non_decent_preds, y = percent_non_decent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(x = "Non-decent Predictions", y = "ONS % Non-decent")

cor.test(full_las$non_decent_preds, full_las$percent_non_decent)
(summary(lm(percent_non_decent ~ non_decent_preds, data = full_las)))

la_map_sf <- lsoas %>% 
  left_join(en_df, by = c("LSOA21CD" = "mnemonic")) %>% 
  left_join(full_las, by = c("LAD23CD","LAD23NM"),
            suffix = c("_lsoa","_lad"))

scale_range <- range(c(la_map_sf$non_decent_preds_lad,la_map_sf$percent_non_decent))

P11 <- la_map_sf %>% 
  ggplot(aes(fill = non_decent_preds_lad)) +
  geom_sf(colour = NA) +
  scale_fill_viridis_c(limits = scale_range,
                       oob = scales::squish) +
  theme_void() +
  labs(title = "Predictions", fill = "Non-decent\n%")

P12 <- la_map_sf %>% 
  ggplot(aes(fill = percent_non_decent)) +
  geom_sf(colour = NA) +
  scale_fill_viridis_c(limits = scale_range,
                       oob = scales::squish) +
  theme_void() +
  labs(fill = "Non-decent\n%", title = "MHCLG figures")

P11 + P12 + plot_layout(guides = "collect")

full_las |> 
  ggplot() +
  geom_density(aes(x = percent_non_decent), fill = "lightblue", alpha = 0.5) +
  geom_density(aes(x = non_decent_preds), fill = "lightgrey", alpha = 0.5)

region <- read_csv("lsoa_to_region.csv") |> 
  select(LAD22CD, RGN22NM) |> 
  unique()

ridge_df <- full_las |> 
  left_join(region, by = c("LAD23CD" = "LAD22CD")) |>
  pivot_longer(c(non_decent_preds,percent_non_decent),
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
  mutate(multiplier = percent_non_decent/non_decent_preds,
         reconcilation_pct = non_decent_preds * multiplier)

full_las_mults <- full_las |> 
  select(LAD23CD, multiplier)

en_df <- en_df |> 
  left_join(full_las_mults, by = "LAD23CD") |> 
  mutate(non_decent_preds_m = non_decent_preds * multiplier)

# checking p=1
en_df %>% 
  mutate(non_decent_preds_total = (non_decent_preds_m/100) * total_households) |> 
  group_by(LAD23CD, LAD23NM) |>  
  summarise(non_decent_preds_total = sum(non_decent_preds_total, na.rm = T),
            all_dwellings = sum(total_households, na.rm = T),
            .groups = "drop") |> 
  mutate(non_decent_preds = (non_decent_preds_total / all_dwellings) * 100) |>  
  left_join(la_decency[,c("ons_code","percent_non_decent")], 
            by = c("LAD23CD" = "ons_code")) |>  
  ggplot(aes(x = non_decent_preds, y = percent_non_decent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(x = "Non-decent PRS Predictions", y = "MHCLG % PRS Non-decent")

# interactive map non-decent predictions ---------------------------------------

preds <- lsoas |>  
  left_join(en_df[,c("mnemonic","non_decent_preds_m")], 
            by = c("LSOA21CD" = "mnemonic")) |> 
  rename(`Non-decent` = non_decent_preds_m)

preds <- st_transform(preds, 4326)

pal <- colorNumeric(palette = viridisLite::viridis(n = 7, option = "turbo"), domain = preds$`Non-decent`)

leaflet(preds) %>% 
  addTiles() %>% 
  addPolygons(weight = 0.25, 
              fillColor = ~pal(`Non-decent`),
              fillOpacity = .8) %>% 
  addLegend(pal = pal, values = ~`Non-decent`, opacity = .8)

# visual validation -----------------------------------------------------

scale_range <- range(c(en_df$non_decent_preds,en_df$non_decent_preds_m))

# mapping sheffield
sheff1 <- lsoas |> 
  left_join(en_df[,c("mnemonic","non_decent_preds")],
            by = c("LSOA21CD" = "mnemonic")) |>  
  filter(str_detect(LSOA21NM, "Sheffield")) |>  
  ggplot(aes(fill = non_decent_preds)) +
  geom_sf() +
  scale_fill_viridis_c(option = "turbo",
                       limits = scale_range,
                       oob = scales::squish) +
  theme_void() +
  labs(fill = "Non-Decent\nPredictions")

sheff2 <- lsoas |> 
  left_join(en_df[,c("mnemonic","non_decent_preds_m")],
            by = c("LSOA21CD" = "mnemonic")) |>  
  filter(str_detect(LSOA21NM, "Sheffield")) |>  
  ggplot(aes(fill = non_decent_preds_m)) +
  geom_sf() +
  scale_fill_viridis_c(option = "turbo",
                       limits = scale_range,
                       oob = scales::squish) +
  theme_void() +
  labs(fill = "Non-Decent\nPredictions")

sheff1 + sheff2 + plot_layout(guides = "collect")

# mapping london
london <- read_csv("london_lsoas.csv")
london1 <- lsoas |>  
  left_join(en_df[,c("mnemonic","non_decent_preds")], 
            by = c("LSOA21CD" = "mnemonic")) |>  
  filter(LSOA21CD %in% london$`LSOA code`) |> 
  ggplot(aes(fill = non_decent_preds)) +
  geom_sf() +
  scale_fill_viridis_c(option = "turbo",
                       limits = scale_range,
                       oob = scales::squish) +
  theme_void() +
  labs(fill = "Non-Decent\nPredictions")

london2 <- lsoas |>  
  left_join(en_df[,c("mnemonic","non_decent_preds_m")], 
            by = c("LSOA21CD" = "mnemonic")) |>  
  filter(LSOA21CD %in% london$`LSOA code`) |> 
  ggplot(aes(fill = non_decent_preds_m)) +
  geom_sf() +
  scale_fill_viridis_c(option = "turbo",
                       limits = scale_range,
                       oob = scales::squish) +
  theme_void() +
  labs(fill = "Non-Decent\nPredictions")

london1 + london2 + plot_layout(guides = "collect")

# mapping manchester
gmca <- "Manchester|Salford|Bury|Bolton|Oldham|Rochdale|Stockport|Tameside|Trafford|Wigan"
manc1 <- lsoas |> 
  left_join(en_df[,c("mnemonic","non_decent_preds")], 
            by = c("LSOA21CD" = "mnemonic")) |>  
  filter(str_detect(LSOA21NM, gmca))  |> 
  ggplot(aes(fill = non_decent_preds)) +
  geom_sf() +
  scale_fill_viridis_c(option = "turbo",
                       limits = scale_range,
                       oob = scales::squish) +
  theme_void() +
  labs(fill = "Non-Decent\nPredictions")

manc2 <- lsoas |> 
  left_join(en_df[,c("mnemonic","non_decent_preds_m")], 
            by = c("LSOA21CD" = "mnemonic")) |>  
  filter(str_detect(LSOA21NM, gmca))  |> 
  ggplot(aes(fill = non_decent_preds_m)) +
  geom_sf() +
  scale_fill_viridis_c(option = "turbo",
                       limits = scale_range,
                       oob = scales::squish) +
  theme_void() +
  labs(fill = "Non-Decent\nPredictions")

manc1 + manc2 + plot_layout(guides = "collect")

# somerset west and taunton comparison
swt1 <- lsoas |>  
  left_join(en_df[,c("mnemonic","non_decent_preds")], 
            by = c("LSOA21CD" = "mnemonic")) |>
  filter(str_detect(LSOA21NM, "Somerset West and Taunton")) |> 
  ggplot(aes(fill = non_decent_preds)) +
  geom_sf() +
  scale_fill_viridis_c(option = "turbo",
                       limits = scale_range,
                       oob = scales::squish) +
  theme_void() +
  labs(fill = "Non-Decent\nPredictions")

swt2 <- lsoas |>  
  left_join(en_df[,c("mnemonic","non_decent_preds_m")], 
            by = c("LSOA21CD" = "mnemonic")) |>
  filter(str_detect(LSOA21NM, "Somerset West and Taunton")) |> 
  ggplot(aes(fill = non_decent_preds_m)) +
  geom_sf() +
  scale_fill_viridis_c(option = "turbo",
                       limits = scale_range,
                       oob = scales::squish) +
  theme_void() +
  labs(fill = "Non-Decent\nPredictions")

swt1 + swt2 + plot_layout(guides = "collect")

# comparison of northumberland
northumberland1 <- lsoas |> 
  left_join(en_df[,c("mnemonic","non_decent_preds")], 
            by = c("LSOA21CD" = "mnemonic")) |>
  filter(str_detect(LSOA21NM, "Northumberland"))  |>  
  ggplot(aes(fill = non_decent_preds)) +
  geom_sf() +
  scale_fill_viridis_c(option = "turbo",
                       limits = scale_range,
                       oob = scales::squish) +
  theme_void() +
  labs(fill = "Non-Decent\nPredictions")

northumberland2 <- lsoas |> 
  left_join(en_df[,c("mnemonic","non_decent_preds_m")], 
            by = c("LSOA21CD" = "mnemonic")) |>
  filter(str_detect(LSOA21NM, "Northumberland"))  |>  
  ggplot(aes(fill = non_decent_preds_m)) +
  geom_sf() +
  scale_fill_viridis_c(option = "turbo",
                       limits = scale_range,
                       oob = scales::squish) +
  theme_void() +
  labs(fill = "Non-Decent\nPredictions")

northumberland1 + northumberland2 + plot_layout(guides = "collect")

# non-decent predictions by high and low LAs ------------------------------

map_las <- function(la_string, preds){
  lsoas |> 
    left_join(en_df[,c("mnemonic","non_decent_preds","non_decent_preds_m")], 
              by = c("LSOA21CD" = "mnemonic")) |> 
    filter(str_detect(LSOA21NM, la_string)) |>  
    ggplot(aes(fill = {{preds}})) +
    geom_sf() +
    scale_fill_viridis_c(option = "turbo",
                         limits = scale_range,
                         oob = scales::squish) +
    theme_void() +
    labs(title = la_string, fill = "ONS\nNon-decent\n%") +
    theme(plot.title = element_text(size = 9,
                                    hjust = 0.5))
}

# high non-complaince LAs
P1 <- map_las("Derbyshire Dales", non_decent_preds)
P2 <- map_las("West Devon", non_decent_preds)
P3 <- map_las("Torridge", non_decent_preds)
P4 <- map_las("Mid Devon", non_decent_preds)
P5 <- map_las("Cornwall", non_decent_preds)

P1 + P2 + P3 + P4 + P5 + plot_layout(ncol = 5,
                                     guides = "collect")

P1m <- map_las("Derbyshire Dales", non_decent_preds_m)
P2m <- map_las("West Devon", non_decent_preds_m)
P3m <- map_las("Torridge", non_decent_preds_m)
P4m <- map_las("Mid Devon", non_decent_preds_m)
P5m <- map_las("Cornwall", non_decent_preds_m)

P1m + P2m + P3m + P4m + P5m + plot_layout(ncol = 5,
                                          guides = "collect")

# low non-compliance local authorities
P6 <- map_las("Bracknell Forest", non_decent_preds)
P7 <- map_las("Wokingham", non_decent_preds)
P8 <- map_las("Milton Keynes", non_decent_preds)
P9 <- map_las("Surrey Heath", non_decent_preds)
P10 <- map_las("Eastleigh", non_decent_preds)

P6 + P7 + P8 + P9 + P10 + plot_layout(ncol = 5,
                                     guides = "collect")

P6m <- map_las("Bracknell Forest", non_decent_preds_m)
P7m <- map_las("Wokingham", non_decent_preds_m)
P8m <- map_las("Milton Keynes", non_decent_preds_m)
P9m <- map_las("Surrey Heath", non_decent_preds_m)
P10m <- map_las("Eastleigh", non_decent_preds_m)

P6m + P7m + P8m + P9m + P10m + plot_layout(ncol = 5,
                                           guides = "collect")

# saving predictions --------------------------------------------------------

to_save <- en_df |> 
  select(lsoa, mnemonic, non_decent_preds_m) |> 
  rename(non_decent_preds = non_decent_preds_m)

write.csv(to_save, file = "non_decent_lsoa_predictions.csv")
