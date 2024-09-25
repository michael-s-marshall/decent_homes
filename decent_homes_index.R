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

# imputation ----------------------------------------------------------------

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

# index ---------------------------------------------------------------------------

index_df <- full %>% 
  select(lsoa, mnemonic, percent_retired, 
         percent_higher_managerial, percent_beds_below, 
         percent_at_beds, percent_la, percent_ha, percent_electric, 
         percent_fixed_room, percent_e, percent_fg,
         percent_pre_1919.b, percent_1919_1944.b, percent_1945_1991.b)

# AMEs
load("AMEs.RData")

ames <- ames_summ %>% 
  select(ame, var) %>% 
  pivot_wider(names_from = "var", values_from = "ame")

rm(ames_summ)

# multiplying by AMEs
index_df <- index_df %>% 
  mutate(
    ind_higher_managerial = percent_higher_managerial * ames$prob_higher_managerial,
    ind_beds_below = percent_beds_below * ames$prob_beds_below,
    ind_at_beds = percent_at_beds * ames$prob_at_beds,
    ind_la = percent_la * ames$prob_la,
    ind_ha = percent_ha * ames$prob_ha,
    ind_electric = percent_electric * ames$prob_electric,
    ind_fixed_room = percent_fixed_room * ames$prob_fixed_room,
    ind_e = percent_e * ames$prob_e,
    ind_fg = percent_fg * ames$prob_fg,
    ind_retired = percent_retired * ames$prob_retired,
    ind_pre_1919 = percent_pre_1919.b * ames$prob_pre_1919,
    ind_1919_1944 = percent_1919_1944.b * ames$prob_1919_1944,
    ind_1945_1991 = percent_1945_1991.b * ames$prob_1945_1991
  )

ind_vars <- index_df %>% select(contains("ind")) %>% names()
index_df <- index_df %>%
  mutate(dhs_ind = rescale01(rowSums(across(all_of(ind_vars)))))

# mapping -----------------------------------------------------------------------

# lsoa shapefile
lsoas <- read_sf("LSOA_2021_EW_BSC_V4.shp")

lsoas <- lsoas %>% 
  filter(LSOA21CD %in% full$mnemonic)

dhs_sf <- function(df, var){
  
  df %>% 
    ggplot(aes(fill = {{var}})) +
    geom_sf() +
    scale_fill_viridis_c(option = "turbo",
                         limits = c(0,1),
                         oob = scales::squish) +
    theme_void() +
    labs(fill = "Non-Decent\nIndex")
  
}

# sheffield
lsoas %>% 
  left_join(index_df, by = c("LSOA21CD" = "mnemonic")) %>% 
  filter(str_detect(LSOA21NM, "Sheffield")) %>% 
  dhs_sf(var = dhs_ind)

# London
london <- read_csv("london_lsoas.csv")

lsoas %>% 
  left_join(index_df, by = c("LSOA21CD" = "mnemonic")) %>% 
  filter(LSOA21CD %in% london$`LSOA code`) %>% 
  dhs_sf(var = dhs_ind)

# Manchester
gmca <- "Manchester|Salford|Bury|Bolton|Oldham|Rochdale|Stockport|Tameside|Trafford|Wigan"

lsoas %>% 
  left_join(index_df, by = c("LSOA21CD" = "mnemonic")) %>% 
  filter(str_detect(LSOA21NM, gmca)) %>% 
  dhs_sf(var = dhs_ind)

# distribution of index
index_df %>% 
  ggplot(aes(x = dhs_ind)) +
  geom_histogram(bins = 100, fill = "white", colour = "lightgrey")

# high DHS non-compliance local authorities -------------------------------------

map_las <- function(la_string){
  lsoas %>% 
    left_join(index_df, by = c("LSOA21CD" = "mnemonic")) %>% 
    filter(str_detect(LSOA21NM, la_string)) %>% 
    dhs_sf(var = dhs_ind) + 
    labs(title = la_string) +
    theme(plot.title = element_text(size = 9,
                                    hjust = 0.5))
}

p1 <- map_las("Derbyshire Dales")
p2 <- map_las("West Devon")
p3 <- map_las("Torridge")
p4 <- map_las("Mid Devon")
p5 <- map_las("Cornwall")

# low non-compliance local authorities --------------------------------------

p6 <- map_las("Bracknell Forest")
p7 <- map_las("Wokingham")
p8 <- map_las("Milton Keynes")
p9 <- map_las("Surrey Heath")
p10 <- map_las("Eastleigh")

pacman::p_load(patchwork)

p1 + p2 + p3 + p4 + p5 + plot_layout(ncol = 5,
                                     guides = "collect")

p6 + p7 + p8 + p9 + p10 + plot_layout(ncol = 5,
                                     guides = "collect")

# comparing to ONS LA estimates --------------------------------------------------------

la_lookup <- read_csv("lsoa_to_la_lookup.csv")
la_decency <- read_csv("non_decent_las.csv")

index_df <- index_df %>% 
  left_join(la_lookup %>% select(LSOA21CD, LAD23CD, LAD23NM),
            by = c("mnemonic" = "LSOA21CD"))

index_la <- index_df %>% 
  group_by(LAD23CD, LAD23NM) %>% 
  summarise(dhs_ind = mean(dhs_ind, na.rm = T),
            .groups = "drop")

full_las <- index_la %>% 
  left_join(la_decency, by = c("LAD23CD" = "ons_code"))

full_las %>% 
  ggplot(aes(x = dhs_ind, y = percent_non_decent)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  geom_text_repel(data = subset(full_las,
                                percent_non_decent > 30 |
                                  dhs_ind > 0.35),
                  mapping = aes(label = LAD23NM)) +
  theme_minimal() +
  labs(x = "Non-decent Index", y = "ONS % Non-decent")

cor.test(full_las$dhs_ind, full_las$percent_non_decent)
(summary(lm(percent_non_decent ~ dhs_ind, data = full_las)))

la_map_sf <- lsoas %>% 
  left_join(index_df, by = c("LSOA21CD" = "mnemonic")) %>% 
  left_join(full_las, by = c("LAD23CD","LAD23NM"),
            suffix = c("_lsoa","_lad"))

p11 <- la_map_sf %>% 
  ggplot(aes(fill = dhs_ind_lad)) +
  geom_sf(colour = NA) +
  scale_fill_viridis_c(option = "turbo") +
  theme_void() +
  labs(fill = "Non-Decent\nIndex")

p12 <- la_map_sf %>% 
  ggplot(aes(fill = percent_non_decent)) +
  geom_sf(colour = NA) +
  scale_fill_viridis_c(option = "turbo") +
  theme_void() +
  labs(fill = "ONS\nNon-decent\n%")

p11 + p12

# Northumberland --------------------------------------------------------------

map_las("Northumberland")

# interactive map ---------------------------------------

index <- lsoas %>% 
  left_join(index_df, by = c("LSOA21CD" = "mnemonic")) %>% 
  rename(`Non-decent` = dhs_ind)

index <- st_transform(index, 4326)

pal <- colorNumeric(palette = viridisLite::viridis(n = 7, option = "turbo"), domain = index$`Non-decent`)

leaflet(index) %>% 
  addTiles() %>% 
  addPolygons(weight = 0.25, 
              fillColor = ~pal(`Non-decent`),
              fillOpacity = .8) %>% 
  addLegend(pal = pal, values = ~`Non-decent`, opacity = .8)

# test -------------------------------------------------

cor.test(index_df$dhs_ind, index_df$percent_fg)
index_df %>%
  ggplot(aes(x = percent_fg, y = dhs_ind)) +
  geom_point(alpha = .2) +
  geom_smooth(method = "lm")

(summary(lm(dhs_ind ~ percent_fg + percent_e +
              percent_retired + percent_higher_managerial +
              percent_beds_below + percent_at_beds + percent_la +
              percent_ha + percent_electric + percent_fixed_room +
              percent_pre_1919.b + percent_1919_1944.b +
              percent_1945_1991.b,
            data = index_df)))

(plot_coefs(lm(dhs_ind ~ percent_fg + percent_e +
                 percent_retired + percent_higher_managerial +
                 percent_beds_below + percent_at_beds + percent_la +
                 percent_ha + percent_electric + percent_fixed_room +
                 percent_pre_1919.b + percent_1919_1944.b +
                 percent_1945_1991.b,
            data = index_df)))
