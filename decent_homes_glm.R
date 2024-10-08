# packages ---------------------------------------------

rm(list = ls())

pacman::p_load(tidyverse, haven, lme4, lmerTest, jtools, margins, ggstance)

# reading data and merging -----------------------------

general <- read_dta("general_18plus19_eul.dta")

physical <- read_dta("physical_18plus19_eul.dta")

interview <- read_dta("interview_18plus19_eul.dta")

# merging 
full <- interview %>% 
  left_join(general, by = "serialanon", suffix = c("int","gen")) %>% 
  left_join(physical, by = "serialanon", suffix = c("int","phy"))

# tables of non-decency ---------------------------------------------

non_decent_table <- function(df, var){
  df %>% 
    group_by({{var}}, dhomesy) %>% 
    summarise(n = n(), .groups = "drop_last") %>% 
    mutate(prop = n/sum(n)) %>% 
    ungroup()
}

my_vars <- c("tenure4x","ethhrp2x","ethprt2x","sexhrp","bedstdx",
             "nbedsx", "hhltsick", "hhinc5x", "housbenx", "hhbensx", 
             "hhtype6", "ndepchild", "hhsizex", "noUnits1", "agehrp2x",
             "agehrp4x", "emphrpx", "emphrp3x", "nssech9",
             "housex", "floor5x", "heat4x", "EPceeb12e","dwage7x")

table_dat <- full %>% select(all_of(my_vars), dhomesy)

dhs_tables <- table_dat %>% 
  select(all_of(my_vars)) %>% 
  names() %>% 
  map(~non_decent_table(table_dat,.data[[.x]]))

dhs_tables

rm(table_dat)

dhs_tables[[24]] %>% 
  ggplot(aes(x = as_factor(dwage7x), y = prop)) +
  geom_col(aes(fill = as_factor(dhomesy)), position = "dodge")

# expected relationships
 # tenure4x, ethhrp2x, bedstdx, hhltsick, hhinc5x (top quintile), hhbensx, 
 # hhtype6 (one person under 60), ndepchild (0, 4 or more),
 # hhsizex, emphrpx, housex, floor5x (less than 50sqm), heat4x,
 # EPceeb12e (F/G), dwage7x

# new factors -----------------------------------------

to_fact <- c("hhtype6", "hhbensx", "ethhrp2x", "sexhrp",
             "tenure4x", "gorehs", "dhomesy", "dwage7x",
             "hhltsick", "housex", "heat4x", "emphrpx")

full <- full %>% 
  mutate(
    ndepchild = fct_collapse(as_factor(ndepchild),
                             "0" = "0",
                             "1-3" = c("1","2","3"),
                             "4+" = c("4","5 or more")),
    hhinc_topquintile = as.factor(ifelse(hhinc5x == 5, "yes", "no")),
    bedstd2x = fct_collapse(as_factor(bedstdx),
                            "below" = c("two or more below standard",
                                        "one below standard"),
                            "at" = c("at standard"),
                            "above" = c("one above standard",
                                        "two or more above standard")),
    floor_less_50 = as.factor(
      ifelse(floor5x == 1, "yes", "no")
      ),
    EPC = fct_collapse(as_factor(EPceeb12e),
                                 "A/B" = "A/B",
                                 "C" = "C",
                                 "D" = "D",
                                 "E" = "E",
                                 "F/G" = c("F","G"))
  )

full[to_fact] <- full[to_fact] %>% map_df(as_factor)

full[to_fact] <- full[to_fact] %>% map_df(fct_drop)

full <- full %>% mutate(bedstd = fct_drop(fct_rev(bedstd2x)),
                        hhtype6 = fct_drop(hhtype6),
                        EPC = fct_drop(EPC),
                        dwage7x = fct_rev(dwage7x))

full %>% 
  select(all_of(to_fact), bedstd, hhtype6, EPC) %>% 
  names() %>% 
  map(~count(full %>% select(all_of(to_fact), bedstd, hhtype6, EPC), .data[[.x]]))

full %>% 
  select(all_of(to_fact), bedstd, hhtype6, EPC) %>% 
  map(levels)

full$high_managerial <- as.factor(
  ifelse(full$nssech9 == 1 | full$nssecp9 == 1, "yes", "no")
)

full$pre_1919 <- as.factor(
  ifelse(full$dwage7x == "pre 1919", "yes", "no")
)

full$nineteen_44 <- as.factor(
  ifelse(full$dwage7x == "1919 to 1944", "yes", "no")
)

full$fortyfive_91 <- as.factor(
  ifelse(full$dwage7x == "1945 to 1964"|full$dwage7x == "1965 to 1980"|full$dwage7x == "1981 to 1990", "yes", "no")
)

# modelling -------------------------------------------------

contrasts(full$dhomesy)

# test of null model
null_lm <- glm(dhomesy ~ 1, data = full, family = "binomial")
null_lmer <- glmer(dhomesy ~ (1|gorehs), data = full,
                   family = binomial("logit"))

AIC(null_lm)
AIC(null_lmer) # not much evidence of multilevel model

dhs_glm <- glm(dhomesy ~ emphrpx + hhtype6 + ndepchild +
                 hhbensx + high_managerial + bedstd +
                 ethhrp2x + sexhrp + tenure4x + hhltsick +
                 housex + heat4x + EPC + pre_1919 + nineteen_44 + fortyfive_91,
               data = full, family = "binomial")
summary(dhs_glm)
summ(dhs_glm)

full %>% 
  mutate(
    dwage7x = fct_collapse(
      dwage7x,
      "pre-1919" = "pre 1919",
      "1919-1944" = "1919 to 1944",
      "1945-1990" = c("1945 to 1964","1965 to 1980","1981 to 1990"),
      "post-1990" = c("1991 to 2002","post 2002"))
    ) %>% 
  group_by(tenure4x, dwage7x) %>%
  summarise(n = n(), .groups = "drop_last") %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = prop, y = tenure4x, fill = as_factor(dwage7x))) +
  geom_col(position = "stack") +
  scale_fill_viridis_d(option = "turbo") +
  theme_minimal() +
  labs(x = "Proportion", y = NULL,
       fill = "Dwelling\nage")

full %>% 
  group_by(tenure4x, EPC) %>%
  summarise(n = n(), .groups = "drop_last") %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = prop, y = tenure4x, fill = EPC)) +
  geom_col(position = "stack") +
  scale_fill_viridis_d(option = "turbo") +
  labs(x = "Proportion", y = NULL,
       fill = "SAP\nBand") +
  theme_minimal()

# parsimonious model -------------------------------------------

full$retired <- as.factor(ifelse(full$emphrpx == "retired",
                                 "yes", "no"))

full$single_hh_under_65 <- as.factor(
  ifelse(full$hhsizex == 1 & full$agehrp4x != 4, "yes", "no")
)

full$ha <- as.factor(
  ifelse(full$tenure4x == "housing association", "yes", "no")
)

full$la <- as.factor(
  ifelse(full$tenure4x == "local authority", "yes", "no")
)

full$prs <- as.factor(
  ifelse(full$tenure4x == "private rented", "yes", "no")
)

full$owner_occ <- as.factor(
  ifelse(full$tenure4x == "owner occupied", "yes", "no")
)

full$sr <- as.factor(
  ifelse(full$tenure4x == "housing association"|full$tenure4x == "local authority", "yes", "no")
)


full$epc_e <- as.factor(
  ifelse(full$EPC == "E", "yes", "no")
)

full$epc_fg <- as.factor(
  ifelse(full$EPC == "F/G", "yes", "no")
)

full$bedstd_below <- as.factor(
  ifelse(full$bedstd == "below", "yes", "no")
)

dhs_pars <- glm(dhomesy ~ high_managerial + retired +
                  bedstd_below + owner_occ + prs + heat4x +  epc_e + epc_fg + 
                  pre_1919 + nineteen_44 + fortyfive_91,
                data = full, family = "binomial")

summary(dhs_pars)
summ(dhs_pars)

# AMEs --------------------------------------------------------------

ames <- margins(dhs_pars, type = "response")

summary(ames)

plot_vars <- tibble(
  factor = as_tibble(summary(ames))$factor,
  var_name = c("Bed Standard: Below",
               "EPC: E",
               "EPC: F/G",
               "Dwelling age: 1945-1990",
               "Heating: Fixed room",
               "Heating: Storage",
               "SEC: Higher-managerial",
               "Dwelling age: 1919-1944",
               "Tenure: Owner-occupier",
               "Dwelling age: pre-1919",
               "Tenure: PRS",
               "Work: Retired")
)

plot_vars <- plot_vars %>% 
  mutate(var_name = fct_relevel(
    as.factor(var_name),
    c("Bed Standard: Below",
    "Dwelling age: pre-1919",
    "Dwelling age: 1919-1944",
    "Dwelling age: 1945-1990",
    "EPC: E",
    "EPC: F/G",
    "Heating: Fixed room",
    "Heating: Storage",
    "SEC: Higher-managerial",
    "Tenure: Owner-occupier",
    "Tenure: PRS",
    "Work: Retired"))
  )

summary(ames) %>% 
  as_tibble() %>% 
  left_join(plot_vars, by = "factor") %>% 
  ggplot(aes(x = AME, y = fct_rev(var_name))) +
  geom_vline(xintercept = 0, linetype = "dashed", 
             linewidth = 1.2, colour = "lightgrey") +
  geom_linerangeh(aes(xmin = lower, xmax = upper),
                  size = 1.2) +
  geom_point(shape = 21, fill = "white", size = 3) +
  theme_minimal() +
  drop_y_gridlines() +
  labs(y = NULL)

# saving model ---------------------------------------------------

save(dhs_pars, file = "dhs_logit_model.RData")

# saving table -------------------------------------------------

summary(ames) %>% 
  as_tibble() %>% 
  left_join(plot_vars, by = "factor") %>% 
  arrange(var_name) %>% 
  write.csv("ames.csv")

# extracting AME probs ------------------------------------------

ames_summ <- summary(ames) %>% 
  as_tibble()

ames_summ <- tibble(
  ame = ames_summ$AME,
  var_name = ames_summ$factor,
  var = c("prob_beds_below",
          "prob_e","prob_fg","prob_1945_1991", 
          "prob_fixed_room", "prob_electric", 
          "prob_higher_managerial",
          "prob_1919_1944","prob_owner_occ", 
          "prob_pre_1919","prob_prs","prob_retired")
  )

save(ames_summ, file = "AMEs.RData")
