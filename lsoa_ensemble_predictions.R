rm(list = ls())

pacman::p_load(caret, tidyverse)

# loading LA level datasets -------------------------------------------------

la_decency <- read_csv("non_decent_las.csv")

retired_la <- read_csv("retired_la.csv")

hm_la <- read_csv("higher_managerial_la.csv")

epc_la <- read_csv("epc_la.csv")

tenure_la <- read_csv("tenure_la.csv")

occ_la <- read_csv("occupancy_la.csv")

heat_la <- read_csv("central_heating_la.csv")

type_la <- read_csv("housing_type_la.csv")

birth_country_la <- read_csv("birth_country_la.csv")

age_lsoa <- read_csv("voapropertyage.csv")

lsoa_lookup <- read_csv("lsoa_to_lad_lookup.csv")

full <- la_decency |> 
  left_join(retired_la[,-1], by = c("ons_code" = "la_code")) |> 
  left_join(hm_la[,-1], by = c("ons_code" = "la_code")) |> 
  left_join(epc_la[,-1], by = c("ons_code" = "la_code")) |> 
  left_join(tenure_la[,-1], by = c("ons_code" = "la_code")) |> 
  left_join(occ_la[,-1], by = c("ons_code" = "la_code")) |> 
  left_join(heat_la[,-1], by = c("ons_code" = "la_code")) |> 
  left_join(type_la[,-1], by = c("ons_code" = "la_code")) |> 
  left_join(birth_country_la[,-1], by = c("ons_code" = "la_code"))

# calculating age for LAs from lsoa data ------------------------------------------------

age_lsoa <- age_lsoa |> 
  left_join(lsoa_lookup[,c("LSOA11CD","LAD22CD")], by = c("AREA_CODE" = "LSOA11CD"))

age_la <- age_lsoa |> 
  mutate(
    total_pre_1919 = (BP_PRE_1900 + BP_1900_1918),
    total_1919_1944 = (BP_1919_1929 + BP_1930_1939),
    total_1945_1991 = (BP_1945_1954 + BP_1955_1964 + BP_1965_1972 + BP_1973_1982 + BP_1983_1992)
  ) |> 
  group_by(LAD22CD) |> 
  summarise(
    total_prop = sum(ALL_PROPERTIES),
    total_pre_1919 = sum(total_pre_1919),
    total_1919_1944 = sum(total_1919_1944),
    total_1945_1991 = sum(total_1945_1991),
    .groups = "drop"
  ) |> 
  mutate(
    percent_pre_1919 = (total_pre_1919 / total_prop)*100,
    percent_1919_1944 = (total_1919_1944 / total_prop)*100,
    percent_1945_1991 = (total_1945_1991 / total_prop)*100
  )

full <- full |> 
  left_join(age_la, by = c("ons_code" = "LAD22CD")) |> 
  mutate(
    percent_beds_below = percent_one_below + percent_two_below,
    percent_fg = percent_f + percent_g,
    percent_fixed_room = percent_no_heating + percent_wood + percent_solid,
    percent_ab = percent_a + percent_b
  ) |> 
  select(ons_code:percent_students, 
         percent_e, percent_owned, percent_prs, percent_electric, 
         percent_detached:percent_asia, 
         percent_pre_1919:percent_ab) |> 
  filter(local_authority != "Isles of Scilly") |> 
  na.omit()

# check of linear fit ---------------------------------------------------

test_mod <- lm(percent_non_decent ~ ., data = full |> select(-ons_code, -local_authority,
                                                             -number_dwellings, -number_non_decent))

summary(test_mod)

# split into test train --------------------------------------------------

set.seed(123)
train_set <- sample_frac(full, 0.8) |> as.data.frame()
test_set <- full |> filter(!ons_code %in% train_set$ons_code) |> as.data.frame()

# Defining the training controls for multiple models -----------------------------

fitControl <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = 'final'
)

predictors <- colnames(model.matrix(test_mod))[-1]

outcome_name <- "percent_non_decent"

# training lower layer models -------------------------------------------------

#Training the random forest model
model_rf <- train(train_set[,predictors],
                  train_set[,outcome_name],
                  method='rf',
                  trControl=fitControl,
                  tuneLength=3)

#Training the lm model
model_lm <- train(train_set[,predictors],
                  train_set[,outcome_name],
                  method='lm',
                  trControl=fitControl,
                  tuneLength=3)

#Training the nn model
model_nn <- train(train_set[,predictors],
                  train_set[,outcome_name],
                  method='nnet',
                  trControl=fitControl,
                  linout = TRUE,
                  tuneLength=3)

#Training the gam model
model_gm <- train(train_set[,predictors],
                  train_set[,outcome_name],
                  method='gamSpline',
                  trControl=fitControl,
                  tuneLength=3)

test_set$pred_rf <- predict(object = model_rf, test_set[,predictors])
test_set$pred_lm <- predict(object = model_lm, test_set[,predictors])
test_set$pred_nn <- predict(object = model_nn, test_set[,predictors])
test_set$pred_gm <- predict(object = model_gm, test_set[,predictors])

#Predicting the out of fold prediction non-decent % for training data
train_set$OOF_pred_rf <- model_rf$pred$pred[order(model_rf$pred$rowIndex)]
train_set$OOF_pred_lm <- model_lm$pred$pred[order(model_lm$pred$rowIndex)]
train_set$OOF_pred_nn <- model_nn$pred$pred[order(model_nn$pred$rowIndex)]
train_set$OOF_pred_gm <- model_gm$pred$pred[order(model_gm$pred$rowIndex)]

#Predicting non-decent % for the test data
test_set$OOF_pred_rf <- predict(model_rf, test_set[predictors])
test_set$OOF_pred_lm <- predict(model_lm, test_set[predictors])
test_set$OOF_pred_nn <- predict(model_nn, test_set[predictors])
test_set$OOF_pred_gm <- predict(model_gm, test_set[predictors])

# ensemble model -----------------------------------------------------------

#Predictors for top layer models 
predictors_top <- c('OOF_pred_rf', 'OOF_pred_lm', 'OOF_pred_nn', 'OOF_pred_gm') 

#lm as top layer model 
model_elm <- train(train_set[,predictors_top],
                   train_set[,outcome_name],
                   method = 'lm',
                   trControl = fitControl,
                   tuneLength = 3)

#nnet as top layer model 
model_enn <- train(train_set[,predictors_top],
                   train_set[,outcome_name],
                   method = 'nnet',
                   trControl = fitControl,
                   linout = TRUE,
                   tuneLength = 3)

#gam as top layer model 
model_egm <- train(train_set[,predictors_top],
                   train_set[,outcome_name],
                   method = 'gamSpline',
                   trControl = fitControl,
                   tuneLength = 3)

#predict using lm top layer model
test_set$pred_elm <- predict(model_elm, test_set[,predictors_top])
test_set$pred_enn <- predict(model_enn, test_set[,predictors_top])
test_set$pred_egm <- predict(model_egm, test_set[,predictors_top])

test_set <- test_set %>% 
  mutate(
    pred_avg = (OOF_pred_rf + OOF_pred_lm + OOF_pred_nn + OOF_pred_gm)/4
  )

# RMSE on test data ----------------------------------------------------------

rmse <- function(x){
  sqrt(mean((test_set$percent_non_decent - test_set[,x])^2))
}

preds <- test_set |> select(starts_with("pred")) |> names()

rmse_vec <- rep(NA, length(preds))
names(rmse_vec) <- preds

for(i in seq_along(preds)){
  rmse_vec[i] <- rmse(preds[i])
}

rmse_vec
plot(rmse_vec, type = "o")
min(rmse_vec)

# best predictor is ensemble model with gm as stacked layer
# but to avoid overfitting, going to use lm as stacked layer

# Predicting outcome for the whole dataset using best model ---------------------------

full$OOF_pred_rf <- predict(model_rf, full[predictors])
full$OOF_pred_lm <- predict(model_lm, full[predictors])
full$OOF_pred_nn <- predict(model_nn, full[predictors])
full$OOF_pred_gm <- predict(model_gm, full[predictors])
full$pred_elm <- predict(model_elm, full[,predictors_top])
full$pred_egm <- predict(model_egm, full[,predictors_top])

# distribution of predictions and real values ---------------------------------

full %>% 
  ggplot() +
  geom_density(aes(x = pred_elm), fill = "lightgrey", alpha = 0.5) +
  geom_density(aes(x = percent_non_decent), fill = "lightblue", alpha = 0.5) +
  theme_bw()

# virtually identical
full %>% 
  ggplot() +
  geom_density(aes(x = pred_egm), fill = "pink", alpha = 0.5) +
  geom_density(aes(x = percent_non_decent), fill = "lightblue", alpha = 0.5) +
  theme_bw()

# visualising distribution of residuals
full %>% 
  mutate(res = percent_non_decent - pred_elm) %>% 
  ggplot(aes(x = res)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 0.25, colour = "black", fill = "lightgrey") +
  geom_density() +
  theme_bw()

# saving model ------------------------------------------------------

save(model_lm, file = "lm_for_ensemble.RData")
save(model_rf, file = "rf_for_ensemble.RData")
save(model_nn, file = "nn_for_ensemble.RData")
save(model_gm, file = "gm_for_ensemble.RData")
save(model_elm, file = "ensemble_model.RData")
