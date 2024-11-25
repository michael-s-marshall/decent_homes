rm(list = ls())

pacman::p_load(caret, tidyverse, elasticnet)

# loading LA level datasets -------------------------------------------------

la_decency <- read_csv("non_decent_las_by_tenure.csv")

names(la_decency) <- c("la_code","la_name", "owner_nd_total",
                       "prs_nd_total", "sr_nd_total", "rented_nd_total",
                       "owner_nd_pct","prs_nd_pct","sr_nd_pct",
                       "rented_nd_pct")

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

regions <- read_csv("lsoa_to_region.csv")

regions <- regions |> 
  select(LAD22CD, RGN22CD) |> 
  unique()

full <- la_decency |> 
  left_join(retired_la[,-1], by = "la_code") |> 
  left_join(hm_la[,-1], by = "la_code") |> 
  left_join(epc_la[,-1], by = "la_code") |> 
  left_join(tenure_la[,-1], by = "la_code") |> 
  left_join(occ_la[,-1], by = "la_code") |> 
  left_join(heat_la[,-1], by = "la_code") |> 
  left_join(type_la[,-1], by = "la_code") |> 
  left_join(birth_country_la[,-1], by = "la_code") |> 
  left_join(regions, by = c("la_code" = "LAD22CD"))

full$RGN22CD[full$la_name == "Cumberland"] <- "E12000002"
full$RGN22CD[full$la_name == "North Yorkshire"] <- "E12000003"
full$RGN22CD[full$la_name == "Somerset"] <- "E12000009"
full$RGN22CD[full$la_name == "Westmorland and Furness"] <- "E12000002"

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
  left_join(age_la, by = c("la_code" = "LAD22CD")) |> 
  mutate(
    #log_pct = log(prs_nd_pct),
    percent_beds_below = percent_one_below + percent_two_below,
    percent_fg = percent_f + percent_g,
    percent_fixed_room = percent_no_heating + percent_wood + percent_solid,
    percent_ab = percent_a + percent_b,
    E12000002 = ifelse(RGN22CD == "E12000002", 1, 0),
    E12000003 = ifelse(RGN22CD == "E12000003", 1, 0),
    E12000004 = ifelse(RGN22CD == "E12000004", 1, 0),
    E12000005 = ifelse(RGN22CD == "E12000005", 1, 0),
    E12000006 = ifelse(RGN22CD == "E12000006", 1, 0),
    E12000007 = ifelse(RGN22CD == "E12000007", 1, 0),
    E12000008 = ifelse(RGN22CD == "E12000008", 1, 0),
  ) |> 
  select(la_code:percent_students, 
         percent_e, percent_owned, percent_prs, percent_electric, 
         percent_detached:percent_asia, 
         percent_pre_1919:E12000008) |> 
  filter(la_name != "Isles of Scilly") |> 
  na.omit()

names(full)

# check of linear fit ---------------------------------------------------

mod_vars <- full |> 
  select(-la_code, -la_name, -owner_nd_total, -prs_nd_total, -sr_nd_total, -rented_nd_total, -rented_nd_pct, -owner_nd_pct, -sr_nd_pct) |> 
  names()

test_mod <- lm(prs_nd_pct ~ ., data = full[,mod_vars])

summary(test_mod)

predictors <- colnames(model.matrix(test_mod))[-1]

pred_cor <- cor(full[,predictors])
high_cor <- findCorrelation(pred_cor, 0.9)
high_cor

predictors <- c("percent_retired","percent_higher_managerial","percent_semi_routine",
                "percent_long_unemployed",
                "percent_owned","percent_prs","percent_electric",
                "percent_detached","percent_semi_detached", "percent_terraced",
                "percent_flats_purpose","percent_converted", "percent_asia",
                "percent_pre_1919","percent_1919_1944", "percent_beds_below",
                "percent_fg","percent_fixed_room",
                "E12000002","E12000003","E12000004","E12000005","E12000006",
                "E12000007","E12000008")

test_mod2 <- lm(prs_nd_pct ~ ., data = full[,c("prs_nd_pct",predictors)])

summary(test_mod2)

# split into test train --------------------------------------------------

outcome <- full$prs_nd_pct
full_predictors <- full[,predictors]
set.seed(123)
in_train <- createDataPartition(outcome, p = 0.8, list = F)
train_predictors <- full_predictors[in_train,] |> as.data.frame()
test_predictors <- full_predictors[-in_train,] |> as.data.frame()
train_outcome <- outcome[in_train]
test_outcome <- outcome[-in_train]

# scaling and centering
x_trans <- preProcess(train_predictors)
train_predictors <- predict(x_trans, train_predictors)
test_predictors <- predict(x_trans, test_predictors)
full_predictors <- predict(x_trans, full_predictors)

# Defining the training controls for multiple models -----------------------------

fitControl <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = "final"
)

# training lower layer models -------------------------------------------------

#Training the random forest model
set.seed(123)
model_rf <- train(train_predictors,
                  train_outcome,
                  method='rf',
                  trControl=fitControl,
                  tuneLength=5)

model_rf

#Training the lm model
model_lm <- train(train_predictors,
                  train_outcome,
                  method='lm',
                  trControl=fitControl)

model_lm

#Training the lasso model
lasso_grid <- data.frame(fraction = seq(.5, 1, length = 10))
set.seed(123)
model_ls <- train(train_predictors,
                  train_outcome,
                  method='lasso',
                  tuneGrid = lasso_grid,
                  trControl=fitControl)

model_ls 

#Training the gam model
set.seed(123)
model_gm <- train(train_predictors,
                  train_outcome,
                  method='gamSpline',
                  trControl=fitControl,
                  tuneLength=3)

model_gm

#Training the nn model
my_grid <- expand.grid(.decay = c(0.9, 0.5, 0.1), .size = c(5:10))

model_nn <- train(train_predictors,
                  train_outcome,
                  method='nnet',
                  tuneGrid = my_grid,
                  trControl=fitControl,
                  linout = TRUE,
                  maxit = 1000)
model_nn

test_predictors$pred_rf <- predict(object = model_rf, test_predictors)
test_predictors$pred_lm <- predict(object = model_lm, test_predictors)
test_predictors$pred_ls <- predict(object = model_lm, test_predictors)
test_predictors$pred_nn <- predict(object = model_nn, test_predictors)
test_predictors$pred_gm <- predict(object = model_gm, test_predictors)

#Predicting the out of fold prediction non-decent % for training data
train_predictors$OOF_pred_rf <- model_rf$pred$pred[order(model_rf$pred$rowIndex)]
train_predictors$OOF_pred_lm <- model_lm$pred$pred[order(model_lm$pred$rowIndex)]
train_predictors$OOF_pred_ls <- model_ls$pred$pred[order(model_nn$pred$rowIndex)]
train_predictors$OOF_pred_nn <- model_nn$pred$pred[order(model_nn$pred$rowIndex)]
train_predictors$OOF_pred_gm <- model_gm$pred$pred[order(model_gm$pred$rowIndex)]

#Predicting non-decent % for the test data
test_predictors$OOF_pred_rf <- predict(model_rf, test_predictors)
test_predictors$OOF_pred_lm <- predict(model_lm, test_predictors)
test_predictors$OOF_pred_ls <- predict(model_ls, test_predictors)
test_predictors$OOF_pred_nn <- predict(model_nn, test_predictors)
test_predictors$OOF_pred_gm <- predict(model_gm, test_predictors)

# no point including lasso as it's equivalent to lm
plot(test_predictors$pred_lm, test_predictors$pred_ls)
mean(test_predictors$pred_lm == test_predictors$pred_ls)

# ensemble model -----------------------------------------------------------

#Predictors for top layer models 
predictors_top <- c('OOF_pred_rf', 'OOF_pred_lm', 'OOF_pred_nn', 'OOF_pred_gm') 

#lm as top layer model 
model_elm <- train(train_predictors[,predictors_top],
                   train_outcome,
                   method = 'lm',
                   trControl = fitControl)

#gam as top layer model 
model_egm <- train(train_predictors[,predictors_top],
                   train_outcome,
                   method = 'gamSpline',
                   trControl = fitControl,
                   tuneLength = 3)

#nnet as top layer model 
model_enn <- train(train_predictors[,predictors_top],
                   train_outcome,
                   method = 'nnet',
                   tuneGrid = my_grid,
                   trControl=fitControl,
                   linout = TRUE,
                   maxit = 1000)

#predict using lm top layer model
test_predictors$pred_elm <- predict(model_elm, test_predictors[,predictors_top])
test_predictors$pred_egm <- predict(model_egm, test_predictors[,predictors_top])
test_predictors$pred_enn <- predict(model_enn, test_predictors[,predictors_top])

test_predictors <- test_predictors %>% 
  mutate(
    pred_avg = (OOF_pred_rf + OOF_pred_lm + OOF_pred_nn + OOF_pred_gm + OOF_pred_ls)/5
  )

# RMSE on test data ----------------------------------------------------------

rmse <- function(x){
  sqrt(mean((test_outcome - test_predictors[,x])^2))
}

preds <- test_predictors |> select(starts_with("pred")) |> names()

rmse_vec <- rep(NA, length(preds))
names(rmse_vec) <- preds

for(i in seq_along(preds)){
  rmse_vec[i] <- rmse(preds[i])
}

rmse_vec
plot(rmse_vec)
min(rmse_vec)

# best predictor is ensemble model with GAM as stacked layer
# using LM as stacked layer to prevent overfitting

#Predictors for top layer models without GAM
predictors_top2 <- c('OOF_pred_rf', 'OOF_pred_lm', 'OOF_pred_nn') 
#lm as top layer model 
model_elm2 <- train(train_predictors[,predictors_top2],
                   train_outcome,
                   method = 'lm',
                   trControl = fitControl)

test_predictors$pred_elm2 <- predict(model_elm2, test_predictors[,predictors_top2])
rmse("pred_elm2")

# Predicting outcome for the whole dataset using best model ---------------------------

full <- as.data.frame(full)
full_predict <- predict(x_trans, full)

full_predict$OOF_pred_rf <- predict(model_rf, full_predict[,predictors])
full_predict$OOF_pred_lm <- predict(model_lm, full_predict[,predictors])
full_predict$OOF_pred_nn <- predict(model_nn, full_predict[,predictors])
full_predict$OOF_pred_gm <- predict(model_gm, full_predict[,predictors])
full_predict$pred_elm <- predict(model_elm, full_predict[,predictors_top])

# distribution of predictions and real values ---------------------------------

full_predict %>% 
  ggplot() +
  geom_density(aes(x = pred_elm), fill = "lightgrey", alpha = 0.5) +
  geom_density(aes(x = prs_nd_pct), fill = "lightblue", alpha = 0.5) +
  theme_bw()

# visualising distribution of residuals
full_predict %>% 
  mutate(res = prs_nd_pct - pred_elm) |>  
  ggplot(aes(x = res)) +
  geom_histogram(aes(y = after_stat(density)), colour = "black", fill = "lightgrey") +
  geom_density() +
  theme_bw()

full_predict |> 
  mutate(res = prs_nd_pct - pred_elm, .before = 3) |> 
  slice_max(res, n = 10)

full_predict |> 
  mutate(res = prs_nd_pct - pred_elm, .before = 3) |> 
  slice_min(res, n = 10)

# saving model ------------------------------------------------------

save(model_lm, file = "lm_for_prs.RData")
save(model_rf, file = "rf_for_prs.RData")
save(model_nn, file = "nn_for_prs.RData")
save(model_gm, file = "gm_for_prs.RData")
save(model_elm, file = "ensemble_model_prs.RData")
