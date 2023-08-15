# 03_Day_2_course_script


# step 1 - distance -------------------------------------------------------


library(rio)
library(tidyverse)
world22_df <- import(here::here("data","world22.xlsx") )
row.names(world22_df) <- world22_df$COUNTRY
temp <- world22_df %>%
   select(STABILITY, GOVEFF, RULEOFLAW) %>%
   na.omit


# euclidean
# First four instyances shown
dist(temp[1:4,], 
     method = "minkowski",
     p = 2)

# manhatten
# First four instyances shown
dist(temp[1:4,],
     method = "minkowski", 
     p = 1)




# Predicting Whether a Country Is a Democracy -----------------------------

# 
# We use data from the 2022 EconomistIntelligenceUnit ’s democracy index to
# distinguish between democracies and other regimes.
# We use three predictive features measured by the World Bank for 2021:
#    Country rank on political stability and the absence of violence.
# Country rank on government effectiveness.
# Country rank on the rule of law.

##setup --------------
# data
work_df <- 
   tibble(world22_df) %>%
   column_to_rownames(var = "COUNTRY") %>%
   mutate(DEMOCRACY = ifelse(REGIME=="Flawed Democracy"|
                                REGIME=="Full Democracy", 1, 2),
          DEMOCRACY = factor(DEMOCRACY, labels = c("Democracy", "Other"))) %>%
   select(DEMOCRACY, STABILITY, GOVEFF, RULEOFLAW) %>%
   na.omit

work_df[1:5,]


# split sample
library(tidymodels)
tidymodels_prefer()
set.seed(1561)
demo_split <- initial_split(work_df, prop = 0.75, strata = DEMOCRACY)
demo_train <- training(demo_split)
demo_test <- testing(demo_split)
demo_split
#124 zum trainieren, 42 zum testen



# recipe 
# Normalization is not strictly required because the predictive features
# are on the same scale in this example. Still, it is good to get into
# the habit.
demo_recipe <- recipe(DEMOCRACY ~ .,                    #democracy vs rest.
                      data = demo_train) %>%
   step_normalize(all_numeric_predictors())

##model -----
demo_spec <- nearest_neighbor(neighbors = tune(),
                              weight_func = "optimal",
                              dist_power = tune()) %>%
   set_mode("classification") %>%
   set_engine("kknn")


##flow -----
demo_flow <- workflow() %>%
   add_model(demo_spec) %>%
   add_recipe((demo_recipe))


##tuning-----
##RE-Sampling 
set.seed(1891)
cv_folds <- vfold_cv(demo_train,
                     v = 10,
                     repeats = 3)

##grid 
# We set an odd number of neighbors to avoid ties in voting
knn_grid <- crossing(
   neighbors = seq(from = 1, 
                   to = 21,
                   by = 2),      #steop by 2, so habe ich immer ungerade anzahl und
                                  # kann immer eine Mehrheit bestimmen, da nie 50-50
   dist_power = 1:2               #power between 1 and 2 (1= manhatten, 2 = euclidean)
)

## metrics 
knn_perf <- metric_set(accuracy,
                       kap)

##run 
set.seed(1012)
knn_tune <- demo_flow %>%
   tune_grid(cv_folds,
             grid = knn_grid, 
             metrics = knn_perf)


autoplot(knn_tune) +
   theme_light() +
   labs(title='Hyperparameter Tuning for kNN')

#Interpretation
#eukliden d ist besser gegeinet  bei weniger NAchbaren als Manhatten (d = 2)


##  Finalizing the Fit and Making Predictions ------------------------------


## fit 
knn_updated <- 
   finalize_model(demo_spec,
                              select_best(knn_tune))

workflow_new <-
   demo_flow %>%
   update_model(knn_updated)

new_fit <- 
   workflow_new %>%
   fit(data = demo_train)

## evaltuation 
knn_test_metrics <-
   new_fit %>%
   predict(demo_test) %>%
   bind_cols(demo_test) %>%
   metrics(truth = DEMOCRACY,
           estimate = .pred_class)

knn_test_metrics


#  Confusion Matrix -------------------------------------------------------
# prediction 
knn_testfit <- new_fit %>%
   predict(demo_test) %>%
   bind_cols(demo_test)
knn_testfit

#marix 
knn_confused <- knn_testfit %>%
   conf_mat(truth = DEMOCRACY, estimate = .pred_class)
knn_confused

#korrekt 37 von 42. 5 wurden falsch klassiert.

# statistics 
summary(knn_confused)
#ppv - positiv prediction fraction
# j_index = Youdenindex
#etc.


# k Setting Up NN Regression in tidymodels --------------------------------
## setting up -----
#splt
work_df <- tibble(world22_df) %>%
   column_to_rownames(var = "COUNTRY") %>%
   select(DEMINDX22, STABILITY, GOVEFF, RULEOFLAW) %>%
   na.omit

set.seed(1561)
demo_split <- initial_split(work_df, prop = 0.75, strata = DEMINDX22)

demo_train <- training(demo_split)
demo_test <- testing(demo_split)
demo_split

#model
demo_spec <- nearest_neighbor(neighbors = tune(),
                              weight_func = tune(),
                              dist_power = tune()) %>%
   set_mode("regression") %>%
   set_engine("kknn")

#recipe
demo_recipe <- 
   recipe(DEMINDX22 ~ ., 
          data = demo_train) %>%
   step_normalize(all_numeric_predictors()) %>%
   step_corr(threshold = 0.9)                      #ausschluss von hohen korrelationen

# flow
demo_flow <- 
   workflow() %>%
   add_model(demo_spec) %>%
   add_recipe(demo_recipe)



## tuning ----
# re-sample 
set.seed(1891)
cv_folds <- vfold_cv(demo_train, v = 10, repeats = 3)

#grid
demo_grid <- crossing(
   neighbors = 1:75,
   weight_func = c("biweight", "cos", "epanechnikov", "gaussian",
                   "inv", "optimal", "rectangula", "triweight"),
   dist_power = 1:2
)
#big grid, will take so time

#run
set.seed(1012)
knn_tune <- demo_flow %>%
   tune_grid(cv_folds, grid = demo_grid, metrics = metric_set(rsq))

#hier bleibt mein kompi stehen. gehe mal davon aus, dass es ein Error ist.
# NEIN, geht
# Echt mühsam , dass wir keine Skripts bekommen die laufen und den 
# Scheiss selber zusammenkopieren müssen!
# einfach abbrechen und dann manuell skript ausführen lassen, dann gehts!
   

autoplot(knn_tune) +
   theme_light() +
   labs(title='Hyperparameter Tuning for kNN')

#am anfang sind die kernel sehr nahe beieinander. Danach performen sie verschieden


## finalizing ----
# fit
knn_updated <- finalize_model(demo_spec,
                              select_best(knn_tune))

workflow_new <- demo_flow %>%
   update_model(knn_updated)

new_fit <- workflow_new %>%
   fit(data = demo_train)

#evaluation
knn_test_metrics <-
   new_fit %>%
   predict(demo_test) %>%
   bind_cols(demo_test) %>%
   metrics(truth = DEMINDX22, estimate = .pred)

knn_test_metrics



# logistig regression -----------------------------------------------------
rm (list = ls())
# 
# We revisit the analysis of democracy.
## This time we use a logistic regression model for the prediction and AUC as the performance metric.
# # We also add a twist:
#   We add more predictors
# # We eliminate missing values using kNN-imputation
# # Instances without labels, however, are dropped


library(rio)
library(tidyverse)
#data
world22_df <- import("Data/world22.xlsx")

work_df <- tibble(world22_df) %>%
   column_to_rownames(var = "COUNTRY") %>%
   filter(!is.na(REGIME)) %>%
   mutate(DEMOCRACY = ifelse(REGIME=="Flawed Democracy"|
                                REGIME=="Full Democracy", 1, 2),
          DEMOCRACY = factor(DEMOCRACY, labels = c("Democracy", "Other")),
          LOGPCGDP = log10(GDPPERCAP)) %>%
   select(DEMOCRACY, POP21, LITERACY, HDI21, LOGPCGDP, GDPGROWTH, GINI,
          EFIDX, STABILITY, GOVEFF, RULEOFLAW)


#split
library(tidymodels)
tidymodels_prefer()
set.seed(10)
demo_split <- initial_split(work_df, prop = 0.75, strata = DEMOCRACY)
demo_train <- training(demo_split)
demo_test <- testing(demo_split)


#recipe
demo_recipe <- recipe(DEMOCRACY ~ ., 
                      data = demo_train) %>%
   step_normalize(all_numeric_predictors()) %>%
   step_impute_knn(neighbors = 3)

#ob man imputiert kommt etwas darauf an, woher die missings kommen
# 1. missing at random? Problemlos, aber man hat weniger Daten
# 2. fehlende Kompetenzen / Wissen / Informationen
#    Da könnte man imputieren, da die Personen in etwa so antworten müssten.


#model
demo_spec <-
   logistic_reg() %>%
   set_mode("classification") %>%
   set_engine("glm")

# flow
demo_flow <- 
   workflow() %>%
   add_model(demo_spec) %>%
   add_recipe(demo_recipe)

# fit
demo_fit <- fit(demo_flow, demo_train)

# prediction 
demo_test <- 
   predict(demo_fit, demo_test, type = "prob") %>%
   bind_cols(demo_test)

demo_test %>%
   roc_auc(DEMOCRACY, .pred_Democracy)