#Script of the course
# sections are days



# setup -------------------------------------------------------------------

rm(list = ls())

# day 1: script 1 -------------------------------------------------------------------

## 1. Step rsample ---------------------------------------------------------
#load data with rsample
library(rio)
library(tidyverse)
happy_df <- import(here::here("data", "whr23.dta"))
row.names(happy_df) <- happy_df$country
happy_clean <- happy_df %>%
   select(
      happiness,
      logpercapgdp,
      socialsupport,
      healthylifeexpectancy,
      freedom,
      generosity,
      perceptionsofcorruption
   ) %>%
   na.omit

rm(happy_df)

#split
library(tidymodels)
#tidymodels_prefer() uses the conflicted package to handle common 
#conflicts with tidymodels and other packages.
tidymodels_prefer()
set.seed(2922)     #set always same random data
happy_split <-
   initial_split(happy_clean, 
                 prop = 0.6, 
                 strata = happiness  #this will stratified by happiness, 
                 #every stratos has same selectionchance
   )
happy_split
rm(happy_clean)
#80 Train, 56 Test, 136 Total
#in_id = Train
happy_split$in_id

#splitting the dataset
training_set <- training(happy_split)
test_set <- testing(happy_split)

rm(happy_split)

#checking
# Exact two-sample Kolmogorov-Smirnov test
# to compare the two samples. See if the distributions are
# identical in the tow subsample
ks.test(training_set$happiness, test_set$happiness)


## 2. Step recipes -----------------------------------------------------
# skiped




## 3. Step parsnip ---------------------------------------------------------
#modelling

happy_fit <- linear_reg() %>%    #defines a model, could also define mode and engine here inside.
   set_mode("regression") %>%    #is used to change the model's mode.
   set_engine("lm") %>%          #specify which package or system will be used to fit the model,
   fit(happiness ~ .,            # "." everything what is not happiness using as 
       # predicting variable
       data = training_set)

#inspect 1
yardstick::tidy(happy_fit)



#inspect 2
#predicter performance on the test-set
glance(happy_fit) #not so bad with 81% R-sqr
#Bemerkung: wenn man modelle auf aggregierten Surveydaten macht, ist
# es nicht ungewöhnlich, dass man so hoche r-Quadrat hat. Auf den 
# Rohdaten hat man niemals so hohe r-Quadrate



## 4. Step yardstick -----------------------------------------------------
#Predictions
# function predict is a generic function for predictions from the results of various
# model fitting functions. The function invokes particular methods which
# depend on the class of the first argument.

happy_fit %>%
   stats::predict(test_set) %>%  #first column is the prediction of happyness
   bind_cols(test_set)  #empirical data of the test_set

#metrics
happy_fit %>%
   predict(test_set) %>%
   bind_cols(test_set) %>%
   metrics(truth = happiness, estimate = .pred)
# This function estimates one or more common performance estimates 
# depending on the class of truth (see Value below) and returns them
# in a three column tibble.


## 5. Step Interpretation --------------------------------------------------

# Partial Dependence Plot
# DALEX = Descriptive mAchine Learning EXplanations (Biecek, 2018)


library(DALEXtra)
#dalextra can not handle tidymodels object. thats why it needs a "translation" = explainer
happy_explainer <- explain_tidymodels(
   model = happy_fit,
   data = test_set,
   y = test_set$happiness
)

# Plot it

# Dataset Level Variable Profile as Partial Dependence 
# or Accumulated Local Dependence Explanations
pdp_gdp <- model_profile(
   happy_explainer,
   variables = "logpercapgdp",       #da wähle ich die Variable aus, welche ich Plotten will.
   N = NULL
)


as_tibble(pdp_gdp$agr_profiles) %>%
   
   ggplot(aes(`_x_`, `_yhat_`)) +
   
   geom_line(size = 1.2, alpha = 0.8) +
   
   geom_rug(sides = "b") +
   
   labs(
      x = "Log Per Capita GDP",
      y = "Predicted Happiness",
      color = NULL
   ) +
   
   ggthemes::theme_economist()

#beim Plot auf der x-Achse sehe ich kleine Striche. Dies zeigen an, wo überhaupt
# GDP-Werte gemessen wurden


# day 1: script 2 ---------------------------------------------------------
rm(list = ls())
happy_df <- import(here::here("data","whr23.dta"))
head(happy_df)


## data praparation --------------------------------------------------------

# Drop ISO3 and stderr.
# Turn continent into a proper factor
# Drop missing values (since we have no information about them except for their location)

work_df <- happy_df %>%
   mutate(continent = factor(continent,
                             labels = c("Africa", "Americas", "Asia",
                                        "Europe", "Oceania"))) %>%
   select(-c(stderr, ISO3)) %>%
   na.omit
rm(happy_df)

## 1. Step rsample ---------------------------------------------------------
#split dataset

set.seed(1560)
happy_split <- initial_split(work_df, prop = 0.6)
training_set <- training(happy_split)
test_set <- testing(happy_split)
rm(work_df)


## 2. Step recipes ---------------------------------------------------------
happy_recipe <- 
   recipe(happiness ~ ., data = training_set) %>%
   update_role(country, new_role = "id") %>% #so country will be ignored as a predicter variable
   step_dummy(continent)                     #creats 4 dummy variables for the continent


yardstick::tidy(happy_recipe)

# update_role() alters an existing role in the recipe or assigns an initial 
# role to variables that do not yet have a declared role.



## 3. Step parsnip ---------------------------------------------------------
#modelling

happy_model <-
   linear_reg() %>%
   set_mode("regression") %>%
   set_engine("lm")

#define workflow in the script
happy_flow <- 
   workflow() %>%                 #workflow makes a empty object
   add_model(happy_model) %>%     #add things to the workflow
   add_recipe(happy_recipe)       #add things to the workflow


#fit
happy_fit <- fit(happy_flow, training_set)
yardstick::tidy(happy_fit)     #does a kind of a summary of the object


# day 1: script 3 ---------------------------------------------------------

library(rio)
library(tidymodels)
library(tidyverse)
tidymodels_prefer()
happy_df <- import(here::here("data", "whr23.dta"))
row.names(happy_df) <- happy_df$country
happy_clean <- happy_df %>%
   select(happiness,logpercapgdp,socialsupport,healthylifeexpectancy,
          freedom,generosity,perceptionsofcorruption) %>%
   na.omit
set.seed(10)
happy_split <- initial_split(happy_clean, prop = 0.8, strata = happiness)
happy_train <- training(happy_split)
happy_test <- testing(happy_split)


## 4b. Validation // re-sampling --------------------------------------------------------
# Lösung von Re-Sampling Problem

val_set <- validation_split(happy_train, prop = 3/4)
val_set


#vanilla Validierung
set.seed(1923)
vanilla_folds <- vfold_cv(happy_train,
                          v = 10           #k = 10
)
vanilla_folds
rm(vanilla_folds)

#Repeated Validierung
set.seed(1923)
repeated_folds <- vfold_cv(happy_train,
                           v = 10,      #k = 10 fold
                           repeats = 5 #repeated 5 times
)
repeated_folds
rm(repeated_folds)

# LOOCV Validierung
#Alle auser eine Beobachtung
set.seed(1923)
loocv_folds <- loo_cv(happy_train)
loocv_folds

rm(loocv_folds)
# Monte Carlo Validierung
set.seed(1923)
mc_folds <- mc_cv(happy_train, 
                  prop = 9/10, #9 von 10 werden für Training benutzt, WITH replacement.
                  times = 20   #20x
)
mc_folds
rm(mc_folds)

#Bootstrap in tidymodels
set.seed(1923)
booted <- bootstraps(happy_train, times = 100)
booted

rm(booted)


# day 1: PLS - Partial last squares ----------------------------------------------


## Setting Up for PLS ------------------------------------------------------

#Prepare
if (!require("remotes", quietly = TRUE)) {
   install.packages("remotes")
}
remotes::install_bioc("mixOmics")



#data
head(happy_train)


#Pre-Process
happy_recipe <- recipe(happiness ~ .,
                       data = happy_train) %>%
   step_normalize(all_numeric_predictors()) %>%  #da wird eig standardisiert, nicht normalisiert
   step_pls(all_numeric_predictors(),            #da passiert die eigentliche PLS berechnung
            outcome = "happiness",               # da spezifiziert man den outcome
            num_comp = tune())                   # da wird festgelegt, wie viele Paramenter
# genommen werden. Man könnte also auch einfach eine Zahl eingeben


# CV
set.seed(20)
cv_folds <- vfold_cv(happy_train,
                     v = 10,                       #anzahl folds (also random Re-Samples)
                     repeats = 3)                  #das ganze 3x genommen.
cv_folds

# Tunegrid
pls_grid <- tibble(num_comp = 1:6)     #num_comp = anzahl componenten, das soll zwingend
# num_comp heissen

# Modell
happy_model <-
   linear_reg() %>%
   set_mode("regression") %>%
   set_engine("lm")

#Metric
happy_metric <- metric_set(rsq)                 

# flow
happy_flow <- workflow() %>%
   add_model(happy_model) %>%
   add_recipe(happy_recipe)


## Selecting the Number of Components --------------------------------------

#syntax
set.seed(30)
happy_tune <- happy_flow %>%
   tune_grid(cv_folds,
             grid = pls_grid,
             metrics = happy_metric)

# tune_grid() computes a set of performance metrics (e.g. accuracy or RMSE)
# for a pre-defined set of tuning parameters that correspond to a model or 
# recipe across one or more resamples of the data.

#plot

autoplot(happy_tune) +
   theme_light() +
   labs(title = "Parameter Tuning for PLS Model")

#optimum
happy_best <-select_best(happy_tune)
happy_best


## Finalizing the Model ----------------------------------------------------

# final train
num_comp <- select_best(happy_tune)           #könnte man auch manuell festlegen
#einfach indem man sagt, nimm x komponenten


final_flow <- happy_flow %>%
   finalize_workflow(num_comp)

final_est <- final_flow %>%
   fit(happy_train)

# parameters
tidy(final_est)


# plot syntax
final_recipe <- recipe(happiness ~ ., 
                       data = happy_train) %>%
   step_normalize(all_numeric_predictors()) %>%
   step_pls(all_numeric_predictors(), 
            outcome = "happiness",
            num_comp = 1)                  #1 komponente, da dies die beste lösung war.

pls_prep <- prep(final_recipe)

tidied_pls <- tidy(pls_prep, 2)

tidied_pls %>%
   group_by(component) %>%
   slice_max(abs(value), n = 5) %>%
   ungroup() %>%
   ggplot(aes(value, fct_reorder(terms,value))) +
   geom_col(show.legend = FALSE,  fill = "#386cb0") +
   facet_wrap(~ component, scales = "free_y") +
   labs(y = NULL) +
   theme_bw()


## Deployment on the Test Set ----------------------------------------------
# fit to test
pls_testfit <- final_est %>%
   predict(happy_test) %>%
   bind_cols(happy_test) %>%
   metrics(truth = happiness, estimate = .pred)
# metrics
pls_testfit


# day 1: Elastic Nets ------------------------------------------------------------
# workflow
model_recipe <- recipe(happiness ~ .,
                       data = happy_train) %>%
   step_normalize(all_numeric_predictors())

elastic_spec <-  linear_reg(penalty = tune(),         #penalty = Landa
                            mixture = tune()) %>%     #mixture  = alpha
   #mixture = 1 -> lasso verfahren
   # mixture = 0 -> Tikonovverfahren
   # wenn = tune() dann elastic net, da man dem algorithmus offen lässt welches verfahren und
   # der algo so selber lernt und entscheidet.
   set_mode("regression") %>%
   set_engine("glmnet")                 #wir nutzen ein anderes "engine"

elastic_wf <- workflow() %>%
   add_model(elastic_spec) %>%
   add_recipe(model_recipe)

elastic_metrics <- metric_set(rsq)

# grid
glmnet_param <- extract_parameter_set_dials(elastic_spec)

elastic_grid <- grid_regular(glmnet_param, levels = 5)         #create grid
#normalerweise schaut dann 10'000 kombinationen an

print(elastic_grid, n = 6)
# Penalty sind sehr klein bei Szenario 1-4, 6.
# aber gross bei Szenario 5

# tuning
doParallel::registerDoParallel()

elastic_tune <-
   elastic_wf %>%
   tune_grid(cv_folds,
             grid = elastic_grid,
             metrics = elastic_metrics)


elastic_best <- select_best(elastic_tune)
elastic_best
#hier machen wir 100% lasso , da mixture = 1 ist.
#mixture zeigt an, wie viel lasso gemacht wird und wie viel Tikhonov gemacht wird.

autoplot(elastic_tune)

# finalize
elastic_updated <- finalize_model(elastic_spec,
                                  elastic_best)

workflow_new <- 
   elastic_wf %>%
   update_model(elastic_updated)

new_fit <- 
   workflow_new %>%
   fit(data = happy_train)

tidy_elastic <- 
   new_fit %>%
   extract_fit_parsnip() %>%
   tidy()

# estimate
tidy_elastic



# day 1: GAM generalized additiv model ------------------------------------


##  Example 1: Polynomial Regression Without Tuning ------------------------


# objective
# The objective is to fit a polynomial regression of degree 3 to the age 
# predictor in a wage model.
# rm(list = ls())

# data
install.packages("ISLR")
library(ISLR)
library(tidymodels)
tidymodels_prefer()
data(Wage)
set.seed(1671)
wage_split <- initial_split(Wage, prop = 0.75, strata = wage)

training_set <- training(wage_split)

test_set <- testing(wage_split)

# Pre-Process
# Standardization is not necessary but often useful
model_recipe <- 
   recipe(wage ~ age, data = training_set) %>%
   step_normalize(all_numeric_predictors()) %>%
   step_poly(age,                                   #das ist eig. der kern!)
             degree = 3)                            #polynom 3 Grades

#Training

poly_spec <- 
   linear_reg() %>%
   set_mode("regression") %>%
   set_engine("lm")

model_flow <- 
   workflow() %>%
   add_model(poly_spec) %>%
   add_recipe(model_recipe)

wage_fit <-
   fit(model_flow,
       data = training_set)

tidy(wage_fit)


# performance 
wage_fit %>%
   predict(test_set) %>%
   bind_cols(test_set) %>%
   metrics(truth = wage, estimate = .pred)

# rsq = nur 8%....




##  Example 2: Natural Splines with Tuning ---------------------------------
# objective 
# Let us train a linear model of wage with age, education, and year of data collection as predictive features.
# We create dummy variables for education.
# We use natural splines for age and year.
# We tune the degrees of freedom for those splines.

# recipe
wage_recipe <- 
   recipe(wage ~ age + education + year,           #kombinertes modell mit 3 Variablen
                      data = training_set) %>%
   step_normalize(c(age,year)) %>%                   #normalisieren, Zentriert um Mean
   step_dummy(education) %>%                         # dummy von Bildung
   step_ns(age, deg_free = tune("age")) %>%          #ns = natrural splieses
   step_ns(year, deg_free = tune("year"))

# step_ns() creates a specification of a recipe step 
# that will create new columns that are basis expansions 
# of variables using natural splines.
extract_parameter_set_dials(wage_recipe)
#interpretation [+] ist gut
# [?] ist nicht gut! denn dann wird die Variable nicht getuned.



#CV = crossvalidation
set.seed(20)
cv_folds <- vfold_cv(training_set, 
                     v = 10, 
                     repeats = 3)


# grid 
# Full factorial
ns_grid <- crossing(
   "age" = 1:5,              #5 degrees of freedom values of age
   "year" = 1:5              #dito
)

ns_grid                       # ergibt 25 kombinationen

# Model+flow
#auch wenn es momentan och linear ist, wird am Schluss ein GAM rauskommen.
wage_spec <- 
   linear_reg() %>%
   set_mode("regression") %>%
   set_engine("lm")

first_flow <-
   workflow() %>%
   add_model(wage_spec) %>%
   add_recipe(wage_recipe)


 # tuning
set.seed(1671)

ns_tune <- 
   first_flow %>%
   tune_grid(cv_folds, 
             grid = ns_grid, 
             metrics = metric_set(rsq))

select_best(ns_tune)

#final
final_flow <- 
   first_flow %>%
   finalize_workflow(select_best(ns_tune))

final_est <- 
   final_flow %>%
   fit(training_set)

tidy(final_est)


#effect Plot
#GAM
library(gam)
gam_fit <- gam(wage ~ education + ns(age, 5) + ns(year, 1),
               data = training_set)
par(mfrow = c(1,3))
plot(gam_fit, se = FALSE, col = "#386cb0")


 

