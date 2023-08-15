
#'package: Intro to Statistical Learning, 2nd Ed.
#'content are mainly datasets.
install.packages("ISLR2")
install.packages("tidymodels") #this will be our main modeling environment.


library(ISLR2) 

package_list <- c("AppliedPredictiveModeling", "bonsai", "brulee",
                  "DALEXtra", "discrim", "doParallel", "gam", "ggthemes", 
                  "glmnet", "kernlab", "kknn", "mlbench", "party",
                  "partykit", "rio", "rpart", "rpart.plot", "rules",
                  "stacks", "themis", "tidymodels", "tidyverse", "xgboost", "ggthemes")

installed_packages <- package_list %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
   install.packages(package_list[!installed_packages])
}
