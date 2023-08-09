# install.packages("pacman")


# load lib and data -------------------------------------------------------
pacman::p_load("car", "gapminder", "GGally", "gridExtra", "naniar", "rio", "skimr",
               "tidyverse")

# https://www.kaggle.com/datasets/ajaypalsinghlo/world-happiness-report-2023

#data import is wrong.
whr23 <- import(here::here("data","whr23.dta") )
row.names(whr23) <- whr23$country
whr23 <- whr23 %>%
   mutate(continent = factor(continent, labels = c("Africa", "Americas",
                                                   "Asia", "Europe",
                                                   "Oceania"))) %>%
   select(c(-country,-stderr))
head(whr23)


# missings ----------------------------------------------------------------
source("ggplot_theme_Publication.R")
gg_miss_var(whr23, show_pct = TRUE) +
   labs(y = "% Missing") +
   theme_Publication()


# univariat stat ----------------------------------------------------------
#5-Number Summary
skimmed <- skim(whr23)
yank(skimmed, "numeric")

#Distributions
whr23 <- na.omit(whr23)
ggplot(whr23, aes(continent, fill = continent)) +
   geom_bar(aes(y = 100*(..count..)/sum(..count..)),alpha = 0.75,
            show.legend = FALSE) +
   xlab("") +
   ylab("Percent") +
   theme_Publication()

ggplot(whr23, aes(x = happiness)) +
   geom_histogram(aes(y = stat(density)),
                  fill = "#386cb0", alpha = 0.75) +
   geom_density(stat = "density", color = "#ef3b2c", size = 1) +
   xlab("Happiness") +
   ylab("Density") +
   theme_Publication()

ggplot(whr23, aes(x = freedom)) +
   geom_boxplot(fill = "#386cb0", alpha = 0.75) +
   xlab("Freedom to Make Life Choices") +
   theme_Publication()


# bivariate stat ----------------------------------------------------------

whr23sub <- whr23 %>%
   select(happiness, freedom, generosity)
p <- ggpairs(whr23sub,
             lower = list(continuous = "smooth_loess"),
             upper = list(continuous = wrap(ggally_cor, displayGrid = FALSE)))
p + theme_Publication()


#Variance Inflation Factors
test_fit <- lm(happiness ~ logpercapgdp + socialsupport +
                  healthylifeexpectancy + freedom + generosity +
                  perceptionsofcorruption,
               data = whr23)
vif(test_fit)


