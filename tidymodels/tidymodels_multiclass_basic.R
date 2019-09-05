#' ---
#' title: "tidymodels_multiclass_basic"
#' author: "Srikanth KS"
#' date: "`r Sys.Date()`"
#' output:
#'   html_document:
#'     theme: default
#'     highlight: default
#'     df_print: kable
#'     css:
#'        - !expr system.file("includes/stylesheets/summarytools.css", package = "summarytools")
#' ---

#' ----

#+ include = FALSE
knitr::opts_chunk$set(echo       = TRUE
                      , include  = TRUE
                      , warning  = FALSE
                      , message  = FALSE
                      )

summarytools::st_options(
  plain.ascii            = FALSE          
  , style                = "rmarkdown"
  , dfSummary.varnumbers = FALSE
  , dfSummary.valid.col  = FALSE
  , headings             = FALSE
  , footnote             = NA
  )

#' This was adapted from https://rviews.rstudio.com/2019/06/19/a-gentle-intro-to-tidymodels/

# load tidymodels
pacman::p_load("tidymodels")

# visualize iris data
iris %>% 
  GGally::ggpairs(aes(color = Species)
                  , lower = list(combo = GGally::wrap("facethist", binwidth=0.8))
                  )

# split into train and validation using rsample::initial_split
iris_split = initial_split(iris, prop = 0.6)
iris_split # keeps a copy of data

# realise train and validation data
iris_train = training(iris_split)
iris_valid = testing(iris_split)

# setup a recipe from recipes package
iris_recipe = 
  training(iris_split) %>%
      # specify design matrix
  recipe(Species ~ .) %>%
      # remove highly correlated features (> 90%)
  step_corr(all_predictors()) %>%
      # center and scale all predictors (in this case numeric) and not y
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes())

# print and confirm the recipe
iris_recipe

# prepare the recipe
iris_recipe_prep = 
  iris_recipe %>% 
  prep()

# replacing in-place
iris_valid = bake(iris_recipe_prep, iris_valid)
iris_train = bake(iris_recipe_prep, iris_train) # 'juice' would have worked for train

# create fit specification
iris_xgboost_spec = 
  parsnip::boost_tree() %>% 
  set_engine("xgboost")

# fit
iris_xgboost_fit = 
  iris_xgboost_spec %>% 
  fit(Species ~ ., data = iris_train)

# predict always returns a tibble
preds = 
  predict(iris_xgboost_fit, iris_valid, type = "prob") %>% 
  bind_cols(iris_valid) %>% 
  bind_cols(predict(iris_xgboost_fit, iris_valid, type = "class"))

# metrics (multiclass probs)
preds %>% 
  metrics(.pred_setosa, .pred_versicolor, .pred_virginica
          , truth = Species
          , estimate = .pred_class
          )

# gain curve per class
preds %>%
  gain_curve(Species, .pred_setosa:.pred_virginica) %>% 
  autoplot()

# lift curve per class
preds %>%
  lift_curve(Species, .pred_setosa:.pred_virginica) %>% 
  autoplot()

# roc-curve per class
preds%>%
  roc_curve(Species, .pred_setosa:.pred_virginica) %>%
  autoplot()

# ----
