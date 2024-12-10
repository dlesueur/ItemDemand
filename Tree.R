library(tidymodels)
library(modeltime)
library(timetk)
library(readr)
library(embed)
library(patchwork)
library(vroom)

train_data <- read_csv("train.csv")
test_data <- read_csv("test.csv")

stores <- seq(1, 10)
items <- seq(1, 50)
store_items <- expand.grid(store = stores, item = items)

# Randomly sample 50 store-item combinations
random_store_items <- store_items[sample(nrow(store_items), 50), ]

# Filter train_data and test_data based on random store-item combinations
train_data_subset <- train_data %>%
  semi_join(random_store_items, by = c("store", "item"))

test_data_subset <- test_data %>%
  semi_join(random_store_items, by = c("store", "item"))



recipe <- recipe(sales ~ date, data = train_data_subset) %>%
  step_date(date, features=c("dow", "month", "year", "decimal")) %>%
  step_mutate(date_dow = factor(date_dow), date_month = factor(date_month), 
              sinDecimal = sin(date_decimal)) %>%
  step_lencode_mixed(all_nominal_predictors(), outcome=vars(sales))

tree_model <- decision_tree(tree_depth = tune(), 
                            cost_complexity = tune(),
                            min_n = tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")

tree_wf <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(tree_model)

tuning_params <- grid_regular(tree_depth(), 
                              cost_complexity(),
                              min_n(),
                              levels = 5)

folds <- vfold_cv(train_data_subset, v = 6, repeats=1)

CV_results <- tree_wf %>%
  tune_grid(resamples=folds,
            grid=tuning_params,
            metrics=metric_set(smape))

best_params <- CV_results %>%
  select_best(metric = "smape")

final_tree_model <- tree_wf %>%
  finalize_workflow(best_params)

fitted_tree_model <- final_tree_model %>%
  fit(data = train_data_subset)

calibrated_model <- modeltime_table(fitted_tree_model) %>%
  modeltime_calibrate(new_data = train_data_subset)

preds <- calibrated_model %>%
  modeltime_forecast(new_data = test_data_subset, actual_data = train_data_subset) %>%
  filter(.key == "prediction") %>%
  select(.value) %>%
  mutate(id = test_data_subset$id) %>%
  rename(sales = .value) %>%
  select(id, sales)


vroom_write(preds, "submission.csv", delim = ",")

