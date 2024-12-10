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

# nStores <- 2
# nItems <- 2

recipe <- recipe(sales ~ date, data = train_data) %>%
  step_date(date, features=c("dow", "month", "year", "decimal")) %>%
  step_mutate(date_dow = factor(date_dow), 
              date_month = factor(date_month), 
              sinDecimal = sin(date_decimal)) %>%
  step_lencode_mixed(all_nominal_predictors(), outcome=vars(sales))

# prophet_model <- prophet_reg() %>%
#   set_engine(engine = "prophet")
# 
# cv_split <- time_series_split(train_data_subset, assess="3 months", cumulative=TRUE)
# cv_split %>%
#   tk_time_series_cv_plan() %>% #Put into a data frame
#   plot_time_series_cv_plan(date, sales, .interactive=FALSE)
# 
# prophet_wf <- workflow() %>%
#   add_recipe(recipe) %>%
#   add_model(prophet_model) %>%
#   fit(data=training(cv_split))
# 
# cv_results <- modeltime_calibrate(prophet_wf,
#                                   new_data=testing(cv_split))
# 
# cv_results %>%
#   modeltime_forecast(new_data = testing(cv_split),
#                      actual_data = training(cv_split)) %>%
#   plot_modeltime_forecast(.interactive=FALSE)
# 
# fullfit <- cv_results %>%
#   modeltime_refit(data = train_data_subset)

arima_model <- arima_reg(seasonal_period=12,
                         non_seasonal_ar=5,
                         non_seasonal_ma=5,
                         seasonal_ar=2,
                         seasonal_ma=2,
                         non_seasonal_differences=2,
                         seasonal_differences=2) %>%
  set_engine("auto_arima")

cv_split <- time_series_split(train_data_subset, assess="3 months", cumulative=TRUE)
cv_split %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

arima_wf <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split))

cv_results <- modeltime_calibrate(arima_wf,
                                  new_data=testing(cv_split))

cv_results %>%
  modeltime_forecast(new_data = testing(cv_split),
                     actual_data = training(cv_split)) %>%
  plot_modeltime_forecast(.interactive=FALSE)

fullfit <- cv_results %>%
  modeltime_refit(data=train_data_subset)

preds <- fullfit %>%
  modeltime_forecast(new_data = test_data, actual_data = train_data_subset) %>%
  filter(.key == "prediction") %>%
  select(.value) %>%
  mutate(id = test_data$id) %>%
  rename(sales = .value) %>%
  select(id, sales)


vroom_write(preds, "submission.csv", delim = ",")
