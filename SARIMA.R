library(tidymodels)
library(modeltime)
library(timetk)
library(readr)
library(embed)
library(patchwork)

test_data <- read_csv("test.csv")
train_data <- read_csv("train.csv")

recipe <- recipe(sales ~ date, data = train_data) %>%
  step_date(date, features=c("dow", "month", "year", "decimal")) %>%
  step_mutate(date_dow = factor(date_dow), 
              date_month = factor(date_month), 
              sinDecimal = sin(date_decimal)) %>%
  step_lencode_mixed(all_nominal_predictors(), outcome=vars(sales))

arima_model <- arima_reg(seasonal_period=12,
                         non_seasonal_ar=5,
                         non_seasonal_ma=5,
                         seasonal_ar=2,
                         seasonal_ma=2,
                         non_seasonal_differences=2,
                         seasonal_differences=2) %>%
  set_engine("auto_arima")

# Item 1

storeItem1_train <- train_data %>%
  filter(store==5, item==34)
storeItem1_test <- test_data %>%
  filter(store==5, item==34)

cv_split <- time_series_split(storeItem1_train, assess="3 months", cumulative=TRUE)
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
  modeltime_refit(data=storeItem1_train)




# Item 2

storeItem2_train <- train_data %>%
  filter(store==9, item==12)
storeItem2_test <- test_data %>%
  filter(store==9, item==12)

cv_split2 <- time_series_split(storeItem2_train, assess="3 months", cumulative=TRUE)
cv_split2 %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)


arima_wf <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split2))

cv_results <- modeltime_calibrate(arima_wf,
                                  new_data=testing(cv_split2))

cv_results %>%
  modeltime_forecast(new_data = testing(cv_split2),
                     actual_data = training(cv_split2)) %>%
  plot_modeltime_forecast(.interactive=FALSE)

fullfit <- cv_results %>%
  modeltime_refit(data=storeItem2_train) %>%
  plot_modeltime_forecast(.interactive=FALSE)


# plots


# Cross-validation predictions for storeItem1
cv_plot1 <- cv_results %>%
  modeltime_forecast(new_data = testing(cv_split),
                     actual_data = training(cv_split)) %>%
  plot_modeltime_forecast(.interactive = FALSE) +
  ggtitle("Store 5, Item 34: CV Predictions")

# 3-month forecasts for storeItem1
forecast_plot1 <- fullfit %>%
  modeltime_forecast(new_data = storeItem1_test,
                     actual_data = storeItem1_train) %>%
  plot_modeltime_forecast(.interactive = FALSE) +
  ggtitle("Store 5, Item 34: 3-Month Forecast")

# Cross-validation predictions for storeItem2
cv_plot2 <- cv_results %>%
  modeltime_forecast(new_data = testing(cv_split2),
                     actual_data = training(cv_split2)) %>%
  plot_modeltime_forecast(.interactive = FALSE) +
  ggtitle("Store 9, Item 12: CV Predictions")

# 3-month forecasts for storeItem2
forecast_plot2 <- fullfit %>%
  modeltime_forecast(new_data = storeItem2_test,
                     actual_data = storeItem2_train) %>%
  plot_modeltime_forecast(.interactive = FALSE) +
  ggtitle("Store 9, Item 12: 3-Month Forecast")

# Combine into 4-panel plot
plot <- (cv_plot1 + cv_plot2) /
  (forecast_plot1 + forecast_plot2)

ggsave("forecast.png")
