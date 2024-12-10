library(tidymodels)
library(modeltime)
library(timetk)
library(readr)
library(embed)
library(patchwork)
library(vroom)
library(bonsai)

train_data <- read_csv("train.csv")
test_data <- read_csv("test.csv")


# nStores <- 2
# nItems <- 2
nStores <- max(train_data$store)
nItems <- max(train_data$item)

my_model <- boost_tree(tree_depth = 2,
                       trees = 1000, 
                       learn_rate = .01) %>%
  set_engine("lightgbm") %>%
  set_mode("regression")

recipe <- recipe(sales ~ date, data = train_data) %>%
  step_date(date, features=c("dow", "month", "year", "doy")) %>%
  step_range(date_doy, min = 0, max = pi) %>%
  step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy)) %>%
  step_lencode_mixed(all_nominal_predictors(), outcome=vars(sales)) %>%
  step_rm(date) %>%
  step_normalize(all_numeric_predictors())

for(s in 1:nStores){
  for(i in 1:nItems){
    storeItemTrain <- train_data %>%
      filter(store==s, item==i)
    storeItemTest <- test_data %>%
      filter(store==s, item==i)
    
    tree_wf <- workflow() %>%
      add_recipe(recipe) %>%
      add_model(my_model) %>%
      fit(data=storeItemTrain)
   
    preds <- tree_wf %>%
      predict(new_data = storeItemTest) %>%
      mutate(id = storeItemTest$id) %>%
      rename(sales = .pred) %>%
      select(id, sales)
    
    if(s == 1 & i == 1){
      all_preds <- preds
    } else {
      all_preds <- bind_rows(all_preds, preds)
    }
  }
}

vroom_write(all_preds, "submission.csv", delim = ",")
