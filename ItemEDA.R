library(tidyverse)
library(forecast)
library(patchwork)

train_data <- read_csv("train.csv")
test_data <- read_csv("test.csv")

# get random store/item pairs
stores <- seq(1,10)
items <- seq(1,50)
store_items <- expand.grid(store = stores, item = items)
set.seed(99)
random_store_items <- store_items[sample(nrow(store_items), 2), ]
# store 2 item 44 and store 9 item 29

store2_item44 <- train_data[train_data$store == 2 & train_data$item == 44, ]
store9_item29 <- train_data[train_data$store == 9 & train_data$item == 29, ]

# column 1: time series plot
plot1 <- store2_item44 %>% 
  ggplot(mapping = aes(x=date, y=sales)) +
  geom_line() +
  geom_smooth(se=FALSE) +
  ggtitle("Store 2 Item 44")

plot2 <- store9_item29 %>%
  ggplot(mapping = aes(x=date, y=sales)) +
  geom_line() +
  geom_smooth(se=FALSE) +
  ggtitle("Store 9 Item 29")

# column 2: ACF plot 1 month lag
plot3 <- store2_item44 %>%
  pull(sales) %>%
  forecast::ggAcf() +
  ggtitle("")

plot4 <- store9_item29 %>%
  pull(sales) %>%
  forecast::ggAcf() +
  ggtitle("")

# column 3: 
plot5 <- store2_item44 %>%
  pull(sales) %>%
  forecast::ggAcf(lag.max=2*365) + 
  ggtitle("")

plot6 <- store9_item29 %>%
  pull(sales) %>%
  forecast::ggAcf(lag.max=2*365) +
  ggtitle("")

grid_plot <- (plot1 | plot3 | plot5) / (plot2 | plot4 | plot6)
# grid_plot <- (
#   wrap_elements(grid::textGrob("Store 2 Item 44", gp = grid::gpar(fontsize = 13))) /
#     (plot1 | plot3 | plot5)
# ) /
#   (
#     wrap_elements(grid::textGrob("Store 9 Item 29", gp = grid::gpar(fontsize = 13))) /
#       (plot2 | plot4 | plot6)
#   )
grid_plot
ggsave("grid_plot.png", grid_plot, width = 8, height = 6)
