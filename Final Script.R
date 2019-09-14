# Load data, explore and visualize
library(tidyverse)

load("~/LearnR/YouTubeCA/dataCA.RData")

glimpse(dataCA)
summary(dataCA)

pairs(dataCA)

correlation_matrix <- data.frame(relation = c("views_N_likes", "views_N_dislikes", "views_N_commentcount", "likes_N_dislikes", "likes_N_commentcount", "dislikes_N_commentcount"), correlation = c(cor(dataCA$views, dataCA$likes), cor(dataCA$views, dataCA$dislikes), cor(dataCA$views, dataCA$comment_count), cor(dataCA$likes, dataCA$dislikes), cor(dataCA$likes, dataCA$comment_count), cor(dataCA$dislikes, dataCA$comment_count))) %>% arrange(desc(correlation))
correlation_matrix

correlation_matrix %>% mutate(relation = fct_reorder(relation, correlation)) %>%
  ggplot(aes(relation, correlation)) + 
  geom_bar(stat = "identity") + coord_flip()

# Modelling and Evaluation

library(caret)
set.seed(2)
test_index <- createDataPartition(y = dataCA$category_id, times = 1, p = 0.2, list = FALSE)
test_set <- dataCA[test_index,]
train_set <- dataCA[-test_index,]

model_1 <- mean(train_set$likes)
y_hat_1 <- mean(train_set$likes)
model_2 <- lm(likes ~ views, data = train_set)
y_hat_2 <- predict(model_2, test_set)
model_3 <- lm(likes ~ views + comment_count, data = train_set)
y_hat_3 <- predict(model_3, test_set)
model_4 <- lm(likes ~ views + comment_count + dislikes, data = train_set)
y_hat_4 <- predict(model_4, test_set)
model_5 <- lm(likes ~ views + comment_count + dislikes + factor(category_id), data = train_set)
y_hat_5 <- predict(model_5, test_set)

RMSE <- function(true_likes, predicted_likes){
  sqrt(mean((true_likes - predicted_likes)^2))
}

RMSE_1 <- RMSE(test_set$likes, y_hat_1)
RMSE_2 <- RMSE(test_set$likes, y_hat_2)
RMSE_3 <- RMSE(test_set$likes, y_hat_3)
RMSE_4 <- RMSE(test_set$likes, y_hat_4)
RMSE_5 <- RMSE(test_set$likes, y_hat_5)

rmse_results <- tibble(Method = c("Method 1",
                                  "Method 2",
                                  "Method 3",
                                  "Method 4", 
                                  "Method 5"),
                       RMSE = c(RMSE_1,
                                RMSE_2,
                                RMSE_3,
                                RMSE_4,
                                RMSE_5),
                       Predictor = c("Naive Average",
                                     "views",
                                     "views_N_comments",
                                     "views_N_comments_N_dislikes",
                                     "views_N_comments_N_dislikes_N_categoryID"))
rmse_results


# Validation

set.seed(13)
val_index <- createDataPartition(y = dataCA$category_id, times = 1, p = 0.1, list = FALSE)
val_set <- dataCA[test_index,]
train_set_val <- dataCA[-test_index,]

model_val <- lm(likes ~ views + comment_count + dislikes + factor(category_id), data = train_set)
y_hat_val <- predict(model_val, test_set)
RMSE_val <- RMSE(val_set$likes, y_hat_val)
RMSE_val