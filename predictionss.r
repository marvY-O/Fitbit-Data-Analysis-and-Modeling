steps_distance <- daily_activity %>% rename(steps = total_steps, distance = total_distance) %>% select(distance, steps)

sample_test <- function(dataframe, percent){
  sample_size = floor((percent/100)*nrow(dataframe))
  set.seed(123)
  train_index <- sample(seq_len(nrow(dataframe)), size = sample_size)
  
  train <- dataframe[train_index, ]
  test <- dataframe[-train_index, ]
  
  return(list(train, test))
 
}

train_test_daily <- sample_test(steps_distance, 80)

train_data <- as.data.frame(train_test_daily[1])
test_data <- as.data.frame(train_test_daily[2])

model <- lm(steps~distance, train_data) 
summary(model)

predicted_data <- test_data %>% mutate(predicted_steps = predict(model, test_data)) 
ggplot(predicted_data, aes(distance, steps)) + geom_point(shape = 1) + geom_smooth(method = "lm", color = "Orange")

head(predicted_data)

summary(model)$adj.r.squared * 100



