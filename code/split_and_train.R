source('code/f_partition.R')

clip <- function(x, lower_bound = .05, upper_bound = .95){
  quantiles <- quantile(x, c(lower_bound, upper_bound))
  x[x < quantiles[1]] <- quantiles[1]
  x[x > quantiles[2]] <- quantiles[2]
  return(x)
}

split_and_train <- function(df, cross_validation = F, cv=3, seed = 123456){
  split_fe_train_df <- f_partition(df, test_proportion = 0.2, seed = seed)
  
  if (cross_validation){
    fit_control <- trainControl(method = "cv", number = cv, search = "random")
    fe_rf <- train(as.factor(price) ~ ., data = split_fe_train_df$train[,!c('id')], 
                   method = "ranger", 
                   trControl = fit_control)
  }else{
    fe_rf <- ranger(formula = as.formula(price~.), data=split_fe_train_df$train[,!c('id')], importance = 'impurity')
  }
  
  print(fe_rf)
  
  fe<-predict(fe_rf, data = split_fe_train_df$test, type='response')$predictions
  df_pipeline_pred<-cbind(df_pipeline_pred, fe)
  
  return(list(split_fe_train_df, fe, df_pipeline_pred, fe_rf))
}