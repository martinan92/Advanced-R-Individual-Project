source('code/f_partition.R')

split_and_train <- function(df, seed = 123456){
  split_fe_train_df <- f_partition(df, test_proportion = 0.2, seed = seed)
  fe_rf <- ranger(formula = as.formula(price~.), data=split_fe_train_df$train[,!c('id')])
  print(fe_rf)
  
  fe<-predict(fe_rf, data = split_fe_train_df$test, type='response')$predictions
  df_pipeline_pred<-cbind(df_pipeline_pred, fe)
  
  return(list(split_fe_train_df, fe, df_pipeline_pred, fe_rf))
}