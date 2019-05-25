univariate_outlier <- function(df, input_var){
  outlier_values <- boxplot.stats(df[[input_var]])$out
  boxplot(df[[input_var]], main= var, boxwex=0.5)
  print(paste("Outliers: ", paste(length(unique(outlier_values)))))
}

bivariate_outlier <- function(df, input_var){
  layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
  boxplot(df[[input_var]] ~ df$month, main=paste0("Monthly Analysis of ", var))
  boxplot(df[[input_var]] ~ df$day_of_week, main=paste0("Day of the Week Analysis of ", var))  
  boxplot(df[[input_var]] ~ df$day, main=paste0("Day of the Month Analysis of ", var)) 
}

multiple_hist <- function(df, subset){
  df[,..subset] %>%
    # Reshape
    gather(key = indicator, value = val) %>%
    # Basic chart
    ggplot(aes(x = val)) +
    geom_bar(colour = "darkgreen", fill = "gray") +
    facet_wrap(~indicator, nrow = 2) +
    ## Theme and looks
    theme_economist() +
    ggtitle("Histograms") +
    theme(strip.background = element_rect(fill = "gray80", colour = "black",
                                          size = 0.5, linetype = "solid"),
          strip.text = element_text(face = "bold"))
}

single_hist <- function(var, var_name){
  qplot(var, geom="histogram") +
    geom_bar(colour = "darkgreen", fill = "gray") +
    ## Theme and looks
    theme_economist() +
    ggtitle(paste0("Histogram of ", var_name)) +
    theme(strip.background = element_rect(fill = "gray80", colour = "black",
                                          size = 0.5, linetype = "solid"),
          strip.text = element_text(face = "bold"))
}


gg_scatter <- function(df, var){
  p <- ggplot(data=df, aes_string(x = var, y = 'price')) +
    geom_point() +
    theme_economist()
  
  return(p)
}

metrics_plot <- function(df, cols, verbose = F){
  result<-data.table(method=cols,
                     rmse=sapply(df[,!c('price','id')],function(x) return(rmse(real=df$price, predicted=x))),
                     mae=sapply(df[,!c('price','id')],function(x) return(mae(real=df$price, predicted=x))),
                     mape=sapply(df[,!c('price','id')],function(x) return(mape(real=df$price, predicted=x))),
                     rsq=sapply(df[,!c('price','id')],function(x) return(custom_rsq(real=df$price, predicted=x))))
  
  # plotting results metrics
  mape_plot<-ggplot(result, aes(x=method, y=mape))+geom_bar(stat='identity')
  rmse_plot<-ggplot(result, aes(x=method, y=rmse))+geom_bar(stat='identity')
  mae_plot<-ggplot(result, aes(x=method, y=mae))+geom_bar(stat='identity')
  rsq_plot<-ggplot(result, aes(x=method, y=rsq))+geom_bar(stat='identity')
  
  grid.arrange(mape_plot, rmse_plot, mae_plot, rsq_plot, ncol=2)
  
  if(verbose){
    result[which.min(result$mape)]
  }
}