univariate_outlier <- function(df, input_var){
  outlier_values <- boxplot.stats(df[[input_var]])$out
  boxplot(df[[input_var]], main= var, boxwex=0.5)
  print(paste("Outliers: ", paste(length(unique(outlier_values)))))
}

bivariate_outlier <- function(df, input_var){
  boxplot(df[[input_var]] ~ df$month, main=paste0("Monthly Analysis of ", var))
  boxplot(df[[input_var]] ~ df$day_of_week, main=paste0("Daily Analysis of ", var))  
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

single_hist <- function(var){
  qplot(var, geom="histogram") +
    geom_bar(colour = "darkgreen", fill = "gray") +
    ## Theme and looks
    theme_economist() +
    ggtitle("Histograms") +
    theme(strip.background = element_rect(fill = "gray80", colour = "black",
                                          size = 0.5, linetype = "solid"),
          strip.text = element_text(face = "bold"))
}
