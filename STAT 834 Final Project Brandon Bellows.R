library(timetk)
library(tidyverse)
library(tvReg)
library(dplyr)
library(car)
library(lubridate)
library(broom)
library(knitr)
library(gt)
library(ggplot2)
library(segmented)
library(strucchange)
library(emmeans)
library(ggpubr)
library(lmerTest)

#### Load Environment ####

# Since data was randomly generated, we first load an environment that contains the specific generated data sets used in the presentation and report

setwd("C:/Users/gamem/Documents/R Files/STAT 834/Final Project")
load('myEnvironment.RData')

#### Model ####

# Fit the model to be used for ANOVA analysis

# Fit a model where all terms are fixed effects
model <- lm(MSPE ~ Block + Transform * Model, data = results_df)
anova(model)

# Fit a model where the blocking term is random effect
model_random_effects <- lmer(MSPE ~ (1 | Block) + Transform * Model, data = results_df)
anova(model_random_effects)

# Since no difference is observed, the first model is used for subsequent analysis

#### Diagnostics ####

par(mfrow=c(2,2))

#Residuals vs. Run Order
plot(1:length(model$resid), model$resid, main = "Residuals vs. Run Order (Full Data Set)", xlab = "Observation Index", ylab = "Residual")

#QQ Norm plot
qqnorm(model$resid, main = "Normal QQ Plot (Full Data Set)")
qqline(model$resid)

## Create a new data set with outliers Removed (use 1.5 IQR away from the median as our cuttoff)
results_df_outliers_removed <- results_df %>% filter(MSPE<(median(results_df$MSPE) + (1.5*IQR(results_df$MSPE))) & MSPE>(median(results_df$MSPE) - (1.5*IQR(results_df$MSPE))))
model2 <- lm(MSPE ~ Block + Transform * Model, data = results_df_outliers_removed)

#Residuals vs. Run Order
plot(1:length(model2$resid), model2$resid, main = "Residuals vs. Run Order (Outliers Removed)", xlab = "Observation Index", ylab = "Residual")

#QQ Norm plot
qqnorm(model2$resid, main = "Normal QQ Plot (Outliers Removed)")
qqline(model2$resid)


#### ANOVA ####

## Full data
anova(model)

## Outliers removed
anova(model2)

#### Plots ####

ResultsBoxplot <- ggplot(data = results_df, aes(x=Model, y=MSPE, fill=Transform)) + 
  geom_boxplot() +
  ylim(0, (median(results_df$MSPE) + (0.75*IQR(results_df$MSPE)))) + 
  ggtitle("MSPE by Model and Transformation") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text = element_text(size = 20))  

ResultsBoxplot

#### Pairwise Comparisons ####

par(mfrow=c(2,1))

## Full data

ResultsModel.emm <- emmeans(model, specs = "Transform")
confintdata <- confint(pairs(ResultsModel.emm, adjust = "bonferonni"))

ConfIntPlotData <- data.frame(
  Transformations = c("Cumulative - None","Cumulative - RollingAverage","None - RollingAverage "),
  Difference = confintdata$estimate,
  lower = confintdata$lower.CL,
  upper = confintdata$upper.CL
)

PairwiseComparisonsFullDataChart <- ggplot(ConfIntPlotData, aes(Transformations, Difference)) +  
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  labs(title="Confidence Intervals for Pairwise Comparisons Between Transformations (Full Data)") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text = element_text(size = 10))  

## Outliers removed

ResultsModel2.emm <- emmeans(model2, specs = "Transform")
confint(pairs(ResultsModel2.emm, adjust = "bonferonni"))
confintdata2 <- confint(pairs(ResultsModel2.emm, adjust = "bonferonni"))

ConfIntPlotData2 <- data.frame(
  Transformations = c("Cumulative - None","Cumulative - RollingAverage","None - RollingAverage "),
  Difference = confintdata2$estimate,
  lower = confintdata2$lower.CL,
  upper = confintdata2$upper.CL
)

PairwiseComparisonsFullDataChart2 <- ggplot(ConfIntPlotData2, aes(Transformations, Difference)) +  
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  labs(title="Confidence Intervals for Pairwise Comparisons Between Transformations (Outliers Removed)") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text = element_text(size = 10))  

ggarrange(PairwiseComparisonsFullDataChart, PairwiseComparisonsFullDataChart2,
          ncol = 1, nrow = 2)


## Model comparison

# Filter down to only the "none" transformation based on the results in the previous analysis

results_df_outliers_removed_no_transform <- results_df %>% filter(Transform %in% "None")
model3 <- lm(MSPE ~ Block + Model, data = results_df_outliers_removed_no_transform)

results_df_outliers_removed_no_transform <- results_df_outliers_removed %>% filter(Transform %in% "None")
model4 <- lm(MSPE ~ Block + Model, data = results_df_outliers_removed_no_transform)

ResultsModel3.emm <- emmeans(model3, specs = "Model")
confint(pairs(ResultsModel3.emm, adjust = "bonferonni"))
confintdata3 <- confint(pairs(ResultsModel3.emm, adjust = "bonferonni"))

ConfIntPlotData3 <- data.frame(
  Models = c("Linear Constant - Local Linear"),
  Difference = confintdata3$estimate,
  lower = confintdata3$lower.CL,
  upper = confintdata3$upper.CL
)

PairwiseComparisonsFullDataChart3 <- ggplot(ConfIntPlotData3, aes(Models, Difference)) +  
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  labs(title="Full Data") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text = element_text(size = 15))  


ResultsModel4.emm <- emmeans(model4, specs = "Model")
#confint(pairs(ResultsModel4.emm, adjust = "bonferonni"))
confintdata4 <- confint(pairs(ResultsModel4.emm, adjust = "bonferonni"))

ConfIntPlotData4 <- data.frame(
  Models = c("Linear Constant - Local Linear"),
  Difference = confintdata4$estimate,
  lower = confintdata4$lower.CL,
  upper = confintdata4$upper.CL
)

PairwiseComparisonsFullDataChart4 <- ggplot(ConfIntPlotData4, aes(Models, Difference)) +  
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  labs(title="Outliers Removed") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text = element_text(size = 15))  

ggarrange(PairwiseComparisonsFullDataChart3, PairwiseComparisonsFullDataChart4, 
          labels = NA,
          ncol = 2, nrow = 1)


#### Functions ####

# Below are the functions used to generate the simulated data sets and fit models

#Function to generate simulated data
DataGenerator <- function(ndays, ReportingErrors = FALSE) {
  
  #Function to generate simulated predictor variable data (i.e. daily observed COVID cases)
  GeneratePredictorVariable <- function(ndays, ReportingErrors = FALSE) {
    
    #Polynomial Component
    a = sample(1:4000,1)
    b = runif(1,-5,5)
    c = runif(1,-0.05,0.05)
    d = runif(1,-0.0005,0.0005)
    
    Poly <- unlist(lapply(1:ndays, function(x) {a + (b*x) + (c*x^2)} ))
    
    
    #Sin Function Component
    seq <- 1:ndays
    seq2 <- (seq / (runif(1,1,5) * 5)) + runif(1,1,ndays) 
    
    Poly <- Poly + (runif(1,100,1000) * sin(seq2))
    
    #Error Component
    e = rnorm(ndays, 0, 250)
    
    #Add together the components to create the simulated predictor variable values
    PolyWithError <- Poly + e 
    
    #Add reporting mistakes
    if (ReportingErrors) {
      n <- sample(5:15,1)
      mistake_ind <- sample(1:(ndays-1),n)
      
      for (i in 1:n) {
        PolyWithError[mistake_ind[i]+1] <- PolyWithError[mistake_ind[i]] + PolyWithError[mistake_ind[i]+1]
        PolyWithError[mistake_ind[i]] <- 0
      }
    }
    
    PolyWithError <- round(PolyWithError,0)
    
    return(PolyWithError)
  }
  
  Cases <- GeneratePredictorVariable(120, ReportingErrors = TRUE)
  
  while (sum(Cases < 0))  {
    Cases <- GeneratePredictorVariable(120)
  }
  
  GenerateResponseVariable <- function(ndays, Cases) {
    
    MortalityRate <- runif(1,0.01,0.06)
    b2 = runif(1,-0.0001,0.0001)
    c2 = runif(1,-0.000005,0.000005)
    
    DeathRatePoly <- unlist(lapply(1:ndays, function(x) {MortalityRate + (b2*x) + (c2*x^2)} ))
    
    #Add random error
    Deaths <- (Cases * DeathRatePoly) + rnorm(ndays,0,4)
    
    Deaths[which(Cases == 0)] <- 0
    
    Deaths <- round(Deaths,0)
    
    return(Deaths)
  }
  
  Deaths <- GenerateResponseVariable(120, Cases)
  
  while (sum(Deaths < 0))  {
    Deaths <- GenerateResponseVariable(120, Cases)
  }
  
  return(data.frame(Cases_New = Cases, Deaths_New = Deaths))
  
}

#Function to calculate a rolling average of counts in the data
RollingAverage <- function(dat, period) {
  
  result <- rep(NA, length(dat))
  
  for (n in 1:length(dat)) {
    
    if (n < period) {
      result[n] <- mean(dat[1:n])
    } else if (n >= period) {
      result[n] <- mean(dat[(n-period+1):n])
    }
    
  }
  
  return(result)
}

#Function to un-transform the rolling average transformation (needed to find predicted value that can be compared to observed values)
InverseRollingAverage <- function(dat, period) {
  
  result <- rep(NA, length(dat))
  
  for (n in 1:length(dat)) {
    
    if (n == 1) {
      result[n] <- dat[n]
        
    } else if (n < period) {
      result[n] <- (n*dat[n]) - sum(result[1:(n-1)])
    
    } else {
      result[n] <- (period*dat[n]) - sum(result[(n-period+1):(n-1)])
    }
    
  }
  
  return(result)
  
}

# Function to fit a local polynomial model to the data set
# The transformation argument should be set to "None", "Cumulative" or "Average"
localpoly2 <- function(df, alpha = 0.05, method = "ll", min_lead = 7, max_lead = 30, transformation = "Cumulative") {
  
  names(df) <- c("Date", "y", "cumulative_y", "x", "cumulative_x")
  
  if (transformation == "None" | transformation == "Average") {
    
    df$cumulative_y_reset <- df$cumulative_y
    df$cumulative_x_reset <- df$cumulative_x
    
  } else if (transformation == "Cumulative") {
    
    # reset the cumulative y/cases using the start_date
    df$cumulative_y_reset <- df$cumulative_y - df$cumulative_y[1] 
    df$cumulative_x_reset <- df$cumulative_x - df$cumulative_x[1]
    
  }

  
  # Return the optimal choice of lead for Tvreg
  tv <- function(df){
    
    #Create a variable to store Mean Squared Prediction Error for each possible lead value
    MSPE_List <- c()
    
    #Loop through all possible leads and record the MSPE
    for (n_ahead_tv in min_lead:max_lead) {
      
      df_tv <- df %>%
        tk_augment_lags(c(cumulative_y_reset, y), .lags = -n_ahead_tv, .names = c("y_best_lead_tv", "daily_y_lead"))
      
      names(df_tv) <- names(df_tv) %>% str_replace_all("lag-|lag", "lead")
      
      D1 <- max(df_tv$Date) -  2 * max_lead
      
      df_tv_insample <- df_tv %>% filter (Date <= D1)
      
      tvLM_insample <- tvLM(y_best_lead_tv ~ cumulative_x_reset, 
                            est = method, 
                            data = df_tv_insample,
                            #tkernel = "Gaussian",
                            singular.ok = FALSE)
      
      newdf1 <- df_tv %>% filter (Date > D1 & Date <= D1 + n_ahead_tv) %>% 
        dplyr::select (c("cumulative_x_reset", "y_best_lead_tv", "daily_y_lead"))
      
      #get predictions for cases outside the insample
      tvLM_df_pred1 <- forecast(tvLM_insample, newdata = as.matrix(newdf1[,1]), n.ahead = n_ahead_tv)
      
      #residual <- c(tvLM_insample$residuals, tvLM_df_pred1 - newdf1$y_best_lead_tv)
      #residual <- tvLM_df_pred1 - newdf1$y_best_lead_tv
      
      
      
      if (transformation == "None") {
        daily_pred <- tvLM_df_pred1[-1]
        
      } else if (transformation == "Cumulative") {
        daily_pred <- tail(tvLM_df_pred1, -1) - head(tvLM_df_pred1, -1)
        
      } else if (transformation == "Average") {
        ConvertedPredictions <- InverseRollingAverage(c(tvLM_insample$fitted, tvLM_df_pred1),7)
        daily_pred <- tail(ConvertedPredictions, n_ahead_tv-1)
        
      }
      
      daily_residual <- daily_pred - newdf1$daily_y_lead[-1]
      MSPE <- mean((daily_residual)^2)
      
      MSPE_List <- c(MSPE_List, MSPE)
    }
    
    #Return the lead with the smallest MSPE
    ans <- which.min(MSPE_List) + (min_lead - 1)
    print (paste("best lead for tv:", ans))
    return(ans)
  }
  
  n_ahead_tv <- tv(df)
  
  
  #predictions based on TvReg
  df_tv <- df %>%
    tk_augment_lags(cumulative_y_reset, .lags = -n_ahead_tv, .names = "y_best_lead_tv")
  names(df_tv) <- names(df_tv) %>% str_replace_all("lag-|lag", "lead")
  
  D1 <- max(df_tv$Date) - 2*n_ahead_tv 
  D2 <- max(df_tv$Date) - n_ahead_tv + 1
  
  df_tv_insample <- df_tv %>% filter (Date <= D1 )
  
  tvLM_insample <- tvLM(y_best_lead_tv ~ cumulative_x_reset, 
                        est = method, 
                        data = df_tv_insample,
                        #tkernel = "Epa",
                        singular.ok = FALSE)
  
  newdf1 <- df_tv %>% filter (Date > D1 & Date < D2) %>% dplyr::select (c("cumulative_x_reset", "y_best_lead_tv"))
  
  tvLM_df_pred1 <- forecast(tvLM_insample, newdata = as.matrix(newdf1[,1]), n.ahead = n_ahead_tv)
  
  
  pred_tv <- c(tvLM_insample$fitted, tvLM_df_pred1)
  
  n <- dim(df_tv)[1] # same as dim(df_tv)[1]
  opt_tv <- data.frame(pred_tv, head(df_tv$y_best_lead_tv, n - n_ahead_tv))
  names(opt_tv) <- c("predicted_tv", "observed_tv")
  n_tv <- length(pred_tv)
  
  if (transformation == "None") {
    opt_tv$pred_daily_tv <- opt_tv$predicted_tv
    
  } else if (transformation == "Cumulative") {
    opt_tv$pred_daily_tv <- c(NA, pred_tv[2:n_tv] - pred_tv[1:(n_tv - 1)])
    
  } else if (transformation == "Average") {
    ConvertedPredictions <- InverseRollingAverage(pred_tv,7)
    opt_tv$pred_daily_tv <- ConvertedPredictions
    
  }
  
  opt_tv$date <- head(df_tv$Date, n - n_ahead_tv) + n_ahead_tv ### date being moved forward
  
  # Pointwise confidence bands
  point_conf <- function (data, n_ahead, B = 200, alpha = 0.05) { # df after the best lead being selected
    #B = 1000 ## number of bootstrap draws
    data_insample <- head (data, dim(data)[1] - n_ahead)
    nobs <- dim(data_insample)[1]
    object <- tvLM(y_best_lead_tv ~ cumulative_x_reset, 
                   data = data_insample,
                   est = method) 
    
    residuals_raw <- object$residuals - mean(object$residuals)
    residuals_b <- matrix(sample (residuals_raw * rnorm(B * nobs), size = B * nobs, replace = TRUE),
                          nrow = B, ncol = nobs)
    y_b <- matrix(object$fitted, nrow = B, ncol = nobs, byrow = TRUE) + residuals_b ## synthetic y
    
    ## out-of-sample prediction 1 with death counts observable
    prediction1 <- matrix(NA, nrow = B, ncol = n_ahead)
    #prediction_daily <- matrix(NA, nrow = B, ncol = n_ahead - 1)
    totobs <- nobs + n_ahead
    
    newdf <- data %>% tail(n_ahead) %>% dplyr::select (c("cumulative_x_reset", "y_best_lead_tv"))
    newdata <- cbind(rep(1, n_ahead), as.matrix(newdf[, 1]))
    pred_raw <- forecast (object, newdata = newdata, n.ahead = n_ahead)
    
    for (k in 1: B) {
      tmp <- tvLM(y_b[k, ] ~ cumulative_x_reset, 
                  data = data_insample,# z = (1:nobs)/nobs,
                  est = "ll") 
      #prediction1[k, ] <- predict(tmp, newdata = newdata, newz = seq(nobs + 1, nobs + n_ahead)/(nobs + n_ahead))
      prediction1[k, ] <- forecast(tmp, newdata = newdata, n.ahead = n_ahead)
      #prediction_daily[k, ] <- tail(prediction1[k, ], -1) -  head(prediction1[k, ], -1)
    }# end of k loop  
    
    
    sd.est <- apply(prediction1, 2, sd)
    Q <- (prediction1 - matrix(pred_raw, nrow=B, ncol=length(pred_raw), byrow=TRUE))/
      matrix(sd.est, nrow=B, ncol=length(sd.est), byrow=TRUE)
    calpha <- apply(Q, 2, function(x){quantile(x, 1-alpha/2, na.rm = TRUE)})
    
    # output
    ans <- list (lower = pred_raw - sd.est * calpha, upper = pred_raw + sd.est * calpha)
  }
  
  # Simultaneous confidence bands
  joint_conf <- function (data, n_ahead, B = 200, alpha = 0.05) { # df after the best lead being selected
    #B = 1000 ## number of bootstrap draws
    data_insample <- head (data, dim(data)[1] - n_ahead)
    nobs <- dim(data_insample)[1]
    object <- tvLM(y_best_lead_tv ~ cumulative_x_reset, 
                   data = data_insample,
                   est = method) 
    
    residuals_raw <- object$residuals - mean(object$residuals)
    residuals_b <- matrix(sample (residuals_raw * rnorm(2 * B * nobs), size = 2 * B * nobs, replace = TRUE),
                          nrow = 2 * B, ncol = nobs)
    y_b <- matrix(object$fitted, nrow = 2 * B, ncol = nobs, byrow = TRUE) + residuals_b ## synthetic y
    
    ## out-of-sample prediction 1 with death counts observable
    prediction1 <- matrix(NA, nrow = 2 * B, ncol = n_ahead)
    totobs <- nobs + n_ahead
    
    newdf <- data %>% tail(n_ahead) %>% dplyr::select (c("cumulative_x_reset", "y_best_lead_tv"))
    newdata <- cbind(rep(1, n_ahead), as.matrix(newdf[, 1]))
    pred_raw <- forecast (object, newdata = newdata, n.ahead = n_ahead)
    
    for (k in 1: (2 * B)) {
      tmp <- tvLM(y_b[k, ] ~ cumulative_x_reset, 
                  data = data_insample, #z = (1:nobs)/nobs,
                  est = "ll") 
      #prediction1[k, ] <- predict(tmp, newdata = newdata, newz = seq(nobs + 1, nobs + n_ahead)/(nobs + n_ahead))
      prediction1[k, ] <- forecast(tmp, newdata = newdata, n.ahead = n_ahead)
    }# end of k loop  
    
    sd.est <- apply(prediction1[1:B,], 2, sd)
    
    # estimate common c_alpha 
    prediction2 <- prediction1[-(1:B), ]
    Q <- abs(prediction2 - 
               matrix(pred_raw, nrow=B, ncol=length(pred_raw), byrow=TRUE))/
      matrix(sd.est, nrow=B, ncol=length(sd.est), byrow=TRUE)
    Qstar <- apply(Q, 2, max)
    calpha <- quantile(Qstar, 1 - alpha/2, na.rm = TRUE)  
    # output
    ans <- list (lower = pred_raw - sd.est * calpha, upper = pred_raw + sd.est * calpha)
  }
  
  #Add cumulative prediction bands for TVReg
  # tv_conf_point <- point_conf(df_tv %>% filter (Date < D2), n_ahead_tv)
  # opt_tv$plower <- c(rep(NA, length(tvLM_insample$fitted)), tv_conf_point$lower)
  # opt_tv$pupper <-  c(rep(NA, length(tvLM_insample$fitted)), tv_conf_point$upper)
  # 
  # tv_conf_joint <- joint_conf(df_tv %>% filter (Date < D2), n_ahead_tv)
  # opt_tv$jlower <- c(rep(NA, length(tvLM_insample$fitted)), tv_conf_joint$lower)
  # opt_tv$jupper <-  c(rep(NA, length(tvLM_insample$fitted)), tv_conf_joint$upper)
  
  
  # if (transformation == "None" | transformation == "Average") {
  #   opt_tv$observed_daily <- opt_tv$observed_tv
  #   
  # } else if (transformation == "Cumulative") {
  #   opt_tv$observed_daily <- df_tv$y[df_tv$Date %in% opt_tv$date]
  #   
  # } else if (transformation == "Average") {
  #   opt_tv$observed_daily <- tail(df$y,length(df$y)-n_ahead)
  #   
  # }
  
  opt_tv$observed_daily <- df_tv$y[df_tv$Date %in% opt_tv$date]
  
  opt_tv_predictiondays <- tail(opt_tv, n_ahead)
  
  mspe <- mean((opt_tv_predictiondays$observed_daily - opt_tv_predictiondays$pred_daily_tv)^2, na.rm = TRUE)
  
  return(list(mspe, opt_tv_predictiondays$observed_daily, opt_tv_predictiondays$pred_daily_tv))
}

#### Data Generation ####

n_runs <- 100

# Create an empty data set with the variables we want
results_df <- data.frame(Block = rep(1:n_runs, each = 6),
                         Transform = rep(rep(c("None","Cumulative","RollingAverage"),each = 2),n_runs),
                         Model = rep(c("LocalPolyLC","LocalPolyLL"), n_runs*3),
                         MSPE = rep(NA,n_runs*6),  #runif(n_runs*9)
                         Observed = rep(NA,n_runs*6),
                         Predicted = rep(NA,n_runs*6)
)

# Specify the data types of several variables
results_df$Block <- as.factor(results_df$Block)
results_df$Transform <- as.factor(results_df$Transform)
results_df$Model <- as.factor(results_df$Model)

# Generate data sets, fit models and record MSPE for each run
for (n in 1:n_runs) {
  data <- DataGenerator(120, ReportingErrors = TRUE)
  
  Date <- rep("2023-01-01", 120)
  Date <- as.Date(Date, format="%Y-%m-%d")
  
  data <- data %>% mutate(Cases_Cumulative = cumsum(data$Cases_New),
                          Deaths_Cumulative = cumsum(data$Deaths_New),
                          Cases_RollingAverage = RollingAverage(data$Cases_New, 7),
                          Deaths_RollingAverage = RollingAverage(data$Deaths_New, 7),
                          Date = Date + 0:119
  )
  
  #No Transformation
  data_transformed <- data %>% dplyr::select(Date, Deaths_New, Cases_New)
  data_transformed$Deaths_Cumulative <- data$Deaths_New
  data_transformed$Cases_Cumulative <- data$Cases_New
  data_transformed <- data_transformed %>% dplyr::select(Date, Deaths_New, Deaths_Cumulative, Cases_New, Cases_Cumulative)
  
  LLResults <- localpoly2(data_transformed, method = "ll", transformation = "None")
  LCResults <- localpoly2(data_transformed, method = "lc", transformation = "None")
  
  results_df$MSPE[1+((n-1)*6)] <- LLResults[1]
  results_df$MSPE[2+((n-1)*6)] <- LCResults[1]
  
  results_df$Observed[1+((n-1)*6)] <- LLResults[2]
  results_df$Observed[2+((n-1)*6)] <- LCResults[2]
  
  results_df$Predicted[1+((n-1)*6)] <- LLResults[3]
  results_df$Predicted[2+((n-1)*6)] <- LCResults[3]
  
  #Cumulative Transformation
  data_transformed <- data %>% dplyr::select(Date, Deaths_New, Deaths_Cumulative, Cases_New, Cases_Cumulative)
  
  LLResults <- localpoly2(data_transformed, method = "ll", transformation = "Cumulative")
  LCResults <- localpoly2(data_transformed, method = "lc", transformation = "Cumulative")
  
  
  results_df$MSPE[3+((n-1)*6)] <- LLResults[1]
  results_df$MSPE[4+((n-1)*6)] <- LCResults[1]
  
  results_df$Observed[3+((n-1)*6)] <- LLResults[2]
  results_df$Observed[4+((n-1)*6)] <- LCResults[2]
  
  results_df$Predicted[3+((n-1)*6)] <- LLResults[3]
  results_df$Predicted[4+((n-1)*6)] <- LCResults[3]
  
  #Rolling Average Transformation
  data_transformed <- data %>% dplyr::select(Date, Deaths_New, Cases_New)
  data_transformed$Deaths_Cumulative <- data$Deaths_RollingAverage
  data_transformed$Cases_Cumulative <- data$Cases_RollingAverage
  data_transformed <- data_transformed %>% dplyr::select(Date, Deaths_New, Deaths_Cumulative, Cases_New, Cases_Cumulative)
  
  
  LLResults <- localpoly2(data_transformed, method = "ll", transformation = "Average")
  LCResults <- localpoly2(data_transformed, method = "lc", transformation = "Average")
  
  results_df$MSPE[5+((n-1)*6)] <- LLResults[1]
  results_df$MSPE[6+((n-1)*6)] <- LCResults[1]
  
  results_df$Observed[5+((n-1)*6)] <- LLResults[2]
  results_df$Observed[6+((n-1)*6)] <- LCResults[2]
  
  results_df$Predicted[5+((n-1)*6)] <- LLResults[3]
  results_df$Predicted[6+((n-1)*6)] <- LCResults[3]
  
  results_df$MSPE <- as.double(results_df$MSPE)
  
}



