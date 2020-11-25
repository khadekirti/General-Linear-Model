library(heuristica)
library(aod)
library(ggplot2) 
library(caret) 
library(tidyverse)
library(broom)
library(car)
library(pscl)

################################################################################### 
################################################################################### 
###################################################################################  
# 4 i  

setwd("~/Desktop/Statistics Machine Learning/Assignment - 1/OnlineNewsPopularity") 
df <- read.csv('OnlineNewsPopularity.csv') 

# Outlier Treatment 
df$kw_min_avg[df$kw_min_avg <0] = 0 
df$kw_min_min[ df$kw_min_min <0] = 0 
df$n_non_stop_words[ (df$n_non_stop_words > 1) ]  = 1 
df$n_unique_tokens[ (df$n_unique_tokens > 1) ]  = 1
df$kw_avg_min[df$kw_avg_min <0] = 0
 

# Applying transforms 
df$kw_max_min       <-    log(df$kw_max_min + 1)
df$kw_avg_min       <-    log(df$kw_avg_min  + 2)
df$kw_min_max       <-    log(df$kw_min_max + 1) 
df$kw_avg_avg       <-    log(df$kw_avg_avg + 1) 
df$global_sentiment_polarity <- df$global_sentiment_polarity ** -2
df$min_positive_polarity  <- df$min_positive_polarity  ** 0.06  
df$n_tokens_content <-    log( df$n_tokens_content + 1) 
df$n_non_stop_words   <- df$n_non_stop_words   ** -0.10  
df$num_imgs         <-    log( df$num_imgs + 1)
df$num_hrefs        <-     log(df$num_hrefs+1)
df$shares <- ifelse(df$shares >= 1000  ,1,0) 
 
# Outlier Treatment due to Transformations
df$global_sentiment_polarity[is.infinite(df$global_sentiment_polarity)] = quantile(df$global_sentiment_polarity,probs=c(.95))[1]
df$n_non_stop_words[is.infinite(df$n_non_stop_words)] = quantile(df$n_non_stop_words,probs=c(.95))[1]


# removingthe variables that will be unknown before sharing, 
# and "url" as its a character 
# and redundent categortrical variables like "is_weekend", "weekday_is_sunday"
# and lastly, "kw_min_min" due to the max records being negative and timedelta as its irrelevent 
drops <- c("url", "is_weekend", "weekday_is_sunday", 
           "self_reference_min_shares", "self_reference_max_shares" , 
           "self_reference_avg_shares", "timedelta", "kw_min_min")

df <- df[ , !(names(df) %in% drops)] 

fit <- glm(shares ~., data = df, 
             family = binomial) 

# Summary 
summary(fit) 

# Use your model to make predictions, in this example newdata = training set, but replace with your test set    
prediction <- predict(fit, data = df, type = "response")

# use caret and compute a confusion matrix
confusionMatrix(data = factor(as.numeric(prediction>0.5)),reference =  factor(df$shares)) 

# Multi colinriairty 
df <- df[ , !(names(df) %in% c("LDA_02"))] 
df <- df[ , !(names(df) %in% c("rate_positive_words"))] 
df <- df[ , !(names(df) %in% c("kw_avg_min"))] 
df <- df[ , !(names(df) %in% c("n_tokens_content"))] 
df <- df[ , !(names(df) %in% c("n_unique_tokens"))] 
df <- df[ , !(names(df) %in% c("non_stop_words"))] 
df <- df[ , !(names(df) %in% c("n_non_stop_words"))] 

fit <- glm(shares ~., data = df, 
           family = binomial) 
 
vif_multicolinairity <- data.frame(vif(fit))
vif_multicolinairity$variable <- rownames(vif_multicolinairity)
vif_multicolinairity <-vif_multicolinairity[order(vif_multicolinairity$vif.fit. , decreasing = TRUE ),] 
head(vif_multicolinairity)
 
# Use your model to make predictions, in this example newdata = training set, but replace with your test set    
prediction <- predict(fit, data = df, type = "response")

# use caret and compute a confusion matrix
confusionMatrix(data = factor(as.numeric(prediction>0.5)),reference =  factor(df$shares)) 
 


# Linearity assumption
# Select only numeric predictors
df_ <- df %>% dplyr::select_if(is.numeric) 
predictors <- colnames(df_)
# Bind the logit and tidying the data for plot
df_ <- df_ %>% 
  mutate(logit = log(prediction/(1-prediction))) %>%
  gather(key = "predictors", value = "predictor.value", -logit) 

# scater plots
ggplot(df_, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") 
# Not able to get any reults from above. 



# Influential points 
cooksd <- cooks.distance(fit) 

# Influential observation 
cutoff <- 4/(nrow(df)-length(fit$coefficients)-1) # common cut off
plot(fit, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")  

hat.plot <- function(fit) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main="Index Plot of Hat Values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(fit)  

# Influential row numbers  
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers 

# Delet the rows that are influential 
df <- df[ -influential, ]  
 
# standarised residuals
# extract model results
fit.results <- augment(fit) %>% 
  mutate(index = 1:n()) 

ggplot(fit.results, aes(index, .std.resid)) + 
  geom_point(aes(color = shares), alpha = .5) +
  theme_bw()  

fit <- glm(shares ~. , data = df, 
           family = binomial) 

# Summary 
summary(fit) 
# Use your model to make predictions, in this example newdata = training set, but replace with your test set    
prediction <- predict(fit, data = df, type = "response")
# use caret and compute a confusion matrix
confusionMatrix(data = factor(as.numeric(prediction>0.5)),reference =  factor(df$shares)) 
 


################################################################################### 
################################################################################### 
###################################################################################  
# 4 iii
 

# Get the summary 
coefficients_data_frame <- data.frame(summary(fit)$coefficient)
colnames(coefficients_data_frame) <- c("Estimate" , "Error Variance" , "T value" , "P value" )
confidence_interval <- data.frame(confint(fit)) 
colnames(confidence_interval) <- c("2.5%" , "97.5%")
coefficients_data_frame <- cbind(coefficients_data_frame,confidence_interval )
write.csv(coefficients_data_frame, "mlr_coeff_lr.csv")

### Calculate the pseudo R square 
efrons_pseudo_R_square = 1 - sum((df$shares - prediction) ** 2)/ sum((df$shares - mean(df$shares)) ** 2)
print(efrons_pseudo_R_square)

################################################################################### 
################################################################################### 
###################################################################################  
###################### Q 7 (ii)  
# Get the top two dataset with highest share
df_prediction <- data.frame(prediction)
df_prediction$article_nu = rownames(df_prediction)
head(df_prediction[order(-prediction),])  

# Original Df
df_ordignal <- read.csv('OnlineNewsPopularity.csv')  
df_ordignal$article_nu = rownames(df_ordignal)
head(df_ordignal[order(-shares),]$url) 


df_ordignal[df_ordignal$article_nu == 6384 , ]$url
df_ordignal[df_ordignal$article_nu == 29270, ]$url 

################################################################################### 
################################################################################### 
###################################################################################  
# 4 i 
###################### Q 7 (iii)
df <- read.csv('OnlineNewsPopularity.csv')  

# Outlier Treatment 
df$kw_min_avg[df$kw_min_avg <0] = 0 
df$kw_min_min[ df$kw_min_min <0] = 0 
df$n_non_stop_words[ (df$n_non_stop_words > 1) ]  = 1 
df$n_unique_tokens[ (df$n_unique_tokens > 1) ]  = 1
df$kw_avg_min[df$kw_avg_min <0] = 0

# removingthe variables that will be unknown before sharing, 
drops <- c("url", "is_weekend", "weekday_is_sunday", 
           "self_reference_min_shares", "self_reference_max_shares" , 
           "self_reference_avg_shares", "timedelta", "kw_min_min", 
           "LDA_02" , "rate_positive_words" , "kw_avg_min", "n_tokens_content",
           "n_unique_tokens", "non_stop_words", "n_non_stop_words")

df <- df[ , !(names(df) %in% drops)] 


coefficients_data_frame <- data.frame(summary(fit)$coefficient)
colnames(coefficients_data_frame) <- c("Estimate" , "Error_Variance" , "T_value" , "P_value" )
coefficients_data_frame$Variable <- rownames(coefficients_data_frame)

coff_max_min = data.frame(colnames(df))
coff_max_min$max = colMax(df)
coff_max_min$min = colMin(df) 
colnames(coff_max_min) = c("Variable" , "max" , "min")
 

coefficients_data_frame = merge(x=coff_max_min,y=coefficients_data_frame,by="Variable",all=TRUE) 
coefficients_data_frame = coefficients_data_frame[c( "Variable","Estimate" ,"max" , "min")]
 
# Replace na = 0
coefficients_data_frame[is.na(coefficients_data_frame) ] = 0
 
# Delete(Intercept)  from the row names
#coefficients_data_frame = coefficients_data_frame[coefficients_data_frame$variable != "(Intercept)" , ]

attach(coefficients_data_frame )

coefficients_data_frame$value_variable = ifelse(Estimate>=0, max, min) 
 
coefficients_data_frame_logistic <- coefficients_data_frame[ c("Variable","Estimate" , "value_variable")]


# Outer join on the two 
coeficient_value_hypothetical_article <- merge(x=coefficients_data_frame_linear,y=coefficients_data_frame_logistic,by="Variable",all=TRUE)

coeficient_value_hypothetical_article[is.na(coeficient_value_hypothetical_article) ] = 0
 

write.csv(coeficient_value_hypothetical_article ,  "hypothetical_article.csv")

sum(coefficients_data_frame$value_variable * coefficients_data_frame$Estimate) + 2.445194e-01

