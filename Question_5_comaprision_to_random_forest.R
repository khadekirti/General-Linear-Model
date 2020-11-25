library(heuristica)
library(aod)
library(ggplot2) 
library(caret) 
library(tidyverse)
library(broom)
library(car)
library(pscl)

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
df$shares <- ifelse(df$shares >= 1400  ,1,0) 

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

TruePositives / (TruePositives + FalsePositives)

accuracy = (14953 + 11095) / (11095  + 6201 +  7395 + 14953)

precison =  14953/ (14953 + 7395)

recal = 14953/ (14953 + 6201) 

f1 = (2 * precison * recal) / (precison + recal) 


print(accuracy,precison,recal, f1  )
