library(lattice)
library(Hmisc) 
library(ggplot2) 
library(jtools)
library(devtools)
library(interactions) 
library(jtools) # for summ()
library(reshape2) 
library(car)
library(tidyverse)
library(broom) 
library(Metrics) 

setwd("~/Desktop/Statistics Machine Learning/Assignment - 1/OnlineNewsPopularity") 
df <- read.csv('OnlineNewsPopularity.csv')  

################################################################################### 
################################################################################### 
###################################################################################  
"
2 iii
"

# Outlier Treatment 
df$kw_min_avg[df$kw_min_avg <0] = 0 
df$kw_min_min[ df$kw_min_min <0] = 0 
df$n_non_stop_words[ (df$n_non_stop_words > 1) ]  = 1 
df$n_unique_tokens[ (df$n_unique_tokens > 1) ]  = 1
df$kw_avg_min[df$kw_avg_min <0] = 0

# Applying transforms 
df$kw_min_avg  <-   log(df$kw_min_avg   + 1)
df$kw_min_min  <-   log( df$kw_min_min + 1)
df$n_tokens_content <-    log( df$n_tokens_content + 1) 
df$num_imgs         <-    log( df$num_imgs + 1)
df$kw_avg_avg      <-     log(df$ kw_avg_avg + 1)
df$kw_avg_max       <-    log( df$kw_avg_max  + 1)
df$kw_avg_min       <-    log(df$kw_avg_min  + 1)
df$kw_max_avg       <-    log( df$kw_max_avg   + 1)
df$kw_max_max       <-    log( df$kw_max_max + 1)
df$kw_max_min       <-    log(df$kw_max_min + 1)
df$kw_min_max       <-    log(df$kw_min_max + 1)
df$num_hrefs        <-     log(df$num_hrefs+1)
df$self_reference_avg_sharess  <-    log(df$self_reference_avg_sharess + 1)
df$self_reference_max_shares   <-    log(df$self_reference_max_shares + 1)
df$self_reference_min_shares   <-    log(df$self_reference_min_shares + 1)
df$shares <- df$shares ** -0.2222222 
 

# removingthe variables that will be unknown before sharing, 
# and "url" as its a character 
# and redundent categortrical variables like "is_weekend", "weekday_is_sunday"
# and lastly, "kw_min_min" due to the max records being negative and timedelta as its irrelevent 
drops <- c("url", "is_weekend", "weekday_is_sunday", 
           "self_reference_min_shares", "self_reference_max_shares" , 
          "self_reference_avg_shares", "timedelta", "kw_min_min")

df <- df[ , !(names(df) %in% drops)] 

# Removing these variables w.r.t. the VIF analysis 
df <- df[, !(colnames(df) %in% c("LDA_03"))] 
df <- df[, !(colnames(df) %in% c("rate_positive_words"))] 
df <- df[, !(colnames(df) %in% c("kw_min_max"))] 
df <- df[, !(colnames(df) %in% c("n_non_stop_words"))] 
df <- df[, !(colnames(df) %in% c("kw_avg_max"))] 
df <- df[, !(colnames(df) %in% c("kw_max_min"))] 
df <- df[, !(colnames(df) %in% c("kw_avg_avg"))] 



# Perfomring Multiple Linear Regression
fit <- lm(shares  ~ ., data = df)
summary(fit)
plot(fit)

# Check if this is the best lambda value 
library(MASS)
bc = boxcox(fit , data = df)
best_lam = bc$x[which(bc$y == max(bc$y))]
print(best_lam) 
# since its close to 1, we are going to asssume that this is the best lambda value 

# check for multicoliniarity 
vif_multicolinairity <- data.frame(vif(fit))
vif_multicolinairity$variable <- rownames(vif_multicolinairity)
vif_multicolinairity <-vif_multicolinairity[order(vif_multicolinairity$vif.fit. , decreasing = TRUE ),] 
head(vif_multicolinairity)

fit <- lm(shares  ~ ., data = df)
summary(fit)
 

# Residual independent 
prediction <- predict(fit, data = df)
x = prediction - df$shares
cr <- acf (x, lag = length(x)-1, correlation = TRUE)
plot(cr)
# Since, all the correlation is less than 0.1, there is no dependency 
 
# normality
par(mfrow=c(1,1))
qqPlot(fit, labels=row.names(states), id.method="identify",
       simulate=TRUE, main="Q-Q Plot") 


# Constance varience - Hetrocity 
ncvTest(fit) 

# linearity
crPlots(fit)
# n_non_stop_unique_tokens did not have a unique plot, hence changing it
df$n_non_stop_unique_tokens <- df$n_non_stop_unique_tokens ** -2 
df$n_non_stop_unique_tokens[df$n_non_stop_unique_tokens <0 ] = 0
df$n_non_stop_unique_tokens[df$n_non_stop_unique_tokens > 4.390  ] = 4.390  

fit <- lm(shares  ~ . , data = df)
summary(fit)

# normality
qqPlot(fit, labels=row.names(df), id.method="identify",
       simulate=TRUE, main="Q-Q Plot") 


#Outliers 
# Influential observation 
cutoff <- 4/(nrow(df)-length(fit$coefficients)-1) # common cut off
plot(fit, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red") 
 

# Examine the highe leverage points 
hat.plot <- function(fit) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main="Index Plot of Hat Values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(fit) 

cooksd <- cooks.distance(fit) 
# Influential row numbers  
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers 

# Delet the rows that are influential 
df <- df[ -influential, ] 

# Cheacking the normal plot post this 
qqPlot(fit, labels=row.names(df), id.method="identify",
       simulate=TRUE, main="Q-Q Plot") 
 
# Global validation of linear model assumption 
library(gvlma)
gvlma(x = fit) 

fit <- lm(shares  ~ . , data = df) 
summary(fit)  

################################################################################### 
################################################################################### 
###################################################################################  
"
2 iv / 3 ii 
" 
attach(df)
fit <- lm(shares  ~ . +
            LDA_00 * data_channel_is_socmed + 
            LDA_01 * data_channel_is_entertainment + 
            LDA_02 * data_channel_is_world +
            LDA_04 * data_channel_is_lifestyle  
            , data = df) 
summary(fit) 


# Get the coefficients 
coefficients_data_frame <- data.frame(summary(fit)$coefficient)
colnames(coefficients_data_frame) <- c("Estimate" , "Error_Variance" , "T_value" , "P_value" )
confidence_interval <- data.frame(confint(fit)) 
colnames(confidence_interval) <- c("2.5%" , "97.5%")
coefficients_data_frame <- cbind(coefficients_data_frame,confidence_interval )

write.csv(coefficients_data_frame, "mlr_coeff2.csv")

prediction <- predict(fit, data = df)


# RMSE 
# The RMSE is the square root of the variance of the residuals. 
# It indicates the absolute fit of the model to 
# the data–how close the observed data points are to the model’s predicted values.
# Whereas R-squared is a relative measure of fit, RMSE is an absolute measure of fit. 
rmse(df$shares, prediction) 
  
################################################################################### 
################################################################################### 
###################################################################################  
###################### Q 7 (ii)  
# Get the top two dataset wit LOWEST Predictions 
df_prediction <- data.frame(prediction)
df_prediction$article_nu = rownames(df_prediction)
head(df_prediction[order(prediction),]) 

# Original Df
df_ordignal <- read.csv('OnlineNewsPopularity.csv')  
df_ordignal$article_nu = rownames(df_ordignal)
head(df_ordignal[order(-shares),]$url) 
 

df_ordignal[df_ordignal$article_nu == 5675 , ]$url
df_ordignal[df_ordignal$article_nu == 1120 , ]$url


################################################################################### 
################################################################################### 
###################################################################################  
###################### Q 7 (iii)
linear_fit <- lm(shares  ~ . , data = df) 
summary(linear_fit)  

df <- read.csv('OnlineNewsPopularity.csv')  

# Outlier Treatment 
df$kw_min_avg[df$kw_min_avg <0] = 0 
df$kw_min_min[ df$kw_min_min <0] = 0 
df$n_non_stop_words[ (df$n_non_stop_words > 1) ]  = 1 
df$n_unique_tokens[ (df$n_unique_tokens > 1) ]  = 1
df$kw_avg_min[df$kw_avg_min <0] = 0

# Applying transforms 
df$kw_min_avg  <-   log(df$kw_min_avg   + 1)
df$kw_min_min  <-   log( df$kw_min_min + 1)
df$n_tokens_content <-    log( df$n_tokens_content + 1) 
df$num_imgs         <-    log( df$num_imgs + 1)
df$kw_avg_avg      <-     log(df$ kw_avg_avg + 1)
df$kw_avg_max       <-    log( df$kw_avg_max  + 1)
df$kw_avg_min       <-    log(df$kw_avg_min  + 1)
df$kw_max_avg       <-    log( df$kw_max_avg   + 1)
df$kw_max_max       <-    log( df$kw_max_max + 1)
df$kw_max_min       <-    log(df$kw_max_min + 1)
df$kw_min_max       <-    log(df$kw_min_max + 1)
df$num_hrefs        <-     log(df$num_hrefs+1)
df$self_reference_avg_sharess  <-    log(df$self_reference_avg_sharess + 1)
df$self_reference_max_shares   <-    log(df$self_reference_max_shares + 1)
df$self_reference_min_shares   <-    log(df$self_reference_min_shares + 1)
 

# removingthe variables that will be unknown before sharing, 
drops <- c("url", "is_weekend", "weekday_is_sunday", 
           "self_reference_min_shares", "self_reference_max_shares" , 
           "self_reference_avg_shares", "timedelta", "kw_min_min", 
           "LDA_03", "rate_positive_words", "kw_min_max","n_non_stop_wordss",
           "kw_avg_max", "kw_max_min", "kw_avg_avg")

df <- df[ , !(names(df) %in% drops)] 

# Get the max and min of all the coeffecients
colMax <- function(data) sapply(data, max, na.rm = TRUE) 
colMin <- function(data) sapply(data, min, na.rm = TRUE) 

coff_max_min = data.frame(colnames(df))
coff_max_min$max = colMax(df)
coff_max_min$min = colMin(df) 
colnames(coff_max_min) = c("Variable" , "max" , "min")

coefficients_data_frame <- data.frame(summary(linear_fit)$coefficient)
colnames(coefficients_data_frame) <- c("Estimate" , "Error_Variance" , "T_value" , "P_value" )
coefficients_data_frame$Variable <- rownames(coefficients_data_frame)

coefficients_data_frame = merge(x=coff_max_min,y=coefficients_data_frame,by="Variable",all=TRUE) 
coefficients_data_frame = coefficients_data_frame[c( "Variable","Estimate" ,"max" , "min")]

# Replace na = 0
coefficients_data_frame[is.na(coefficients_data_frame) ] = 0

coefficients_data_frame = coefficients_data_frame[coefficients_data_frame$Variable != "(Intercept)" , ]

coefficients_data_frame_linear = coefficients_data_frame[c("Variable" , "Estimate", "value_variable")]

