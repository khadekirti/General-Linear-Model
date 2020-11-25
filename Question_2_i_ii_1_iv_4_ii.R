'''
2. (i) Perform exploratory data analysis as relevant to the construction of the regression models.
Investigate and highlight any apparent structure in the data.  
'''

#install.packages("jtools")
#install.packages("devtools")
#install.packages("interactions")
 
library(lattice)
library(Hmisc) 
library(ggplot2) 
library(jtools)
library(devtools)
library(interactions) 
library(jtools) # for summ()
library(reshape2)
library(dplyr) 
library(plyr)

setwd("~/Desktop/Statistics Machine Learning/Assignment - 1/OnlineNewsPopularity") 
df <- read.csv('OnlineNewsPopularity.csv') 
attach(df)

 

colnames(df)
describe(df)

#####################################################
# EDA on categorical variable 
########### EDA on categorical variable 

df_cat <- df[ c("data_channel_is_lifestyle" , 
                "data_channel_is_entertainment",  
                "data_channel_is_bus" , 
                "data_channel_is_socmed", 
                "data_channel_is_tech" , 
                "data_channel_is_world",
                "weekday_is_monday", "weekday_is_tuesday" , "weekday_is_wednesday" , "weekday_is_thursday" , "weekday_is_friday" , "weekday_is_saturday" , "weekday_is_sunday" , "is_weekend" , "shares")] 


df_cat$channel <- ifelse(df$data_channel_is_lifestyle ==1, "lifestyle",
                         ifelse(df$data_channel_is_entertainment ==1, "entertainment",
                                ifelse(df$data_channel_is_bus ==1, "bus",
                                       ifelse(df$data_channel_is_socmed ==1, "socmed",
                                              ifelse(df$data_channel_is_world ==1, "world",
                                                     ifelse(df$data_channel_is_tech ==1, "tech",
                                                            "NA"))))))

counts <- table(df_cat$channel)


barplot(counts, main="Distribution of Channel",
        xlab="Number of shares in each channel") 

df$channel <- ifelse(df$data_channel_is_lifestyle ==1, "lifestyle",ifelse(df$data_channel_is_entertainment ==1, "entertainment", ifelse(df$data_channel_is_bus ==1, "bus", ifelse(df$data_channel_is_socmed ==1, "socmed", ifelse(df$data_channel_is_world ==1, "world", ifelse(df$data_channel_is_tech ==1, "tech", "NA"))))))


df$day <- ifelse(df$weekday_is_monday ==1, "Monday",ifelse
                 (df$weekday_is_tuesday ==1, "Tuesday", ifelse 
                   (df$weekday_is_wednesday ==1, "Wednesday", ifelse 
                     (df$weekday_is_friday ==1, "Friday", ifelse 
                       (df$weekday_is_saturday ==1, "Saturday", ifelse
                         (df$weekday_is_sunday ==1, "Sunday", ifelse 
                           (df$weekday_is_thursday ==1, "Thursday", "NA")))))))

df[df$day == "NA" , ] 

counts <- table(df$day)

barplot(counts, main="Distribution of Day",
        xlab="Number of shares in each day")  

#####################################################
# Corrrelated columns 

drops <- c("url", "data_channel_is_lifestyle" ,  "data_channel_is_entertainment",  "data_channel_is_bus" , "data_channel_is_socmed", "data_channel_is_tech" , "data_channel_is_world",
           "weekday_is_monday", "weekday_is_tuesday" , "weekday_is_wednesday" , "weekday_is_thursday" , "weekday_is_friday" , "weekday_is_saturday" , "weekday_is_sunday" , "is_weekend",
           "day" , "channel") 

df_numeric  <- df[ , !(names(df) %in% drops)]

df_numeric_cor <- data.frame(cor(df_numeric)) 

df_numeric_cor <- cor(df_numeric) 

melted_df <- melt(round(df_numeric_cor, 3))

melted_df <- melted_df[melted_df$Var1 != melted_df$Var2 , ] 

head(melted_df) 

melted_df$value = abs(melted_df$value)

df_cor <- melted_df[melted_df$value >0.7, ] 

print(df_cor)
df_cor_share <- melted_df[melted_df$Var1  == "shares", ] 
print(df_cor_share)

#####################################################
# EDA on share
bwplot(shares)
ggplot(shares)
#####################################################
# EDA on all continuous variable 
df_logistic <- read.csv('OnlineNewsPopularity.csv') 
df <- read.csv('OnlineNewsPopularity.csv')
#Using box-cox value, we found the value of transform for linear model
df$shares <- df$shares ** -0.2222222 
# Fr logistic Model 
df_logistic$shares <- ifelse(df_logistic$shares >= 1000,1,0) 

 
###
# Change the variable as per study, apply transformation if any  
# And add outlier treatment if any
variable = df$LDA_00
### 

#Describe
describe(variable)
#Max
max(variable)
#Min 
min(variable)
#Outliers 
bwplot(variable)
#Normal Q-Q plot 
ggplot(variable)
# Linearity Test

fit <- lm(df$shares ~ variable)
plot(fit, which = 1)
plot(fit, which = 2)

#Log odd quaretile Test 
# Use the variable used above, 
#or any transformation on it for which we think is valid
variable2 = variable
describe(variable2)
#Check that there is no NaN value
variable2[is.na(variable2)] = 0
# If there is infinte value, replace it with 95% value
variable2[is.infinite(variable2)] = quantile(variable2,probs=c(.95))[1]
# Make qurtile for the variable
df_logistic$quartile <- ntile(variable2, 1000)
# Get the log odds value
average.p.a <- aggregate( df_logistic$shares, list(df_logistic$quartile), mean)['x'] 
log_ods <- as.numeric(log((average.p.a / (1 - average.p.a )) + 1)$x)
describe(log_ods)
# If infinite,replace with 95% value
log_ods[is.infinite(log_ods)] = quantile(log_ods,probs=c(.95))[1]
quartile_values <- as.numeric(unlist(aggregate(variable2, list(df_logistic$quartile), mean)['x'])) 
describe(quartile_values)
# Fit the linear regression model 
fit <- lm(log_ods ~ quartile_values)
plot(fit, which = 1) 
# If not linear, figure out the best lambda using the inverse of lambda found here 
bc = boxcox(fit , data = df_logistic)
best.lam = bc$x[which(bc$y == max(bc$y))]
print(best.lam) 
 

#####################################################
#  EDA on categorical and continuous variable 
df$channel <- ifelse(df$data_channel_is_lifestyle ==1, "lifestyle",
                         ifelse(df$data_channel_is_entertainment ==1, "entertainment",
                                ifelse(df$data_channel_is_bus ==1, "bus",
                                       ifelse(df$data_channel_is_socmed ==1, "socmed",
                                              ifelse(df$data_channel_is_world ==1, "world",
                                                     ifelse(df$data_channel_is_tech ==1, "tech",
                                                            "NA"))))))
 
attach(df)
boxplot( LDA_00 ~ channel) 
boxplot( LDA_01 ~ channel)
boxplot( LDA_02 ~ channel) 
boxplot( LDA_03 ~ channel) 
boxplot( LDA_04 ~ channel) 

################################################################################### 
################################################################################### 
################################################################################### 

'''
2. (ii)/ 1(iv) Perform exploratory data analysis as relevant to the construction of the regression models.
Investigate and highlight any apparent structure in the data.  
''' 


# Interaction between global_rate_negative_words and avg_negative_polarity    
fiti <- lm(log(shares)~ LDA_00 * data_channel_is_socmed + LDA_00 + data_channel_is_socmed, data = df)
summ(fiti)
interact_plot(fiti, pred = LDA_00, modx = data_channel_is_socmed)
 
fiti <- lm(log(shares)~ LDA_01 * data_channel_is_entertainment + LDA_01 + data_channel_is_entertainment, data = df)
summ(fiti)
interact_plot(fiti, pred = LDA_01, modx = data_channel_is_entertainment) 


fiti <- lm(log(shares)~ LDA_02 * data_channel_is_world + LDA_02 + data_channel_is_world, data = df)
summ(fiti)
interact_plot(fiti, pred = LDA_02, modx = data_channel_is_world) 


fiti <- lm(shares ~ LDA_03 * data_channel_is_entertainment, data = df)
summ(fiti)
interact_plot(fiti, pred = LDA_03, modx = data_channel_is_entertainment)  


fiti <- lm(shares ~ LDA_04 * data_channel_is_lifestyle, data = df)
summ(fiti)
interact_plot(fiti, pred = LDA_04, modx = data_channel_is_lifestyle)  

################################################################################### 
################################################################################### 
###################################################################################  





