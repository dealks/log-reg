library(corrgram)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrplot)
library(caTools)

#READ DATA
df <- read.csv('input_data.csv', sep=',')
head(df)

#CHECK SUMMARY
summary(df)

######CLEAN DATA
#check for nulls - none
any(is.na(df))

#check categorical vars
str(df)

df$CHG_L26_39_TRANS <- as.numeric(df$CHG_L26_39_TRANS)
df$CHG_L26_39_SALES <- as.numeric(df$CHG_L26_39_SALES)
df$CHG_L26_39_BOXES <- as.numeric(df$CHG_L26_39_BOXES)

str(df)

df2_cleaned <- df

##### CHECK CORRELATIONS
#grab number columns only
num.cols <- sapply(df,is.numeric)
print(num.cols)

#summary
summary(df[,num.cols])
summary(df)

cor.data <- cor(df[,num.cols])
print(cor.data)

#Corrplot
corrplot(cor.data,method='color')

#split data into training/test
sample <- sample.split(df2_cleaned$LAPSED_FLAG, SplitRatio = .7)
train <- subset(df2_cleaned, sample==TRUE)
test <- subset(df2_cleaned,sample==FALSE)
head(train)
head(test)

#TRAIN AND BUILD MODEL
head(df2_cleaned)

log.model <- glm(formula=LAPSED_FLAG ~ CHG_L26_39_SALES + CHG_L13_26_TRANS + CHG_L13_26_SALES +
                   CLAIM_ON_LAST_TRANS, 
                 family = binomial(link='logit'),data = train)

#Interpret the Model
print(summary(log.model))

#Predictions - ACCURACY AT 88%
fitted.probabilities <- predict(log.model,newdata=test,type='response')
head(fitted.probabilities)
test$preds <- fitted.probabilities
head(test)

fitted.results <- ifelse(fitted.probabilities > 0.5,1,0)
test$fitted.results <- fitted.results
head(test)
misClasificError <- mean(fitted.results != test$LAPSED_FLAG)
print(paste('Accuracy',1-misClasificError))

table(test$LAPSED_FLAG, fitted.probabilities > 0.5)

head(fitted.results)


#GET CONFIDENCE INTERVALS FOR PREDICTIONS 

preds <- predict(log.model, newdata = test, type = "link", se.fit = TRUE)

critval <- 1.96 ## approx 95% CI
upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit

test$lwr <- lwr
test$upr <- upr
test$fit <- fit

head(test)

write.csv(test, file = "model_output.csv",row.names=FALSE)

head(test)