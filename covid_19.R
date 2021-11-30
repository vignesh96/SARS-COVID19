library(ggplot2)
library(dplyr)
library(plotrix)
library(performanceEstimation)

covid = read.csv("Covid Dataset.csv", stringsAsFactors = TRUE)

View(covid)
col_names = c("breathe", "fever", "cough", "sore_throat", "running_nose", "asthma", 
              "lung", "headache", "heart", "diabetes", "hyper_tension", "fatigue", 
              "gastrointestinal", "abroad", "contact", "large_gethering", "public_exposed_places",
              "family", "masks", "sanitization", "covid19")
colnames(covid) = col_names

summary(covid)

plot(covid19~breathe+fever, data=covid)

counts <- table(covid$breathe, covid$fever)
mosaicplot(counts, xlab='breathe', ylab='fever',
           main='Comparision', col='steelblue')

covid[covid$covid19 == "Yes",]

yesses = sapply(covid,FUN = function(x){(length(x[x=="Yes"])/4383)*100})
print(yesses)
yesses = yesses[-21]
sum_yes = round((yesses / sum(yesses)) * 100, 2)
print(sum_yes)

df_sum_yes = data.frame(sum_yes)
df_sum_yes["groups"] = row.names(df_sum_yes)

ggplot(data=df_sum_yes, aes(x=reorder(groups, sum_yes), y=sum_yes)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=sum_yes), hjust=1.6, color="white", size=3.5)+ coord_flip()

set.seed(508)
hw2sort_lr<-sample(1:nrow(covid),nrow(covid)*0.8)
train_lr<-covid[hw2sort_lr,]
test_lr<-covid[-hw2sort_lr,]
balanced.data <- smote(covid19 ~., train_lr, perc.over = 4800, k = 5, perc.under = 1000)

