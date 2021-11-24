library(ggplot2)
library(dplyr)
library(plotrix)
covid = read.csv("Covid Dataset.csv", stringsAsFactors = TRUE)

View(covid)
col_names = c("breathe", "fever", "cough", "sore_throat", "running_nose", "asthma", 
              "lung", "headache", "heart", "diabetes", "hyper_tension", "fatigue", 
              "gastrointestinal", "abroad", "contact", "large_gethering", "public_exposed_places",
              "family", "masks", "sanitization", "covid19")
colnames(covid) = col_names

summary(covid)

ggplot(covid, aes(x=breathe, y=covid19, size = pop)) + geom_point(alpha=0.7)

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

ggplot(data=df_sum_yes, aes(x=reorder(df_sum_yes$groups, df_sum_yes$sum_yes), y=df_sum_yes$sum_yes)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=df_sum_yes$sum_yes), hjust=1.6, color="white", size=3.5)+ coord_flip()




