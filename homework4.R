data = list(
  c('B','D','F','H'),
  c('C','D','F','G'),
  c('A','D','F','G'),
  c('A','B','C','D','H'),
  c('A','C','F','G'),
  c('D','H'),
  c('A','B','E','F'),
  c('A','D','F','G','H'),
  c('A','C','D','F','G'),
  c('D','F','G','H'),
  c('A','C','D','E'),
  c('B','E','F','H'),
  c('D','F','G'),
  c('C','F','G','H'),
  c('A','C','D','F','H')
)

data = as(data, "transactions")
inspect(data)

library(dplyr)

find_freq_itemsets = function(data, min_support_count) {
  # convert support count into support
  min_support = min_support_count / length(data)
  
  # find itemsets with support>=min_support
  itemsets = eclat(data, parameter=list(support=min_support))
  
  # convert to data.frame, it is easier to manipulate
  itemsets = as(itemsets, "data.frame") 
  
  # items are factors, convert them to strings
  itemsets$items = as.character(itemsets$items)
  
  # sort by length of string, and among equal-length strings sort alphabetically
  itemsets = itemsets %>% arrange(nchar(items), items) 
}

itemsets = find_freq_itemsets(data,5)
print(itemsets)
print(find_freq_itemsets(data,1))

rules = apriori(data,parameter=list(support=5/length(data),conf=0.5))
inspect(rules)

titanic = read.csv('titanic.csv')
head(titanic)
str(titanic)
table(titanic$Class)
table(titanic$Sex)
table(titanic$Age)
table(titanic$Survived)

titanicRules = apriori(titanic)
titanicRules2 = titanicRules %>% as("data.frame") %>% arrange(-lift)
inspect(titanicRules)

rules = apriori(titanic,parameter=list(supp=0.000001,conf=0.000001))
rules = as(rules,"data.frame")
rules = rules %>% arrange(-lift)
rules %>% subset(confidence == 1 & lift > 3)
top3rules$count / top3rules$confidence

rulesA = rules %>% subset(support > 0.7) # remember you can show as many rows as you want by changing n
rulesA