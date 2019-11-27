dataaa <- read.csv("./hw1/abalone.csv")
t#library readr read_csv
#str(data) structure of data
names(data) # column names
nrow(data) # number of rows
data[1:4,] #first 4 rows head(data,4)
data[1:4,6] #value of rings feature
data[(nrow(data)-2):nrow(data),5] # Weights of last 3 abalones tail(data,3)
data[577,3] # value of diameter in the row 577 #data[577,"Diameter"] data$Diameter[577]
mean(data[,4]) # mean of the height column
#mean(data$Height)
selected <- subset(data,Gender=="M" & Weight<0.75) # Gender M and Weight<0.75
mean(selected[,3]) # mean of diameter in selected data
mean(subset(data,Gender=="M" & Weight<0.75)$Diameter)

data %>%
  filter(Gender == "M") %>%
  filter(Weight<0.75) -> selected2
mean(selected2[,3])

data %>% #from top to bottom
  filter(Gender == "M") %>%
  filter(Weight<0.75) %>%
  .$Diameter %>%
  mean()

data %>%
  arrange(Length) %>%
  filter(Rings == 18) -> selected3
selected3[1,2] #minimum length when rings = 18

data %>%
  filter(Rings == 18) %>%
  select(Length) %>% #.$Length
  min()

grouped_data = group_by(data,Rings)
ringsToWeights = summarise(grouped_data, average_weight = mean(Weight))

data %>%
  group_by(Rings) %>%
  summarise(mean_weight=mean(Weight))
