adult_Data <- read.delim("adult.csv", header = TRUE, sep = "\t", quote = "\"",
                         dec = ".", fill = TRUE, comment.char = "")
adult_Data_Org <- read.delim("adult.csv", header = TRUE, sep = "\t", quote = "\"",
                             dec = ".", fill = TRUE, comment.char = "")

first100Nominal <- adult_Data[1:500,2]
table(first100Nominal)
table(table(first100Nominal))


#adjust age attribute
allAges <- adult_Data[,2]
######################
allMissingAges = which(as.character(allAges) < "17" | as.character(allAges) > "69")
allAges[allMissingAges] = NA
allAges[which(as.character(allAges) == "320")] = NA
adult_Data[,2] = allAges
ggplot(adult_Data,aes(x=age)) + geom_histogram(stat = "count",binwidth = 10)
###############
allWorkClasses <- adult_Data[,3]
allMissingWorkClasses = which(as.character(allWorkClasses)==" ?" | as.character(allWorkClasses)== "privat" |
                as.character(allWorkClasses)==" Never-worked" | as.character(allWorkClasses)==" Without-pay")
allWorkClasses[allMissingWorkClasses] = NA
adult_Data[,3] = allWorkClasses
#######################
allEducation <- adult_Data[,4]
allMissingEducation = which(as.character(allEducation)==" Preschool")
allEducation[allMissingEducation] = NA
adult_Data[,4] = allEducation
################
allOccupation <- adult_Data[,5]
allMissingOccupation = which(as.character(allOccupation)==" ?")
allOccupation[allMissingOccupation] = NA
adult_Data[,5] = allOccupation
####
allCapGain <- adult_Data[,6]
####
allCapLoss <- adult_Data[,7]
####
allNativeCountry <- adult_Data[,8]
allMissingNativeCountry = which(as.character(allNativeCountry)=="United-States" | 
                                  as.character(allNativeCountry)== "Unitedstates" |
                                  as.character(allNativeCountry)==" South" | 
                                  as.character(allNativeCountry)==" Outlying-US(Guam-USVI-etc)" |
                                  as.character(allNativeCountry)==" ?" |
                                  as.character(allNativeCountry)==" Hong" |
                                  as.character(allNativeCountry)==" Laos"                                                 
                                  )
allNativeCountry[allMissingNativeCountry] = NA
adult_Data[,8] = allNativeCountry
####
allSalaries <- adult_Data[,9]
allMissingSalaries = which(as.integer(allSalaries) != allSalaries)
allSalaries[allMissingSalaries] = NA
adult_Data[,9] = allSalaries
####################
allJobSatisfaction <- adult_Data[,10]
allMissingJobSatisfaction = which(as.character(allJobSatisfaction)=="Very good") #row 200
allJobSatisfaction[allMissingJobSatisfaction] = NA
adult_Data[,10] = allJobSatisfaction
#####################
allMale <- adult_Data[,11]
#######################
allFemale <- adult_Data[,12]
############################
modifiedRows = unique(
  c(allMissingAges,allMissingWorkClasses,allMissingEducation,allMissingSalaries,allMissingJobSatisfaction)
  )
######################
table(allAges)
table(allWorkClasses)
table(allEducation)
table(allOccupation)
table(allCapGain)
table(allCapLoss)
table(allNativeCountry)
table(allSalaries)
table(allJobSatisfaction)
table(allMale)
table(allFemale)
##################################

#Create a table where each row stands for an occupation, 
#each column stands for a level of education, and the cells in the table 
#contain the average salary of people with the corresponding occupation and education level.
educationLevelNum = length(levels(allEducation)) - 1
occupationLevelNum = length(levels(allOccupation)) - 1
occupationLevel = levels(allOccupation)[-1]
educationLevel = levels(allEducation)[-14]
salaryMatrix = matrix(rep(0,educationLevelNum*occupationLevelNum),ncol=educationLevelNum,byrow = TRUE)
for(occ in 1:occupationLevelNum) {
  occupation = occupationLevel[occ]
  for(edc in 1:educationLevelNum) {
    education = educationLevel[edc]
    salaryMatrix[occ,edc] = mean(subset(allSalaries,adult_Data$education==education 
                                        & adult_Data$occupation==occupation))
  }
}
colnames(salaryMatrix) <- educationLevel
rownames(salaryMatrix) <- occupationLevel
salaryTable <- as.table(salaryMatrix)
orderedEducation <- factor(educationLevel, 
                           levels = c(" 1st-4th"," 5th-6th"," 7th-8th", " 9th", " 10th", " 11th", " 12th",
                                      " HS-grad"," Some-college", " Bachelors", " Masters", " Doctorate",
                                      " Assoc-acdm", " Assoc-voc", " Prof-school"), ordered = TRUE)
colnames(salaryMatrix) <- levels(orderedEducation)
wideData <- gather_(as.data.frame(salaryMatrix), key_col = "Education", value_col = "AverageSalary",levels(orderedEducation))
occupations <- rep(occupationLevel,educationLevelNum)
wideData[,3] = occupations
colnames(wideData)[3] = "Occupation"
wideData$Education <- factor(wideData$Education,
                             levels = c(" 1st-4th"," 5th-6th"," 7th-8th", " 9th", " 10th", " 11th", " 12th",
                                        " HS-grad"," Some-college", " Bachelors", " Masters", " Doctorate",
                                        " Assoc-acdm", " Assoc-voc", " Prof-school"), ordered = TRUE)
allHigherSalary = which(wideData$AverageSalary>40000)
wideData = wideData[-allHigherSalary,]
ggplot(wideData,aes(x=Education,y=Occupation,fill=AverageSalary)) + geom_tile()

################################################
instacartDataSet <- read.csv("instacart_100k.csv")

ggplot(instacartDataSet,aes(x=order_hour_of_day)) + geom_histogram(binwidth = 1) + facet_wrap(. ~ order_dow)
ggplot(instacartDataSet,aes(x=department)) + geom_histogram(stat = "count") + theme(axis.text.x = element_text(angle=90, vjust = 1))

ggplot(adult_Data,aes(x=salaries)) + geom_histogram() + theme(axis.text.x = element_text(angle=90, vjust = 1))