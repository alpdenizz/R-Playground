data = sample(c(1,2,3), 20, replace = T, prob = c(0.1, 0.4, 0.5))
sample_n <- function(N, probs) {
  
  trials = c(rep(F,100))
  for(index in 1:100){
  mySample = sample(c(1,2,3,4), N, replace = T, prob = probs)
  distribution = table(mySample) / N
  errors = abs(distribution-probs)
  trials[index] = (length(which(errors<0.005)) == 4)
  }
  numberOfTrue = length(which(trials==TRUE)) 
  return(numberOfTrue)
  
}

sample_n <- function(N, probs) {
  
  trials = c(rep(F,100))
  for(index in 1:100){
    sDev = sqrt(probs*N)
    errors = abs(rnorm(4, mean = 0, sd = sDev) / N)
    trials[index] = (length(which(errors<0.005)) == 4)
  }
  numberOfTrue = length(which(trials==TRUE)) 
  return(numberOfTrue)
  
}

sample_n(5000,c(0.1,0.2,0.3,0.4))
sample_n(10000,c(0.1,0.2,0.3,0.4))
sample_n(20000,c(0.1,0.2,0.3,0.4))
sample_n(30000,c(0.1,0.2,0.3,0.4))
sample_n(40000,c(0.1,0.2,0.3,0.4))
sample_n(50000,c(0.1,0.2,0.3,0.4))
sample_n(55000,c(0.1,0.2,0.3,0.4))
sample_n(80000,c(0.1,0.2,0.3,0.4))
######################################
sample_n(5000,c(0.25,0.25,0.25,0.25))
sample_n(10000,c(0.25,0.25,0.25,0.25))
sample_n(20000,c(0.25,0.25,0.25,0.25))
sample_n(30000,c(0.25,0.25,0.25,0.25))
sample_n(40000,c(0.25,0.25,0.25,0.25))
sample_n(50000,c(0.25,0.25,0.25,0.25))
sample_n(55000,c(0.25,0.25,0.25,0.25))
sample_n(60000,c(0.25,0.25,0.25,0.25))
##########################################
ex3data = read.csv("hw3_ex3.csv")
corData = ex3data[,-1]
corData2 = ex3data[,-1]
correlations = cor(corData)
diag(correlations) = NA
correlations[lower.tri(correlations)] <- NA
testMax = max(correlations[upper.tri(correlations)])
testMin = min(correlations[upper.tri(correlations)])
###############################################
maxCorrelations = c(rep(0,1000))
minCorrelations = c(rep(0,1000))
for(t in 1:1000){
for(index in 1:100){
  corData[,index] = corData[sample(nrow(corData)),index]
}
correlations2 = cor(corData)
diag(correlations2) = NA
correlations2[lower.tri(correlations2)] <- NA
maxCorrelations[t] = max(correlations2[upper.tri(correlations2)])
minCorrelations[t] = min(correlations2[upper.tri(correlations2)])
}
maxStat = length(which(maxCorrelations >= testMax)) / 1000
minStat = length(which(abs(minCorrelations) >= abs(testMin))) / 1000
maxRandomCorr = max(maxCorrelations)
which(correlations>maxRandomCorr)
hist(maxCorrelations)
hist(minCorrelations)
########
beer <- c(27, 19, 20, 20, 23, 17, 21, 24, 31, 26, 28, 20, 27, 19, 25, 31, 24, 28, 24, 29, 21, 21, 18, 27, 20)
water <- c(21, 19, 13, 22, 15, 22, 15, 22, 20, 12, 24, 24, 21, 19, 18, 16, 23, 20)
##apply t-test
t.test(beer,water,alternative = "greater")
#############

###apply permutation
testMeanDiff = mean(beer) - mean(water)
meanDiffs = c(rep(0,50000))
for(r in 1:50000){
  place = sample(1:18,10)
  fromBeer = beer[place]
  fromWater = water[place]
  beer[place] = fromWater
  water[place] = fromBeer
  meanDiffs[r] = mean(beer) - mean(water)
}
hist(meanDiffs)
length(which(abs(meanDiffs) >= testMeanDiff)) / 50000
########