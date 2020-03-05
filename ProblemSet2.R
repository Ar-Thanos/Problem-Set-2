#Problem 1
flu <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter
                    /wp-content/data/chapter10/
                    chap10e6AgesAtDeathSpanishFlu1918.csv"))
head(flu)
hist(flu$age, right = FALSE)
realMean <- mean(flu$age)
n <- 10
results <- vector()
containedInInterval <- double(10000)
count <- 0
for(i in 1:10000){
  AgeSample <- sample(flu$age, size = n, replace = FALSE)
  meanAge <- mean(AgeSample)
  stdDev <- sd(AgeSample)
  SE <- stdDev/sqrt(n)
  tStat <- (meanAge - realMean)/SE
  lower <- meanAge - (2.262*SE)
  higher <- meanAge + (2.262*SE)
  
  containedInInterval[i] <- (lower < realMean & realMean < higher)
}
for(i in 1:10000){
  if(containedInInterval[i] == TRUE){
    count = count + 1
  }
}
count/10000 #Fraction of intervals that have true population mean
#Same thing but now adjusting 80% confidence interval.
#Main change is changing critical t value
#Problem 1
flu <- read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter
                    /wp-content/data/chapter10
                    /chap10e6AgesAtDeathSpanishFlu1918.csv"))
head(flu)
hist(flu$age, right = FALSE)
realMean <- mean(flu$age)
n <- 10
results <- vector()
containedInInterval <- double(10000)
count <- 0
for(i in 1:10000){
  AgeSample <- sample(flu$age, size = n, replace = FALSE)
  meanAge <- mean(AgeSample)
  stdDev <- sd(AgeSample)
  SE <- stdDev/sqrt(n)
  tStat <- (meanAge - realMean)/SE
  lower <- meanAge - (1.383*SE)
  higher <- meanAge + (1.383*SE)
  
  containedInInterval[i] <- (lower < realMean & realMean < higher)
}
for(i in 1:10000){
  if(containedInInterval[i] == TRUE){
    count = count + 1
  }
}
count/10000
#Problem 2
matingPref <- c(-32,-29.8,-40.6,-90.8,-29.2,-28.8,-78.4,-59.2,-74.3)
t.test(matingPref, mu = 0)

#Problem 3
domSpec <- c(0.384,0.386,0.252,0.226,0.323,0.287,0.303,0.317,0.277)
subSpec <- c(0.507,0.569,0.235,0.415,0.436,0.451,0.399,0.220,0.338)
diffSpec <- double(9)
for(i in 1:length(domSpec)){
  diffSpec[i] <- domSpec[i] - subSpec[i]
}
meanDiff <- mean(diffSpec)
diffSE = sd(diffSpec)/sqrt(9)
t_stat <- meanDiff/diffSE
sp <- ((sd(domSpec)^2)*8 + (sd(subSpec)^2 *8))/(8+8)
pooledError <- sqrt(sp*(1/9 + 1/9))
t_stat <- (meanDom-meanSub)/pooledError
t.test(diffSpec)
#Problem 4
konanee <- c(1.11,1.34,1.55,1.53,1.5,1.71,1.87,
             1.86,1.82,2.01,1.95, 2.01, 1.66,1.49,1.59,1.69)
sockeye <- c(0.98,0.88,0.97,0.99,1.02,1.03,0.99,0.97,0.98,
             1.03,1.08,1.15,0.90,0.95,0.94,0.99)
meanKonColor <- mean(konanee) 
meanSockColor <- mean(sockeye)
spSalmon <- ((sd(konanee)^2)*15 + (sd(sockeye)^2 *15))/(15+15)
pooledErrorSalmon <- sqrt(spSalmon*(1/16 + 1/16))
t_stat_salmon <- (meanKonColor - meanSockColor)/pooledErrorSalmon
t.test(konanee,sockeye,paired=FALSE,var.equal = TRUE)
#now to logify this data set
konaneeLog <- log(konanee)
sockeyeLog <- log(sockeye)

meanKonLogColor <- mean(konaneeLog)
meanSockLogColor <- mean(sockeyeLog)
spSalmonLog <- ((sd(konaneeLog)^2)*15 + (sd(sockeyeLog)^2 *15))/(15+15)
pooledErrorSalmonLog <- sqrt(spSalmonLog*(1/16 + 1/16))
t_stat_salmonLog <- (meanKonLogColor - meanSockLogColor)/pooledErrorSalmonLog
t.test(konaneeLog,sockeyeLog,paired=FALSE,var.equal = TRUE)
#Problem 6
islandNoSquirrels <- c(9.6,9.4,8.9,8.8,8.5)
islandSquirrels <- c(6.8,6.6,6.0,5.7,5.3)
mainlandSquirrels <- c(6.7,6.4,6.2,5.7,5.6)

islandNoSquirrelMean <- mean(islandNoSquirrels)
islandSquirrelMean <- mean(islandSquirrels)
mainlandMean <- mean(mainlandSquirrels)
grandMean <- (islandNoSquirrelMean + islandSquirrelMean + mainlandMean)/3
SSgroups <- 5*(grandMean - islandNoSquirrelMean)^2
+5*(grandMean - islandSquirrelMean)^2+5*(grandMean - mainlandMean)^2

islandNoSquirSD <- sd(islandNoSquirrels)
islandSquirSD <- sd(islandSquirrels)
mainSD <- sd(mainlandSquirrels)
SSerror <- 4*(islandNoSquirSD^2) + 4*(islandSquirSD^2) + 4*(mainSD^2)
SStotal <- SSgroups + SSerror
MSgroups <- SSgroups/2
MSerror <- SSerror/12
Fstat <- MSgroups/MSerror
#Note p-value obtained from online calculator and reference to f-table