# Rena Jing and Kevin Dang Nguyen
# ECS 132 - Project 3

#install.packages('stringr')
# Load in the required libraries
library(stringr)
# connect to the file
connecter <- file("1milliondigitsofpi.txt", "r", blocking = FALSE)
# read the data
z <- scan(connecter, what="character", skip=0)
close(connecter)

# join all digits
# number of pi digits considered
pidigits <- 200000
y <- ""
j <- 2
while (str_length(y) <= pidigits) {
  y <- str_c(y, z[j])
  j <- j + 1
}
n <- str_length(y);
print(n)

##########################################################################################
array = as.numeric(strsplit(as.character(y), "")[[1]])

##########################################################################################
count <- function(digit, length){
  
  counter = 0
  
  for (i in 1:length){
    if (array[i] == digit){
      counter = counter + 1
    }
  }
  return(counter)
}

##########################################################################################
#distance <- function(digit, length){
#  
#  array_Locations = NULL
#  array_Distances = NULL

#  location = 0

#  for (i in 1:length){
#    if (array[i] == digit){
#      location = i
#      array_Locations = c(array_Locations, location) 
#    }      
#  }
#  for (i in 1:length(array_Locations)){
#    distance = array_Locations[i+1] - array_Locations[i] - 1
#    array_Distances =  c(array_Distances, distance)
#  }
#  return(array_Distances)
#}
##########################################################################################
distance <- function(digit, length){
  
  array_Distances = NULL
  distance = 0
  counter = 0
  
  for (i in 1:length){
    if(array[i] == digit && counter == 0){
      counter = 1
    }
    
    else if (array[i] == digit && counter != 0){  
      array_Distances = c(array_Distances, distance) 
      distance = 0
      counter = counter + 1 
    }#if
    else{
      distance = distance + 1
    }
  }#for searching through entire array
  
  return(array_Distances)
}

##########################################################################################
# Step 1.1
distArray = NULL
distArray = distance(1,200000)
plot(distArray, xlab = "Number of Occurances", ylab = "Distances", main = "3.1.1 Scatterplot of Distances")

###########
# Step 1.2
boxplot(distArray, ylab = "Distances", main = "3.1.2 Box and Whisker Plot of Distances")

###########
# Step 1.3
quantile(distArray, 0.25)
quantile(distArray, 0.50)
quantile(distArray, 0.75)

###########
# Step 1.4
mean(distArray)
median(distArray)
var(distArray)

##########################################################################################

# Step 2.1
sampleMean <- function(sampleSize, numExperiements){
  distArray2 = distArray
  sampleMean = NULL
  for (i in 1:numExperiements){
    x = sample(distArray2, sampleSize, replace = FALSE)
    x = mean(x)
    sampleMean = c(sampleMean, x)
  }
  plot(sampleMean, xlab = "Number of Experiments", ylab = "Distances", main = "3.2.1/2 Sample Mean of Distances")
  return(sampleMean)
}

sampleMean(10, 100)
mean(sampleMean(10, 100))

###########
# Step 2.2
sampleMean(20, 100)
#mean(sampleMean(20, 100))
sampleMean(30, 100)
#mean(sampleMean(30, 100))
sampleMean(40, 100)
#mean(sampleMean(40, 100))

###########
# Step 2.3
sampleVariance <- function(sampleSize, numExperiements){
  distArray2 = distArray
  sampleVar = NULL
  for (i in 1:numExperiements){
    x = sample(distArray2, sampleSize, replace = FALSE)
    x = var(x)
    sampleVar = c(sampleVar, x)
  }
  plot(sampleVar, xlab = "Number of Experiments", ylab = "Distances", main = "3.2.3 Sample Variance of Distances")
  return(sampleVar)
}

sampleVariance(10, 100)
#mean(sampleVariance(10, 100))
sampleVariance(20, 100)
#mean(sampleVariance(20, 100))
sampleVariance(30, 100)
#mean(sampleVariance(30, 100))
sampleVariance(40, 100)
#mean(sampleVariance(40, 100))

###########
# Step 2.4
mean(distArray)
var(distArray)
# In statistics, the larger the sample sizes, the more it accurately reflects the 
# population mean and variance compared to the sample mean and sample variance.

##########################################################################################

# Step 3.1
rand_norm <- rnorm(100, mean(distArray), sd(distArray))
rand_norm
qqplot(sampleMean(10, 100), rand_norm)s

###########
# Step 3.2
qqplot(sampleMean(30,100), rand_norm)
qqplot(sampleMean(50,100), rand_norm)

###########
# Step 3.3


##########################################################################################
# Step 4.1
popSD = sd(distArray)
  # 95% confidence interval
sampleSize_95 = (1.96 * popSD / 0.01)^2
sampleSize_95 #3474958
  # 99% confidence interval
sampleSize_99 = (2.576 * popSD / 0.01)^2
sampleSize_99 #6002459

###########
# Step 4.2
sampleSD <- function(sampleSize, numExperiements){
  distArray2 = distArray
  sampleSD = NULL
  for (i in 1:numExperiements){
    x = sample(distArray2, sampleSize, replace = FALSE)
    x = mean(x)
    sampleSD = c(sampleSD, x)
  }
  return(sampleSD)
}
sampSD = mean(sampleSD(10,100))

sampleSize_sampleSD_95 = (1.96 * sampSD / 0.01)^2
sampleSize_sampleSD_95
sampleSize_sampleSD_99 = (2.576 * sampSD / 0.01)^2
sampleSize_sampleSD_99

###########
# Step 4.3
# Finding the sample size with the respected confidence intervals using the population 
# standard deviation is more accurate than using the sample standard deviations. Our 
# sample sizes are much larger using the population standard deviation compared to using
# the sample standard deviations which lowers are sample sizes.

##########################################################################################
# Step 5.1

popMean <- function(digit){
  
  count = count(digit,200000)
  mean = count/200000
  return(mean)
}


popDigitMean = NULL

for(i in 0:9){
  mean = popMean(i)
  popDigitMean <- c(popDigitMean, mean)
  
}


array2 = array 
randomSample2 = sample(array2, size = 30, replace = FALSE, prob = NULL)


countSample <- function(digit, length){
  
  counter = 0
  
  for (i in 1:length){
    if (randomSample2[i] == digit){
      counter = counter + 1
    }
  }
  return(counter)
}


meanSampleDistances <- function(digit){
  count = countSample(digit,30)
  mean = count/30
  return(mean)
  
}

sampleDigitMean = NULL


for(i in 0:9){
  mean = meanSampleDistances(i)
  sampleDigitMean <- c(sampleDigitMean, mean)
}

fit = lm(formula = sampleDigitMean ~ popDigitMean, data = galton)
fit
summary(fit)
cor(sampleDigitMean, popDigitMean)
plot(fit)



##########################################################################################
# Step 6.1
sampleMean100 = mean(sampleMean(100,100))
sampleMean100
rand_poisson100 = rpois(100, sampleMean100)
rand_poisson100

###########
# Step 6.2
numOfNines = 0
for (i in 1:(length(rand_poisson100))){
  if (rand_poisson100[i] == 9)
    numOfNines = numOfNines + 1
}
numOfNines

###########
# Step 6.3
hist(rand_poisson100, xlab = "Index", ylab = "Distance", main = "Histogram of Poisson Distribution of Distance")
hist(rand_norm, xlab = "Index", ylab = "Distance", main = "Histogram of Normal Distribution of Distance")
hist(sampleMean(10, 100))

###########
# Step 6.4
qqplot(rand_poisson100, rand_norm)
qqplot(rand_poisson100, sampleMean(10,100))

###########
# Step 6.5
# The Poisson distribution can be considered approximately Normal by the Central Limit Theorem if the sample size
# is big enough. Hence, the CLT states that the distribution of the sample average for any process is normally 
# distributed as shown in the qqplots where they look almost identical in the case with the Poisson random variable
# compared with the Normal and sample means.





