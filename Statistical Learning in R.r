### 1. Linear Regression

# Write functions
LoadLibraries=function() {
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded.")
}

LoadLibraries()

# Read dataset "Auto"
summary(Auto)
sapply(Auto,function(x) sum(is.na(x))) # No null values
sapply(Auto,function(x) length(unique(x))) # Check number of unique values
sapply(Auto, is.factor) # Which variables are factor by default

# Build simple linear regression
lm1=lm(mpg~horsepower,data=Auto) # Linear model of mpg vs. horsepower
summary(lm1) # Model output
pred = predict(lm1,data.frame(horsepower=c(98)),interval = "confidence") # Predict mpg when horsepower=98 and show CI
pred
plot(Auto$horsepower,Auto$mpg) # Scatterplot between mpg vs. horsepower
abline(lm1) # + linear line
par(mfrow=c(2,2))
plot(lm1) # Produce diagnostic plots of the linear regression fit

# Build multiple linear regression
pairs(subset(Auto,select = c(-name))) # Create scatterplots of variables
cor(Auto[,c(1:8)]) # Check correlation
lm2=lm(mpg~.-name, data = Auto) # Fit linear model of mpg of all the other variables except name
summary(lm2) # Model output
par(mfrow=c(2,2))
plot(lm2)
