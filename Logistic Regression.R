# Install packages
install.packages('Amelia')
library(Amelia)

install.packages('pscl')
library(pscl)

install.packages('gplots')
install.packages('bitops')
library(gplots)

install.packages('ROCR')
library(ROCR)

# Read data
getwd()
setwd("C:/Users/xxu/Desktop/R")

training.data.raw = read.csv('train.csv', header=T, na.strings = c(""))
sapply(training.data.raw, function(x) sum(is.na(x)))
sapply(training.data.raw, function(x) length(unique(x)))

# Check missing values
missmap(training.data.raw, main = "Missing values vs observed")
names(training.data.raw)
data = subset(training.data.raw, select = c(2, 3, 5, 6, 7, 8, 10, 12))

# Replace missing values
data$Age[is.na(data$Age)] = mean(data$Age, na.rm = T)
data = data[!is.na(data$Embarked),]
sapply(data, function(x) sum(is.na(x)))

# Format data
is.factor(data$Sex)
is.factor(data$Embarked)
is.factor(data$Survived)
is.factor(data$Pclass)

contrasts(data$Sex)
contrasts(data$Embarked)

data_dsp = data
data_dsp$Survived = factor(data$Survived, levels = c(1,0), labels = c("Survived", "Died"))
data_dsp$Pclass = factor(data$Pclass, levels = c(1,2,3), labels = c("First class", "Second class", "Third class"))
data_dsp$Sex = factor(data$Sex, levels = c("female","male"), labels = c("Female","Male"))

# Plot data
plot1 = mosaicplot(data_dsp$Pclass ~ data_dsp$Survived, main = "Passenger Survival by Class", color = c("#8dd3c7", "#fb8072"), 
                   shade=FALSE,  xlab="", ylab="",off=c(0), cex.axis=1.4)

plot2 = mosaicplot(data_dsp$Sex ~ data_dsp$Survived, main = "Passenger Survival by Gender", color = c("#8dd3c7", "#fb8072"), 
                   shade=FALSE,  xlab="", ylab="",off=c(0), cex.axis=1.4)

# Logistic regression
sample_size = floor(0.9 * nrow(data))
set.seed(123)
train_ind = sample(seq_len(nrow(data)), size = sample_size)
train = data[train_ind,]
test = data[-train_ind,]

model = glm(Survived ~., family = binomial(link = 'logit'), data = train)
summary(model)
anova(model, test='Chisq')
pR2(model)

# Assess the predictivity ability of the model
fitted.results = predict(model, newdata = subset(test, select = c(2:8)), type='response')
fitted.results = ifelse(fitted.results > 0.5, 1, 0)

misClassificationError = mean(fitted.results != test$Survived)
paste('Accuracy', 1-misClassificationError)

p = predict(model, newdata = subset(test, select = c(2:8)), type='response')
pr = prediction(p, test$Survived)
prf = performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc = performance(pr, measure = 'auc')
auc = unlist(auc@y.values)
auc
