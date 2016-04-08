# Use the wine quality data
install.packages('randomForest')
library(randomForest)
library(ggplot2)

url = 'https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv'
wine = read.csv(url, sep = ';', header = TRUE)
head(wine)

# Assign segment membership
barplot(table(wine$quality))

wine$taste = ifelse(wine$quality < 6, 'bad', 'good')
wine$taste[wine$quality == 6] = 'normal'
is.factor(wine$taste)
wine$taste = as.factor(wine$taste)
table(wine$taste)

# Split data into train vs. test datasets
set.seed(123)
sample_size = sample(nrow(wine), 0.8 * nrow(wine))
train = wine[sample_size,]
test = wine[-sample_size,]

# Build random forest model
model = randomForest(taste ~ . - quality, data = train, importance = TRUE)
model
pred = predict(model, newdata = test)
table(pred, test$taste)
misClassificationError = mean(pred != test$taste)
paste('Accuracy', 1-misClassificationError)

# Create feature importance chart
imp = importance(model, type = 1)
featureImportance = data.frame(feature = row.names(imp), importance = imp[,1])

plot = ggplot(featureImportance, aes(x = reorder(feature, importance), y = importance)) +
  geom_bar(stat = "identity", fill = "#53cfff") +
  coord_flip() +
  theme_light(base_size = 15) +
  xlab("") +
  ylab("Importance") +
  ggtitle("Random Forest Feature Imporance\n") +
  theme(plot.title = element_text(size = 18))
  
plot
