# Use the same Titanic data
install.packages('randomForest')
library(randomForest)
library(ggplot2)


rf = randomForest(train[,c(2:8)], train$Survived, ntree = 100, importance = TRUE)

imp = importance(rf, type = 1)
featureImportance = data.frame(Feature = row.names(imp), importance = imp[,1])

plot = ggplot(featureImportance, aes(x=reorder(Feature, imp), y=imp)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("") +
  ylab("Importance") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=18))

plot
