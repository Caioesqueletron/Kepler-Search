library(readr)

data <- read.csv("../datasets-uci-iris.csv", stringsAsFactors = FALSE)

data.column.class <- data$class
data$class <- NULL

boxplot(data)
hist(data$sepal.length)

max(data$sepal.length)
min(data$sepal.length)
mean(data$sepal.length)  
median(data$sepal.length)
  
value.column <- ifelse(data.column.class == "setosa", -1, ifelse (data.column.class == "versicolor", 0, 1))
hist(value.column)
