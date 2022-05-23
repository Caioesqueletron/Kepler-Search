library(readr)
library(gridExtra)
data  = read.csv('cumulative.csv', stringsAsFactors = FALSE)

data.column.class = data$koi_score
data$class <- NULL



data.column.class.class = data$koi_impact_err1.length

hist(data.column.class)
