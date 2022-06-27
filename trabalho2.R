library(readr)
library(gridExtra)
library(dplyr)
data  = read.csv('cumulative.csv', stringsAsFactors = FALSE)

#1target
target <- data$koi_pdisposition
sample(data)
unique(data$koi_duration)

data.column.class = data$koi_pdisposition
data$rowid<- NULL
data$kepid<- NULL
data$kepler_name<- NULL
data$koi_disposition<- NULL
data$koi_pdisposition<- NULL
data$kepoi_name<-NULL
data$koi_teq_err1<-NULL
data$koi_teq_err2<-NULL


sapply(data, typeof)

# 4 - Exploração dos dados atraves de medidas de localidade

#função para coletar a moda
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]

}
moda <- getmode(data$koi_disposition)
print(moda)
#Frequencia pdisposition
hist(data$koi_pdisposition)


#Frequencia do koi_score
hist(data$koi_score)


#Tamanaho dos corpos
boxplot(data$koi_duration)
max(data$ra)
min(data$ra)
mean(data$ra)

boxplot(data$dec)
mean(data$dec)
#Tamanho do koi_time
boxplot(data$koi_time0bk)


#Separação de conjuntos de teste e treino
sample <- sample(c(data), nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train  <- data[sample, ]
test   <- data[!sample, ]