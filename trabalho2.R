library(readr)
library(gridExtra)
data  = read.csv('cumulative.csv', stringsAsFactors = TRUE)

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


#Frequencia pdisposition
hist(data$koi_pdisposition)


#Frequencia do koi_score
hist(data$koi_score)


#Tamanaho dos corpos
boxplot(data$ra)
max(data$ra)
min(data$ra)
mean(data$ra)

boxplot(data$dec)
mean(data$dec)
#Tamanho do koi_time
boxplot(data$koi_time0bk)

