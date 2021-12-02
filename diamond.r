#read the data
diamond = read.csv("diamond.csv",header=T)
str(diamond)

#to check the dimension of the dataset
dim(diamond)

#copy of the dataset
diamond_copy = diamond

#checking missing values
sum(is.na(diamond))

#The first column is an index and thus we are going to remove it
diamond = subset(diamond,select = -c(X))

#to check the summary of the dataset
summary(diamond)

#dropping dimentionless diamonds
library(dplyr)
diamond <- diamond %>% filter(x!= 0)
diamond <- diamond %>% filter(y!= 0)
diamond <- diamond %>% filter(z!= 0)

#DISTRIBUTION OF PRICE ON NUMERICAL VARIABLES
library(ggplot2)

#price vs carat
ggplot(diamond,aes(x=carat,y=price))+
  geom_point(col="blue",shape=22)+
  labs(title="price Vs carat",x="carat",y="price")+geom_smooth(method=lm,col='black')

#price Vs depth
ggplot(diamond,aes(x=price,y=depth))+
  geom_point(col="blue",shape=22)+
  labs(title="price Vs depth",x="price",y="depth")+geom_smooth(method=lm,col='black')

#price Vs table
ggplot(diamond,aes(x=price,y=table))+
  geom_point(col="blue",shape=22)+
  labs(title="price Vs table",x="price",y="table")+geom_smooth(method=lm,col='black')

#price Vs x
ggplot(diamond,aes(x=price,y=x))+
  geom_point(col="blue",shape=22)+
  labs(title="price Vs x",x="price",y="x")+geom_smooth(method=lm,col='black')

#price vs y
ggplot(diamond,aes(x=price,y=y))+
  geom_point(col="blue",shape=22)+
  labs(title="price Vs y",x="price",y="y")+geom_smooth(method=lm,col='black')

#price vs z
ggplot(diamond,aes(x=price,y=z))+
  geom_point(col="blue",shape=22)+
  labs(title="price Vs z",x="price",y="z")+geom_smooth(method=lm,col='black')

#dropping the outliers by setting working range
diamond = diamond[diamond$depth<75 & diamond$depth>45,]
diamond = diamond[diamond$table<80 & diamond$depth>40,]
diamond = diamond[diamond$x<30 & diamond$x>0,]
diamond = diamond[diamond$y<30 & diamond$y>0,]
diamond = diamond[diamond$z<30 & diamond$z>2,]
  

#DISTRIBUITION OF PRICE ON CATEGORICAL VARIABLES
#price vs cut
boxplot(price~cut, data=diamond, 
        col="blue",
        main="Price by Cut", xlab="Cut")

#price vs color
boxplot(price~color, data=diamond, 
        col="blue",
        main="Price by Color", xlab="Color")

#price vs clarity
boxplot(price~clarity, data=diamond, 
        col="blue",
        main="Price by Clarity", xlab="Clarity")

#clarity vs cut
ggplot(diamond,aes(x=clarity))+
  geom_bar(col="white",aes(fill=cut))

#cut vs clarity 
ggplot(diamond,aes(x=cut))+
  geom_bar(col="white",aes(fill=clarity))

#color vs clarity
ggplot(diamond,aes(x=color))+
  geom_bar(col="white",aes(fill=clarity))




