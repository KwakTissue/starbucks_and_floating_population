rm(list = ls())
par(family="AppleGothic") #한글깨짐 해결(mac)
options(scipen=100) 

base_df <- read.csv('전처리_데이터_peradd.csv',header = T, fileEncoding = "euc-kr", stringsAsFactors = T)

head(base_df)
base_df <- base_df[,-c(1,2,11,12,13,14,15,18)]

summary(base_df$pop_per)
hist(base_df$pop_per, breaks=40, freq=TRUE)
#type 더미데이터 만들기
base_df$type_general <- ifelse(base_df$type == '일반', 1, 0)
base_df$type_DT <- ifelse(base_df$type == 'DT', 1, 0)
base_df$type_R <- ifelse(base_df$type == 'R', 1, 0)
base_df <- base_df[,-1]

# 분위수 계산
quantiles <- quantile(base_df$pop_per, probs = seq(0, 1, by=1/3))
pop <- cut(base_df$pop_per, breaks = quantiles, 
           labels = c("Low", "Medium", "High"), include.lowest = TRUE)
# 이진분할
#pop <- cut(base_df$pop_per, breaks = c(-66, 0, 46), labels = c("Low", "High"), include.lowest = TRUE)

base_c <- cbind(base_df, pop)

base_c <- base_c[,-10]

table(base_c$pop)

#데이터 셋 나누기
set.seed(111) 
train_index <- sample(1:nrow(base_c), 0.7*nrow(base_c))
train_data <- base_c[train_index,]
test_data <- base_c[-train_index,]
dim(train_data)
dim(test_data)
table(train_data$pop)

head(test_data)

levels(train_data$pop)

train_data$pop <- relevel(train_data$pop, 'Medium')

library(nnet)
mlogit <- multinom(pop ~., data=train_data)
summary(mlogit)

predicted <- predict(mlogit, test_data, type = "class")

t <- table(predicted, test_data$pop)
t[c(2,1),] <- t[c(1,2),]
t

sum(table(predicted, test_data$pop))-sum(diag(table(predicted, test_data$pop))))/sum(table(predicted, test_data$pop)



