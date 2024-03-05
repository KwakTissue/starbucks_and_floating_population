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

head(base_c)
table(base_c$pop)

base_c$pop
library(ggplot2)
ggplot(base_c,aes(x=price_dff, colours=pop))+geom_bar(fill="gold", colour="black")
  
#geom_bar(stat="identity",fill="gold",colour="black")



library(corrplot)
corr <- round(cor(base_df),3)
corrplot(corr, method="circle", number.cex = 0.5, number.font = 0.5, type = "lower")
cor_df = cor(base_df)
col <- colorRampPalette(c('#BB4444', '#EE9988', '#FFFFFF', '#77AADD', '#4477AA'))
corrplot(cor_df,
         method='color', # 시각화 방법
         type='lower',  # 아래만
         number.cex = 0.5,
         number.font = 0.5,
         diag=F, # 대각선 포함 여부
         tl.srt= 45, # 각도
         tl.ces = 0.7,
         addCoef.col="black", # 상관관계 값의 텍스트 라벨 색상 지정
         tl.col = "black"
)