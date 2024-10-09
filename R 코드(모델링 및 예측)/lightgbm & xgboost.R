base_df <- read.csv('전처리_데이터_peradd.csv',header = T, fileEncoding = "euc-kr", stringsAsFactors = T)

str(base_df)
head(base_df, 3)
# id 변수 삭제
colnames(base_df)
colnames(base_df)[c(1,2,11,12,13,14,15,18)]
base_df <- base_df[,-c(1,2,11,12,13,14,15,18)]
str(base_df)

#type 더미데이터 만들기
base_df$type_general <- ifelse(base_df$type == '일반', 1, 0)
base_df$type_DT <- ifelse(base_df$type == 'DT', 1, 0)
base_df$type_R <- ifelse(base_df$type == 'R', 1, 0)
base_df <- base_df[,-1]
str(base_df)

# 종속변수 탐구
summary(base_df$pop_per)
dim(base_df)
hist(base_df$pop_per, breaks=40, freq=TRUE, main='Histogram of pop_per')

# 분위수 계산
quantiles <- quantile(base_df$pop_per, probs = c(0, 1/3, 2/3, 1))

# 분위수별로 데이터를 백분위수 카테고리로 만들기
pop <- cut(base_df$pop_per, breaks = quantiles, 
           labels = c("Low", "Medium", "High"), include.lowest = TRUE)
table(pop)
# 자른거
abline(v = quantiles, col = "red", lty = "dashed")
boxplot(base_df$pop_per, horizontal=T, main='Boxplot of pop_per')


# 종속변수 전처리후 통합하기
base_c <- cbind(base_df, pop)
colnames(base_c)
base_c <- base_c[,-10]
table(base_c$pop)
str(base_c)


# 필요한 라이브러리 로드
library(xgboost)  # xgboost 패키지 로드
library(lightgbm)  # lightgbm 패키지 로드
library(caret)  # caret 패키지 로드


# 데이터 준비
base_cc <- base_c
base_cc$pop <- as.integer(base_c$pop)-1
str(base_cc)

set.seed(111)  # 재현성을 위해 시드 설정
lgb_data <- lgb.Dataset(data = as.matrix(base_cc[, -which(colnames(base_cc)=='pop')]),
                              label = base_cc[, which(colnames(base_cc)=='pop')])

params <- list(
  objective = "multiclass",
  metric = "multi_error",
  boosting_type = "gbdt",
  num_leaves = 31,
  learning_rate = 0.01,
  nthread = 4,
  num_class = 3,
  return_cvbooster = TRUE
)

cv_results <- lgb.cv(params = params,
                     data = lgb_data,
                     nfold = 5,
                     stratified = TRUE)

record_evals <- cv_results$record_evals
best_iter <- cv_results$best_iter
best_params <- record_evals[[best_iter]]$params
print(best_params)
 
# 최적 성능 지표와 해당 반복 횟수 출력
print(paste("Best Iteration:", best_iter))
print(paste("Best Score:", best_score))

# 최적 반복 횟수에서의 모델 가져오기
best_model <- boosters[[best_iter - 1]]
best_model <- boosters[[best_iter]]  # 최적 반복 횟수에 해당하는 모델 가져오기

# 최적 모델 사용 예측
prediction <- lgb.predict(best_model, data = lgb_data)
print(prediction)




prediction <- lgb.predict(best_model, data = lgb_data)
print(prediction)


# 최적 반복 횟수의 결과에 접근합니다.
cvbooster <- cv_results$boosters
best_results <- lgb.predict(cvbooster, data = lgb_data, num_iteration = best_iter)
print(best_results)


# Train the booster using the best parameters
booster <- lgb.train(params = params, data = lgb_data)

# Extract feature importance
importance <- lgb.importance(booster, percentage = TRUE)
print(importance)



################################################################################## # xgboost
rm(list=ls())
gc()
base_df <- read.csv('전처리_데이터_peradd.csv',header = T, fileEncoding = "euc-kr", stringsAsFactors = T)

str(base_df)
head(base_df, 3)
# id 변수 삭제
colnames(base_df)
colnames(base_df)[c(1,2,11,12,13,14,15,18)]
base_df <- base_df[,-c(1,2,11,12,13,14,15,18)]
str(base_df)

#type 더미데이터 만들기
base_df$type_general <- ifelse(base_df$type == '일반', 1, 0)
base_df$type_DT <- ifelse(base_df$type == 'DT', 1, 0)
base_df$type_R <- ifelse(base_df$type == 'R', 1, 0)
base_df <- base_df[,-1]
str(base_df)


library(corrplot)
cor_df = cor(base_df)
col <- colorRampPalette(c('#BB4444', '#EE9988', '#FFFFFF', '#77AADD', '#4477AA'))
corrplot(cor_df,
         method='color', # 시각화 방법
         type='lower',  # 아래만
         diag=F, # 대각선 포함 여부
         tl.srt= 45, # 각도
         tl.ces = 0.7,
         addCoef.col="black" # 상관관계 값의 텍스트 라벨 색상 지정
)



# 종속변수 탐구
summary(base_df$pop_per)
dim(base_df)
hist(base_df$pop_per, breaks=40, freq=TRUE)

# 분위수 계산
quantiles <- quantile(base_df$pop_per, probs = c(0, 1/3, 2/3, 1))

# 분위수별로 데이터를 백분위수 카테고리로 만들기
pop <- cut(base_df$pop_per, breaks = quantiles, 
           labels = c("Low", "Medium", "High"), include.lowest = TRUE)

# # 백분위수 (Percentiles)
# pop_c <- cut(data, breaks = quantiles, labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE)
# 
# pop=cut(base_df$pop_per, breaks=c(-65.3131, -5.7805, 1.4833, 7.6868, 46),right=FALSE,
#         labels=c("1","2","3","4"),include.lowest=TRUE)

pop
hist(base_df$pop_per, breaks=40, freq=TRUE)
# 자른거
abline(v = quantiles, col = "red", lty = "dashed")


# 종속변수 전처리후 통합하기
base_c <- cbind(base_df, pop)
colnames(base_c)
base_c <- base_c[,-10]
table(base_c$pop)
str(base_c)

library(xgboost)

# 데이터 준비
str(base_c)
base_cc <- base_c
base_cc$pop <- as.integer(base_c$pop)-1
str(base_cc)
x <- as.matrix(base_cc[, -which(colnames(base_cc) == 'pop')])
y <- base_cc$pop
dim(x)
length(y)
dtrain <- xgb.DMatrix(data = x, label = y)
dim(dtrain)


# 매개변수 설정
params <- list(
  objective = "multi:softmax",
  eval_metric = "merror",
  num_class = 3,
  max_depth = 6,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# 교차 검증 수행
set.seed(111)
cv_results <- xgb.cv(
  params = params,
  data = dtrain,
  nfold = 5,
  nrounds = 100,
  stratified = TRUE,
  early_stopping_rounds = 10,
  maximize = FALSE,
  verbose = 0
)

# 교차 검증 결과 출력
print(cv_results)

# 최적의 반복(iteration) 확인
best_iteration <- cv_results$best_iteration
best_iteration

# 최적 모델 학습
model <- xgb.train(params = params, data = dtrain, nrounds = best_iteration)

# 변수 중요도 추정
importance <- xgb.importance(model=model)
importance$Feature <- iconv(importance$Feature, from = "UTF-8", to = "euc-kr")
print(importance, quote = FALSE)



# 새로운 데이터 준비
new_data <- read.csv('uos_data_set_fin.csv',header = T, fileEncoding = "euc-kr", stringsAsFactors = T)
View(new_data)
str(new_data[,-1])

# 새로운 데이터에 대한 DMatrix 생성
dtest <- xgb.DMatrix(data = as.matrix(new_data[,-1]))
pred <- predict(model, dtest)
base_cc$pop
pred


