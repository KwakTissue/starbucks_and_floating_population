
base_df <- read.csv('전처리_데이터_peradd.csv',header = T, fileEncoding = "euc-kr", stringsAsFactors = T)

str(base_df)
head(base_df, 3)
# id 변수 삭제
colnames(base_df)[c(1,2,13,14,15)]
base_df <- base_df[,-c(1,2,13,14,15)]
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
base_c <- base_c[,-13]
table(base_c$pop)



# 필요한 라이브러리 로드
library(xgboost)
library(caret)

# 데이터 준비
str(base_c)
x <- base_c[, -which(colnames(base_c)=='pop')]  # 독립 변수
y <- base_c$pop  # 종속 변수

# 데이터 분할 (예시로 70%는 훈련, 30%는 테스트 데이터로 분할)
set.seed(111)
train_indices <- createDataPartition(y, p = 0.7, list = FALSE)
train_x <- x[train_indices, ]
train_y <- y[train_indices]
test_x <- x[-train_indices, ]
test_y <- y[-train_indices]

table(test_y)


# 하이퍼파라미터 그리드 설정
param_grid <- expand.grid(
  nrounds = c(100, 150, 200),           # 트리의 개수
  max_depth = c(3, 6, 9),               # 트리의 깊이
  eta = c(0.01),             # 학습률
  gamma = c(0, 0.1, 0.2),                # 트리의 잎 노드에서 추가적인 분할을 수행할 때 필요한 최소 손실 감소량
  colsample_bytree = c(0.8, 0.9, 1),    # 트리에 사용될 변수 샘플링 비율
  min_child_weight = c(1, 3, 5),         # 트리의 잎 노드에서 추가적인 분할을 수행하기 위해 필요한 최소 가중치 합
  subsample = c(0.7, 0.8)            # 훈련 데이터 샘플링 비율
)
param_grid

xgb_model <- train(
  x = as.matrix(train_x),
  y = train_y,
  method = "xgbTree",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = param_grid,
  verbose = FALSE,
  metric = "Accuracy",
)

# 최적의 파라미터 조합 및 성능 결과 출력
print(xgb_model$bestTune)
print(xgb_model$results)


final_xgb_model <- xgboost(
  data = as.matrix(train_x),
  label = train_y,
  nrounds = xgb_model$bestTune$nrounds,
  max_depth = xgb_model$bestTune$max_depth,
  eta = xgb_model$bestTune$eta,
  gamma = xgb_model$bestTune$gamma,
  colsample_bytree = xgb_model$bestTune$colsample_bytree,
  min_child_weight = xgb_model$bestTune$min_child_weight,
  subsample = xgb_model$bestTune$subsample,
  verbose = 0
)

# 테스트 데이터에 대한 예측
predictions <- predict(final_xgb_model, as.matrix(test_x), type = "response")

str(test_x)
summary(predictions)
summary(train_y)
summary(test_y)
