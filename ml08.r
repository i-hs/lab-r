# Regression Tree와 Model Tree
rm(list = ls())
#1. 데이터 준비
wine <- read.csv(file = "mlwr/whitewines.csv")

#2 데이터 확인 전처리
str(wine)
# 4,898 obs.(예시), 12variables(특징) - white wine 데이터
summary(wine) # 기술통계량
# 종속 변수 (quality)의 분포
hist(wine$quality)

#regression tree를 사용하기 위한 패키지
#rpart : recursive partitioning
install.packages('rpart')
library(rpart)

#3 모델 학습
# 학습 데이터 세트(75%)/테스트 데이터 세트(25%)
head(wine)
4898 * 0.75
wine_train<-wine[1:3674,]
wine_test<-wine[3675:4898,]

# 학습 데이터를 rpart 패키지를 사용해서 학습시킴
wine_rpart <- rpart(formula = quality ~ ., data = wine_train)
wine_rpart

summary(wine_rpart)

# rpart (회귀 트리)결과를 시각적으로 보여주는 패키지
install.packages('rpart.plot')
library(rpart.plot)
rpart.plot(x= wine_rpart, digits = 3)
rpart.plot(wine_rpart, digits = 4, fallen.leaves = T)

# 4. 모델 평가 - regression tree가 테스트 데이터를 얼마나 잘 설명?
wine_predict <-predict(wine_rpart, wine_test)
summary(wine_predict)
summary(wine_test$quality)

# 모델 성능 평가 1)
# 상관 계수(correlation coefficient): -1 <= cor <= 1
cor(wine_predict, wine_test$quality) # 0.54

# 모델 성능 평가 2) 
# MAE(Mean Absolute Error): 평균 절대 오차
# 오차(실제값 - 예측값)들의 절대값의 평균 
MAE <- function (actual, predict){
  return(mean(abs(actual-predict)))
}

# 함수 테스트
MAE(actual = c(1,2,3),predict = c(1.1, 1.9, 3.0))

MAE(wine_test$quality, wine_predict) # 값이 작을수록 정확하다 

#5 모델 성능 향상
# 모델 트리(Model Tree):
# Regression Tree(분류) + Regression Modeling(회귀 모델 적용)
# 교재(R을 활용한 러신 머닝 2/e) : RWeka 패키지의 M5P 함수 사용
# Cubist 패키지: 규칙 학습 기반 분류 + M5P 알고리즘 회귀 모델 적용
install.packages("Cubist")
library(Cubist)
#cubist(x = 훈련 데이터, y = 훈련 데이터의 결과)
wine_cubist <- cubist(x = wine_train[-12], y = wine_train$quality) 
# 종속변수를 제외한 데이터 프레임을 넣어야 함, 12번째 컬럼 제외
# (데이터프레임, 훈련 label)
wine_cubist
summary(wine_cubist)

# 모델 트리의 성능 테스트 !!
wine_predict2 <- predict(wine_cubist, wine_test)
summary(wine_predict2)
summary(wine_test$quality)
summary(wine_predict)

# 상관 계수
cor(wine_predict2, wine_test$quality) #0.64

# MAE: 평균 절대 오차
MAE(wine_predict2, wine_test$quality)
