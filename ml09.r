rm(list = ls())
# Artificial eural Network(인공지능 신경망)
#f(x) = 2, +1
 curve(expr= 2 * x + 1, from= -5, to= 5)
 # Sigmoid 함수: f(x) = 1/(1+exp(-x))
 curve(expr=1/(1 + exp(-x)), from= -10, to = 10)  # 0 ~ 1
 
 # hypobolic tangent: f(x) = tanh(x)
 curve(expr = tanh(x), from = -5, to =5)  # -1 ~ 1
 
 
 # 콘크리트의 강도 예측
 # 1. 데이터 준비
 concrete <- read.csv(file = 'mlwr/concrete.csv')

head(concrete)
head(concrete_norm)
# 데이터 확인, 전처리
str(concrete)

#정규화(Normalization): 실제값 -> 0~1 사이의 값으로 변환
#표준화(Standardization): z-score 표준화(평균, 표준편차)

normalization <- function(x) {
        return((x - min(x))/(max(x)-min(x)))
}

concrete_norm<- as.data.frame(lapply(concrete,normalization))
summary(concrete_norm)

# 신경망 알고리즘을 적용하기 위한 패키지 : neuralnet
# 오차 역전파(backpropagation)를 사용해서 신경망을 훈련시키는 알고리즘 
install.packages('neuralnet')
library(neuralnet)

#3. 모델 생성, 학습
# 학습 데이터 세트(75%)/ 테스트 데이터 세트(25%)
1030*0.75 #772.5
concrete_train <- concrete_norm[1:773,]  # 훈련 데이터
concrete_test <-  concrete_norm[774:1030,] # 테스트 데이터

summary(concrete_train$strength)

# 신경망 모델 생성 
set.seed(12345) # 수업용 난수 고정
concrete_model  <- neuralnet(formula = strength~ ., 
                             data = concrete_train)

# 생성된 NN을 확인
plot(concrete_model)
str(concrete_model)
# 4. 만들어진 NN을 평가 - 테스트 데이터 세트에 적용 
model_result <- compute(concrete_model, concrete_test[-9])
head(model_result) # 신경망 모델에 의해서 계산된 strength 예측값
summary(model_result)

predict_result <- model_result$net.result # 배열열
str(predict_result)
summary(predict_result)
# 예측 결과와 실제 값의 상관 관계 - 상관 계수
cor(predict_result, concrete_test$strength)  #0.806
concrete_test[255:257,9]

# 오차
MAE <- function (actual, predict){ return(mean(abs(actual-predict))) }
MAE(concrete_test$strength, predict_result)   # 0.093

#모델 향상            hidden node 증가! 
model2 <- neuralnet(formula = strength ~.,
                    data = concrete_train,
                    hidden =2)
plot(model2)
cor(predict_result, concrete_test$strength)
# hidden node = 5
model5 <- neuralnet(formula = strength ~.,
                    data = concrete_train,
                    hidden =5)
plot(model5)


# 성과 측정 node 2
model2_result <- compute(model2, concrete_test[-9])
predict_result2 <- model2_result$net.result
# 예측 결과와 실제 값의 상관 관계 - 상관 계수
cor(predict_result2, concrete_test$strength)  #0.9024
concrete_test[255:257,9]

# 오차
MAE(concrete_test$strength, predict_result2) # 0.0686
compute
# 성과 측정 node 5
model5_result <- compute(model5, concrete_test[-9])
model5
head(model5_result)
predict_result5 <- model5_result$net.result
# 예측 결과와 실제 값의 상관 관계 - 상관 계수
cor(predict_result5, concrete_test$strength)  #0.9280
concrete_test[255:257,9]

# 오차
MAE(concrete_test$strength, predict_result5) # 0.0574
head(concrete_test$strength)

# 역 정규화(정규화 -> 실제값) 함수 작성
max_st<-max(concrete$strength)
min_st<-min(concrete$strength)
max_st
min_st
denormalization <- function(x) {
        max_str <- max(concrete$strength)
        min_str <- min(concrete$strength)
        return((x*(max_str-min_str))+min_str)
}


str(model_result$net.result)
concrete_denorm<- denormalization(model_result$net.result)
concrete_denorm2<- denormalization(model2_result$net.result)
concrete_denorm5<- denormalization(model5_result$net.result)

# 상관계수
concrete_strength <- as.data.frame(concrete$strength)
concrete_strength_test<- concrete_strength[774:1030,]

cor(concrete_abnorm, concrete_strength_test)        #cor1    0.8064
cor(concrete_abnorm2, concrete_strength_test)        #cor2   0.9024
cor(concrete_abnorm5, concrete_strength_test)        #cor5   0.9280
MAE(concrete_strength_test, concrete_abnorm)          #MAE    7.49
MAE(concrete_strength_test, concrete_abnorm2)         #MAE2   5.50
MAE(concrete_strength_test, concrete_abnorm5)         #MAE5   4.60

auctal_predict_df <- data.frame(actual = concrete[774:1030,9],
                                predict1 = concrete_abnorm,
                                predict2 = concrete_abnorm2,
                                predict3 = concrete_abnorm5)

# neuralnet 함수의 파라미터 중에서
# hidden 파라미터는 은닉 노드와 은닉 계층의 갯수를 조정할 수 있고,
# act.fct 파라미터는 활성 함수를 바꿔줄 수 있습니다.
# 두 개의 파라미터를 활용해서 다른 신경망 모델을 만들어 보고,
# 예측 결과가 얼마나 개선되는 지 확인해 보세요.
set.seed(12345)
softsum <- function(x){
        log(1 + exp(x))
}
curve(expr = softsum, from =-5, to = 5)

model <- neuralnet(formula = strength ~ .,
                   data = concrete_train, # 정규화된 데이터
                   hidden = c(5,3),       # c(첫번째 층 노드 갯수, 두번째 층 노드 갯수)
                   act.fct = softsum,
                   stepmax = 1e6)     # 활성함수
plot(model)
correct2_count
