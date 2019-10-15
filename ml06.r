rm(list = ls())
# 선형 회귀(Linear Regression)
# 종속 변수 하나와 독립(설명) 변수들 간의 관계를 수식으로 설명/예측하는 방법
# y = a + bx(a, b는 상수)
# x: 독립(설명) 변수, y: 종속 변수

# 데이터 준비
heights <- read.csv('mlwr/heights.csv')
head(heights)

# 아들의 키가 아버지의 키에 영향을 받는가? (유전)
# son: 종속 변수(y축), father: 독립 변수(x축)

# 아버지 키(father)의 분포 
summary(heights)
hist(heights$father)
hist(heights$son)
boxplot(heights$father)

# 산점도 그래프(scatter plot)
plot(heights, col = rgb(0.7,0.2,0.7,0.9))
#rgb( red, green, blue, 불투명도)

abline(h = mean(heights$son)) #h = horizontal 가로 보조선
#abline(): 보조선, h : 수평보조선, v : 수직 보조선
abline(v = mean(heights$father),lty = 2)
#lty : 점선?, 그래프 위에 덮어쓰기가 안되므로 초기화 해주어야 함

#lm() 함수 : linear regression model(선형 회귀 모델)
lm_heights <- lm(formula = son ~ father, data = heights) # formula = 종속변수 ~ 독립변수
lm_heights

summary(lm_heights)

# 선형 모델에서 찾은 coefficient(계수)들을 이용해서
# 선형 모델 그래프를 추가
abline(a = 86.10257, b = 0.51391)

# ggplot2를 이용한 그래프
library(ggplot2)
ggplot(data = heights, mapping = aes(x = father, y = son)) +
  geom_point(color = rgb(0.7,0.2,0.5,0.5))+
  geom_hline(yintercept = mean(heights$son),
             linetype = 'dashed', color = 'darkblue')+
  geom_vline(xintercept = mean(heights$father),
             linetype = 'dashed', color = 'darkblue')+
            stat_smooth(method = 'lm')  # 흐린 부분들은 잔차들의 오차범위

# 선형 회귀 모델식 : y = a + bx
# a = mean(y) - b * mean(x)
# 선형 회귀 모델식은 점 (x의평균,y의평균)을 지나가는 점이다. 
# b = Cov(x,y) / Var(x)
m_x <- mean(heights$father) # 아버지 키의 평균
m_y <- mean(heights$son)    # 아들 키의 평균
cov_xy <- cov(heights$father, heights$son) # x와 y의 공분산(covariance)
var_x <- var(heights$father) # 분산(variance)
b <- cov_xy/ var_x
a <- m_y-b*m_x

#Pearson's correlation Coefficient( 상관계수 )
cor(heights$father, heights$son)

# 피어슨 상관 계수의 절대값이 1에 가까울수록 상관관계가 높고,
# -1 <= Cor =<= 1, 0에 가까울수록 상관관계가 없다.

# 챌린저호의 사고 조사 데이터!!
launch <- read.csv(file = 'mlwr/challenger.csv')
str(launch)
head(launch)
summary(launch)

# 단순 선형 회귀(distress_ct ~ temperature)
plot(x = launch$temperature, y = launch$distress_ct)

lm_launch <- lm(formula=distress_ct ~ temperature, data = launch )
summary(lm_launch)

a <- lm_launch$coefficients[1] # 선형모델의 y절편
a
b <- lm_launch$coefficients[2] # 선형 모델의 기울기
b

abline(a= a, b= b, col= 'blue')

# 다중선형 회귀(multiple linear regression
# y ~ x1 + x2 + x3 + ...
str(launch)
lm_launch <- lm(formula = distress_ct ~ ., data= launch )
summary(lm_launch)
