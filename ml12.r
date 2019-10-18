rm(list = ls())

#Clustering (군집화)

# 데이터 준비
teens <- read.csv(file='mlwr/snsdata.csv')

# 데이터 확인
str(teens)
head(teens,n = 20)

# 몇가지 변수(특징)에서 결측치(NA)가 보임!
summary(teens)
# gender 변수의 NA 갯수 
table(is.na(teens$gender))

table(teens$gender, useNA = 'ifany')

# female 변수를 데이터프레임에 추가
# 성별이 'F'이고, NA가 아니면 1, 그렇지 않으면 0을 입력 - 더미코딩
teens$female <- ifelse(teens$gender == 'F' & !is.na(teens$gender),1,0)
table(teens$female, useNA = 'ifany')

# nogender 변수를 데이터프레임에 추가
# gender 변수가 NA이면 1, 그렇지 않으면 0을 입력
teens$nogender <- ifelse(is.na(teens$gender),1,0)
table(teens$nogender)

# age 변수 확인
summary(teens$age)

# age의 정상 범위는 13 ~ 19라고 가정 -> 이외의 값들은 NA 
teens$age <- ifelse(teens$age >= 13 & teens$age <= 19, teens$age, NA)

summary(teens$age)
# age의 NA 값들을 gradeyear별 age의 평균값으로 대체
# dplyr 패키지 이용
library(dplyr)
teens %>% 
  group_by(gradyear) %>% 
  summarise(mean(age), na.rm = T)

# 그룹별 평균( 또는 임의함수 )를 적용해서 벡터를 리턴하는 함수
# stats::ave(평균을 계산할 벡터, 그룹핑 변수, FUN = mean)
df <- data.frame(class = c(1, 1, 1, 2, 2),
                 score = c(10, 9, NA, 9, 8))
df
my_mean <- function(x){
  mean(x, na.rm = T)
}
ave(df$score, df$class, FUN = mean)
ave(df$score, df$class, FUN = my_mean)

ave_age <- ave(teens$age, teens$gradyear, FUN = my_mean)
ave_age
tail(ave_age)
teens$age <- ifelse(is.na(teens$age),ave_age,teens$age)
summary(teens$age)

# k- 평균 군집화 알고리즘 모델을 생성
str(teens)
# 개인 식별 정보 (gradyear, gender, age, friends)를 제외하고,
# 오로지 관심사들로만 clustering을 시도
interests <- teens[5:40]

#정규화
normalize<- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

interests_n<-as.data.frame(lapply(interests,normalize))



str(interests_n)
set.seed(2345)

teen_clusters_n <- kmeans(interests_n, 5)
str(teen_clusters_n)
str(teen_clusters_n$cluster)
table(teen_clusters_n$cluster)

# 모델이 분류한 클러스터가 어떤 특징들을 갖고 있을까?
teens$cluster_n <- teen_clusters_n$cluster
teens[1:10, c('cluster_n', 'gender', 'age', 'friends')]
teen_clusters_n$centers
