library(gmodels)
library(e1071)
library(dplyr)
library(ggplot2)

setwd("C:/source")
mushroom <- read.csv('mushrooms.csv')

names(mushroom)
str(mushroom)
dim(mushroom)

colSums(is.na(mushroom))

name <- names(mushroom)
for(i in 1:length(name)){
  print(name[i])
  print(table(mushroom[name[i]]))
}

mushroom %>%
  ggplot(aes(type)) +
  geom_bar()

# cap.color
mushroom %>%
  group_by(type) %>%
  ggplot(aes(cap_color, fill = type)) +
  geom_bar(position = "dodge")

# gill.color
mushroom %>%
  group_by(type) %>%
  ggplot(aes(gill_color, fill = type)) +
  geom_bar(position = "dodge")
colSums(is.na(mushroom))

# spore.print.color
mushroom %>%
  group_by(type) %>%
  ggplot(aes(spore_print_color, fill = type)) +
  geom_bar(position = "dodge")

# odor
mushroom %>%
  group_by(type) %>%
  ggplot(aes(odor, fill = type)) +
  geom_bar(position = "dodge")

#테스트용 8123 제외
mush_test <- mushroom[8123,]
mush_test
write.csv(mush_test,"mush_test.csv",row.names=FALSE)
nrow(mushroom)
mushroom <- mushroom[-8123,]
nrow(mushroom)

#8123행 

train <- mushroom[1:6500,-1]
test <- mushroom[6501:8123,-1]
train_labels <- mushroom[1:6500,1]
test_labels <- mushroom[6501:8123,1]

# 모델/분류기 생성
mushroom_classifier <- naiveBayes(train, train_labels, laplace = 1)
# test 데이터 예측
m_pred <- predict(mushroom_classifier, test)
# Cross Table 확인 및 정답률 확인
CrossTable(m_pred, test_labels)
print(sum(m_pred==test_labels)*100/length(m_pred))

n <- nrow(mushroom) # 행 개수
result <- c(0,0,0,0,0,0,0,0,0,0) # 빈칸
for(i in 1:10){
  x <- round(n*i/10)
  train <- mushroom[1:x,-1]
  test <- mushroom[x:n,-1]
  train_labels <- mushroom[1:x,1]
  test_labels <- mushroom[x:n,1]
  
  mushroom_classifier <- naiveBayes(train, train_labels, laplace = 1)
  m_pred <- predict(mushroom_classifier, test)
  result[i] <- sum(m_pred==test_labels)*100/length(m_pred)
}

# 결과물
result

result2 <- predict(mushroom_classifier,mush_test[,-1]) #8123 데이터 예측
result2#예측값
mush_test[,1]#실제값
