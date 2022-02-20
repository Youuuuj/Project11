rm(list = ls())

getwd()
setwd('C:\\Users\\You\\Desktop\\빅데이터\\빅데이터 수업자료\\R\\사례연구\\사례연구 11')

library(dplyr)
library(caret)

# 데이터
train_air <- read.csv('train.csv')
test_air <- read.csv('test.csv')

head(train_air)
str(train_air)
str(test_air)

# 훈련용 데이터 
# id 제거
train_air <- train_air[,-c(1,2,4)]

# 고객 타입 변경
train_air$Customer.Type[train_air$Customer.Type == 'disloyal Customer'] <- 0
train_air$Customer.Type[train_air$Customer.Type == 'Loyal Customer'] <- 1

# Type.of.Travel 변경
train_air$Type.of.Travel[train_air$Type.of.Travel == 'Business travel'] <- 0
train_air$Type.of.Travel[train_air$Type.of.Travel == 'Personal Travel'] <- 1

train_air$Flight.Distance[train_air$Flight > 0 & train_air$Flight <= 1200] <- 1 
train_air$Flight.Distance[train_air$Flight > 1200 & train_air$Flight <= 2400] <- 2 
train_air$Flight.Distance[train_air$Flight > 2400 & train_air$Flight <= 3600] <- 3 
train_air$Flight.Distance[train_air$Flight > 3600 & train_air$Flight <= 4800] <- 4 
train_air$Flight.Distance[train_air$Flight > 4800] <- 5

train_air$target <- as.factor(train_air$target)


# 여행 타입별 분류
train_air %>% filter(Type.of.Travel == '0') -> Business_tr
train_air %>% filter(Type.of.Travel == '1') -> Personal_tr


# 클래스별 분류
Business_tr %>% filter(Class == 'Eco') -> Bu_eco_tr
Business_tr %>% filter(Class == 'Business') -> Bu_bu_tr
Business_tr %>% filter(Class == 'Eco Plus') -> Bu_ep_tr

str(Bu_eco_tr)

Bu_eco_tr <- Bu_eco_tr[,-c(2,3)]
Bu_bu_tr <- Bu_bu_tr[,-c(2,3)]
Bu_ep_tr <- Bu_ep_tr[,-c(2,3)]

Personal_tr %>% filter(Class == 'Eco') -> Pe_eco_tr
Personal_tr %>% filter(Class == 'Business') -> Pe_bu_tr
Personal_tr %>% filter(Class == 'Eco Plus') -> Pe_ep_tr

str(Pe_eco_tr)

Pe_eco_tr <- Pe_eco_tr[,-c(1:3)]
Pe_bu_tr <- Pe_bu_tr[,-c(1:3)]
Pe_ep_tr <- Pe_ep_tr[,-c(1:3)]




# 검증용 데이터 
# id 제거
test_air <- test_air[,-c(1,2,4)]

# Customer.Type 변경
test_air$Customer.Type[test_air$Customer.Type == 'disloyal Customer'] <- 0
test_air$Customer.Type[test_air$Customer.Type == 'Loyal Customer'] <- 1

# Type.of.Travel 변경
test_air$Type.of.Travel[test_air$Type.of.Travel == 'Business travel'] <- 0
test_air$Type.of.Travel[test_air$Type.of.Travel == 'Personal Travel'] <- 1

# Flight.Distance 변경
test_air$Flight.Distance[test_air$Flight > 0 & test_air$Flight <= 1200] <- 1 
test_air$Flight.Distance[test_air$Flight > 1200 & test_air$Flight <= 2400] <- 2 
test_air$Flight.Distance[test_air$Flight > 2400 & test_air$Flight <= 3600] <- 3 
test_air$Flight.Distance[test_air$Flight > 3600 & test_air$Flight <= 4800] <- 4 
test_air$Flight.Distance[test_air$Flight > 4800] <- 5

str(test_air)

# 여행 타입별 분류
test_air %>% filter(Type.of.Travel == '0') -> Business_te
test_air %>% filter(Type.of.Travel == '1') -> Personal_te


# 클래스별 분류
Business_te %>% filter(Class == 'Eco') -> Bu_eco_te
Business_te %>% filter(Class == 'Business') -> Bu_bu_te
Business_te %>% filter(Class == 'Eco Plus') -> Bu_ep_te

str(Bu_eco_te)

Bu_eco_te <- Bu_eco_te[,-c(2,3)]
Bu_bu_te <- Bu_bu_te[,-c(2,3)]
Bu_ep_te <- Bu_ep_te[,-c(2,3)]

Personal_te %>% filter(Class == 'Eco') -> Pe_eco_te
Personal_te %>% filter(Class == 'Business') -> Pe_bu_te
Personal_te %>% filter(Class == 'Eco Plus') -> Pe_ep_te

str(Pe_eco_te)

Pe_eco_te <- Pe_eco_te[,-c(1:3)]
Pe_bu_te <- Pe_bu_te[,-c(1:3)]
Pe_ep_te <- Pe_ep_te[,-c(1:3)]







# 랜포 - 타입 : 비즈니스
# 비즈니스 - 에코
library(randomForest)
Bu_eco_rf <- randomForest(target ~ .,
                       data=Bu_eco_tr,
                       na.action = na.omit,
                       ntree=100,
                       proximity=T)
Bu_eco_rf

importance(Bu_eco_rf)  # 지니평균감소량이 클수록 중요도가 높은 변수가 된다.
varImpPlot(Bu_eco_rf)  

# 예측 - Seat.comfort의 평균점수 2.88점
mean(Bu_eco_te$Seat.comfort)
Bu_eco_pred <- predict(Bu_eco_rf, Bu_eco_te)
table(Bu_eco_pred)

# 110/364
# 0.3021978

# 예측 - Seat.comfort의 평균점수 4.5점
Bu_eco_te$Seat.comfort <- 4.5 
str(Bu_eco_te)

Bu_eco_pred2 <- predict(Bu_eco_rf, Bu_eco_te)
table(Bu_eco_pred2)

# 144/364
# 0.3956044


# 비즈니스 - 비즈니스 ## 
Bu_bu_rf <- randomForest(target ~ .,
                            data=Bu_bu_tr,
                            na.action = na.omit,
                            ntree=100,
                            proximity=T)
Bu_bu_rf

importance(Bu_bu_rf)  # 지니평균감소량이 클수록 중요도가 높은 변수가 된다.
varImpPlot(Bu_bu_rf)  

# 예측 - Inflight.entertainment의 평균점수 3.8
mean(Bu_bu_te$Inflight.entertainment)

Bu_bu_pred <- predict(Bu_bu_rf, Bu_bu_te)
table(Bu_bu_pred)

# 682 / 932
# 0.7317597

# 예측 - Inflight.entertainment의 평균점수 4.5점
Bu_bu_te$Inflight.entertainment <- 4.5
Bu_bu_pred2 <- predict(Bu_bu_rf, Bu_bu_te)
table(Bu_bu_pred2)

# 781 / 932
# 0.8379828
  



# 비즈니스 - 에플
Bu_ep_rf <- randomForest(target ~ .,
                      data=Bu_ep_tr,
                      na.action = na.omit,
                      ntree=100,
                      proximity=T)
Bu_ep_rf

importance(Bu_ep_rf)  # 지니평균감소량이 클수록 중요도가 높은 변수가 된다.
varImpPlot(Bu_ep_rf)  # 

# 예측 - Seat.comfort 3점
mean(Bu_ep_te$Seat.comfort)

Bu_ep_pred <- predict(Bu_ep_rf, Bu_ep_te)
table(Bu_ep_pred)

# 35/80
# 0.4375

# 예측 - Seat.comfort 4.5점
Bu_ep_te$Seat.comfort <- 4.5

Bu_ep_pred2 <- predict(Bu_ep_rf, Bu_ep_te)
table(Bu_ep_pred2)

# 39/80
# 0.4875


# 랜포 - 타입 : 퍼스널
# 퍼스널 - 에코 ## 
Pe_eco_rf <- randomForest(target ~ .,
                          data=Pe_eco_tr,
                          na.action = na.omit,
                          ntree=100,
                          proximity=T)
Pe_eco_rf

importance(Pe_eco_rf)  # 지니평균감소량이 클수록 중요도가 높은 변수가 된다.
varImpPlot(Pe_eco_rf)  


# 예측 - inflight.entertainment 3.16점
mean(Pe_eco_te$Inflight.entertainment)

Pe_eco_pred <- predict(Pe_eco_rf, Pe_eco_te)
table(Pe_eco_pred)  # test_target 

# 243/497
# 0.4889336

# 예측 - inflight.entertainment 4.5점
Pe_eco_te$Inflight.entertainment <- 4.5

Pe_eco_pred2 <- predict(Pe_eco_rf, Pe_eco_te)
table(Pe_eco_pred2) 

# 303/497
# 0.6096579



# 퍼스널 - 비즈니스
Pe_bu_rf <- randomForest(target ~ .,
                          data=Pe_bu_tr,
                          na.action = na.omit,
                          ntree=100,
                          proximity=T)
Pe_bu_rf

importance(Pe_bu_rf)  # 지니평균감소량이 클수록 중요도가 높은 변수가 된다.
varImpPlot(Pe_bu_rf)  


# 예측 - Leg.room.service 3.25점
mean(Pe_bu_te$Leg.room.service)

Pe_bu_pred <- predict(Pe_bu_rf, Pe_bu_te)
table(Pe_bu_pred)

# 22/44
# 0.5

# 예측 - Leg.room.service 4.5점
Pe_bu_te$Leg.room.service <- 4.5

Pe_bu_pred2 <- predict(Pe_bu_rf, Pe_bu_te)
table(Pe_bu_pred2)

# 25/44
# 0.5681818


# 퍼스널 - 에플
Pe_ep_rf <- randomForest(target ~ .,
                          data=Pe_ep_tr,
                          na.action = na.omit,
                          ntree=100,
                          proximity=T)
Pe_ep_rf

importance(Pe_ep_rf)  # 지니평균감소량이 클수록 중요도가 높은 변수가 된다.
varImpPlot(Pe_ep_rf)  

# 예측 - Inflight.entertainment 3.5점
mean(Pe_ep_te$Inflight.entertainment)

Pe_ep_pred <- predict(Pe_ep_rf, Pe_ep_te)
table(Pe_ep_pred)

# 40/83
# 0.4819277

# 예측 - Inflight.entertainment 4.5점
Pe_ep_te$Inflight.entertainment <- 4.5

Pe_ep_pred2 <- predict(Pe_ep_rf, Pe_ep_te)
table(Pe_ep_pred2)

# 47/83
# 0.5662651
