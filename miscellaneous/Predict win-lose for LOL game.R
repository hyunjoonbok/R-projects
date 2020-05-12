## Predict win-lose based on decision tree

require(tidyverse)
require(reshape2)
require(tree)

set.seed(0623) # 나중에 재현을 위한 seed값 설정

base_dir <- "C:/Users/bokhy/Desktop/R/Predict win-lose for LOL Game" #새로운 폴더 경로 및 이름 저장
directory <- dir.create(base_dir) 
setwd(base_dir)

lol<- read_csv("lol_mvp.csv")
glimpse(lol)

#결측치를 찾기 위한 코드
lol[!complete.cases(lol),]
#결측치 확인을 위한 예제
example_data<-read.csv("결측_공정기록데이터.csv") # 이 데이터에는 결측값이 하나가 있습니다.
na.fail(example_dat)
example_data[!complete.cases(example_data),] 


#첫번째,아홉번째 열 삭제 
lol<-lol[,-c(1,9)];str(lol) # ;다음 코드는 아랫줄에서 코드를 실행하는 효과 

#시각화
plot(lol$Win)
boxplot(lol$Win)

# 데이터 분할
lol_index<-sample(1:nrow(lol),size=nrow(lol)*0.7)

train<-lol[lol_index,]
test<-lol[-lol_index,]
# 데이터의 수와 변수 숫자 확인
dim(train)
dim(test)
head(train)

attach(lol) # 데이터 고정시키기 위한 함수
#모델 설계 시작
win_tree<-tree(factor(Win) ~  Maximum.continuous.kills + Damage.to.the.champion + Damage.to.turret + Healing.amount + Damage.done + Installed.ward + Destroyed.ward + Purchase.control.ward + Number.of.minion.treats + Neutral.monster.treatment + First.Kill + First.turret + Mini.Baron + Baron +ACE +MVP , data = train)
tree
win_tree #완성된 모델 확인
plot(win_tree) # 모델 시각화
text(win_tree,cex=0.8) # cex는 글자 크기 설정

?prune.tree
prune.win_tree<-prune.tree(win_tree,method = "misclass") # 가지치기를 위한 코드
prune.win_tree
plot(prune.win_tree) # 가지치기한 결과값을 시각화 해서 보여줍니다.

prune.win_tree<-prune.tree(win_tree,best=13) # 가짓수 설정해서 모델 모델 재설계
plot(prune.win_tree) # 시각화
text(prune.win_tree)

?predict
lol.yhat_test <- predict(prune.win_tree,test,type="class") # 만들어진 모델에 대한 예측
#성능 측정을 위한 cross table 생성
cross_table <-table(real=test$Win, predict=lol.yhat_test); cross_table
win_tree_acc<-(cross_table[1,1]+cross_table[2,2])/ sum(cross_table);win_tree_acc
win_tree_acc

###cv.tree를 활용한 가지치기 
win_tree_2<-tree(factor(Win) ~  Maximum.continuous.kills + Damage.to.the.champion + Damage.to.turret + Healing.amount + Damage.done + Installed.ward + Destroyed.ward + Purchase.control.ward + Number.of.minion.treats + Neutral.monster.treatment + First.Kill + First.turret + Mini.Baron + Baron + MVP+ACE, data = train)
?cv.tree()
cv.tr<-cv.tree(win_tree_2,FUN =prune.misclass)
plot(cv.tr)
cv.tr

#어떻게 가지를 쳤는지 확인 후 모델 재설
prune.win_tree_2<-prune.misclass(win_tree_2,best = 13)
#시각화 
plot(prune.win_tree_2)
text(prune.win_tree)
#성능테스트
pred_win<-predict(prune.win_tree_2,test,type = "class")
cross_table2<-table(real=test$Win,predice=pred_win);cross_table2
win_tree_2_acc<-(cross_table2[1,1]+cross_table2[2,2])/sum(cross_table2)



#우리가 설계한 모델의 성능을 최종적으로 측정하는데 사용되는 평가방법.
install.packages("Epi")
library(Epi)
ROC(test = pred_win,stat = test$Win, plot = "ROC", AUC = T,main="TREE")
#최종 그래프
plot(prune.win_tree)
text(prune.win_tree)


## 데이터 결과 예측  
new.one.data<-data.frame( Maximum.continuous.kills =2, Damage.to.the.champion =10.3, Damage.to.turret =1.2, Healing.amount =18.5, Damage.done =15.8, Installed.ward =35, Destroyed.ward =7, Purchase.control.ward =5, Number.of.minion.treats =10, Neutral.monster.treatment =0, First.Kill =0, First.turret =0, Mini.Baron =0, Baron =0,MVP=0,ACE=0)

# 우리가 만든 모델로 새로운 데이터를 넣었을때, 그 결과 예측  
predict(win_tree_2,newdata = new.one.data,type = "class")


# 랜덤포레스트 실시
install.packages("randomForest")
library(randomForest)
#모델 설계는 의사결정과 비슷, 단 몇 가지의 옵션값 추가 설정가능
m_rf<-randomForest(Win~ Maximum.continuous.kills + Damage.to.the.champion + Damage.to.turret + Healing.amount + Damage.done + Installed.ward + Destroyed.ward + Purchase.control.ward + Number.of.minion.treats + Neutral.monster.treatment + First.Kill + First.turret + Mini.Baron + Baron +MVP+ACE, data = lol,mtry=9,ntree=500,subset = lol_index,importance=T)
#ntree = 중요한 변수의 수 설정
#mtry = 나무의 숫자 설정
#importance = 중요한 변수 출력

#결과 출력 및 시각화
importance(m_rf)
varImpPlot(m_rf)

#ntree를 몇개를 하면 좋을지 알려주는 plot()
plot(m_rf)
