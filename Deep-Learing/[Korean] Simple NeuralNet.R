#코드의 설명영상  https://www.youtube.com/watch?v=Ixkw32Ajl2c&t=17s

getwd()  #현재 경로 확인
setwd("C:/Users/leeyua/Desktop/에이림_R") #경로 재설정
autoparts <-read.csv("공정기록데이터.csv",header = TRUE) #데이터 읽어 들이기

autoparts1 <- autoparts[autoparts$prod_no == "90784-76001",c(2:11)] # 데이터에서 원하는 범주를 따로 저장.  
#plot(autoparts1$c_thickness)
#boxplot(autoparts1$c_thickness)

autoparts2<-autoparts1[autoparts1$c_thickness<1000,]   # 데이터 범주 중 1000이하인 값 만을 저장
plot(autoparts2$c_thickness)                           # c_thickness 라는 변수의 시각화
boxplot(autoparts2$c_thickness)

autoparts2$g_class <- as.factor(ifelse(autoparts2$c_thickness <20,1,ifelse(autoparts2$c_thickness < 32,2,3)))
#as.factor로 변환하여 ifelse를 사용하여 0~19는 라벨1, 20~31는 라벨2, 32이상은 라벨3 으로 g_class변수에 새롭게 저장.

t_index <- sample(1:nrow(autoparts2),size=nrow(autoparts2)*0.7) #t_index에 데이터의 70%를 랜덤하게 분포하여 저장.
train<-autoparts2[t_index, ]                                    # t_index에 저장된 값들을 train데이터에 저장
test <- autoparts2[-t_index, ]                                  # t_index가 아닌 값들인 -t_index를 test에 저장

library(nnet) #nnet 라이브러리 
m15<- nnet(g_class ~ fix_time + a_speed + b_speed + separation+ 
             s_separation + rate_terms + mpa + load_time + highpressure_time, data = train,size = 15)
# nnet() 종속변수 ~ 독립변수 1 + 독립변수2 + ... , 데이터= 데이터 이름, 노드 갯수 = 숫자)

print(m15)  # 설계된 모델의 상세정보 확인

yhat_test <- predict(m15, test,type = "class") #설계된 모델을 활용하여 예측한 후 yhat_test로 저장
table<-table(real1=test$g_class,predict1=yhat_test) # 교차 테이블 생성하기
table #테이블 확인
(table[1,1]+ table[2,2] + table[3,3]) / sum(table) # 모델의 현재 정확도를 계산해주기


#위의 코드가 실행되지 않고 오류를 낼때
as.factor(yhat_test) #맨 마지막 levels 이 1,2 만 나오면 아래 코드를 실행 

x1<- c(0,0,0) # 새로운 항목 생성
table<- cbind(x1,table) # cbind()활용하여 table에 x1 항목 추가
table
(table[1,1]+ table[2,2] + table[3,3]) / sum(table) 

#인공신경망 시각화 
library(reshape2)
library(devtools)
source_url("https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r")
plot(m15)

# 새로운 데이터를 넣어서 이 데이터의 결과가 불량품인지 정상제품인지 확인
# 숫자는 수정가능
new.data<- data.frame(fix_time=c(87,85.6), a_speed=c(0.609,0.472),b_speed=c(1.715,1.685),separation=c(242.7,243.4) ,s_separation=c(657.5,657.9),rate_terms=c(95,95),mpa=c(88,28.8),load_time=c(18.1,18.2),highpressure_time=c(10,60))

#새롭게 만든 데이터 프레임 수치가 정상인제 비정상인지 확인 # 2 면은 정상 1,3이면 불량!!
predict(m15, newdata = new.data,type = "class")

