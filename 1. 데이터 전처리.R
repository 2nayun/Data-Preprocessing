#데이터 탐색
iris
head(iris) #데이터 앞부분 확인
tail(iris) #데이터 뒷부분 확인

dim(iris) #데이터 사이즈 확인
summary(iris) #데이터의 각 열의 기본 통계량을 확인

str(iris) #데이터를 구성하는 열 속성 확인
view(iris) #표 형태로 데이터를 확인 가능
plot(iris) #열간의 상관정도를 확인, 분포 확인

plot(iris$Sepal.Length) 
#특정 열(sepal.length)의 데이터를 집중해서 보기
#length에 따라 클러스터링 된 것을 볼 수 있다.

#install.packages("caret")
library(caret)
featureplot(iris[,1:4],iris$Species) #종류별 각 데이터가 어떻게 분포되는지 보여줌.


#-----------------------

#데이터 전처리(데이터 클리닝)
#데이터 분석을 수행하기 전에 데이터에 대한 형식의 변경이나 보완 작업을 수행하는 것.


#결측값 처리: 결측값 삭제(delete) 방법
iris_test <- iris
iris_test[c(5,7,8,20,60,100),1] <-NA
iris_test[c(1,2,3),3] <-NA #임의로 결측값이 있는 데이터를 생성

head(iris_test)

iris_test2 <-iris_test3 <-iris_test

iris_test[!complete.cases(iris_test),] #결측값이 있는 데이터 출력
iris_test[complete.cases(iris_test),] #결측값이 없는 데이터 출력

ind <-which(!complete.cases(iris_test)) #결측값이 있는 데이터의 위치 확인
ind

mapply(mean, iris_test[,1:4], na.rm=TRUE)
#결측값을 제외한 나머지 데이터의 평균을 구함

iris_test <-iris_test[complete.cases(iris_test),]
#결측값 데이터를 삭제하고 나머지만 저장



#결측값 처리: 결측값 대체(replace) 방법
#install.packages("DMwR2")
#결측값의 처리를 지원하는 패키지
library(DMwR2)

#1. 중앙값으로 결측값을 대체
iris_test2[ind,] <-centralImputation(iris_test2)[ind,]
iris_test2[ind,] #대체된 데이터만 확인

#2. knn k개의 인접 이웃값의 가중평균으로 결측값 대체
iris_test3[ind,] <-knnImputation(iris_test3)[ind,]
iris_test3[ind,] #대체된 데이터만 확인

mapply(mean, iris_test2[ind,1:4], na.rm=TRUE)
mapply(mean, iris_test3[ind,1:4], na.rm=TRUE)


#이상값 처리
boxplot(iris)
#점들이 겹쳐보일 수 있음. 개수가 정확하지 않음.
#sepal width에 위아래로 몇개의 데이터가 이상치로 존재함을 확인.

upper <-median(iris$Sepal.Width) + IQR(iris$Sepal.Width)*1.5
#상방 이상치 위치와 값, 개수 확인
which(iris$Sepal.Width > upper) #12개의 이상치값이 보임
iris$Sepal.Width[which(iris$Sepal.Width > upper)]

under <-median(iris$Sepal.Width) - IQR(iris$Sepal.Width)*1.5
#하방 이상치 위치와 값, 개수 확인
which(iris$Sepal.Width < under) #4개의 이상치값이 보임
iris$Sepal.Width[which(iris$Sepal.Width > under)]


#데이터 정규화: 정규화하여 추후 적합하는 모형의 정확성을 높이는데 도움이 되도록
normiris <- as.data.frame(cbind(scale(iris[,1:4]), iris$Species))
result <- princomp(iris[,1:4])
summary(result) #comp1,2만으로 약 98% 정도를 설명할 수 있음.
head(result$score)
head(result$score[,1:2])


#변수제거(의미없는 데이터, 0에 가까운 분산을 갖는 변수를 제거)
library(caret)
nearZeroVar(iris, saveMetrics=TRUE)
#0에 가까운 분산을 갖는 변수 없음

install.packages("mlbench")
library(mlbench)

data(Soybean)
nearZeroVar(Soybean, saveMetrics=TRUE)
#leaf.mild, mycelium은 제거해도 분석 결과에 영향을 미치지 않을 확률이 높음.

findCorrelation(cor(iris[,1:4])) #3열 데이터가 다른 열들과 상관관계가 높음.
myiris <-iris[,-3] #3열의 데이터 제거
head(myiris)


#---------------------
#그래프 추가 정보 입력 방법
x <-c(1,3,6,8,9)
y <-c(12,56,78,32,9)
plot(x,y)
segments(6,78,8,32) #두 점을 잇는 선
arrows(3,56,1,12) #두 점을 잇는 화살표
rect(4,20,6,30,density = 10) #두 점을 대각 선상의 점으로 하는 사각형

#문자열 찍기(위치, 출력각도 지정)
text(4,40,"문자열", srt=45)
text(6,50,"문자", srt=-20)
mtext("상단 문자열", side=3)
mtext("우측 문자열", side=4)
box(col=2) #테두리를 빨간색으로
axis(1,pos=40,at=2:9,col=2)
axis(2,pos=3,at=10:70,col=4)
