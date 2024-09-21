#시계열 데이터


#시계열 데이터로 변형하는 명령어 ts
a <-ts(1:60, frequency=12, start=c(2017,1))
a
plot(a)
#시계열 객체에서는 시간이 같이 기록되어 plot으로 그리면 인덱스로 시간이 찍힘

b <-ts(sample(1:60,60), frequency=12, start=c(2017,1))
b
plot(b)


#항공 승객수 데이터 AirPassengers
decom <-decompose(AirPassengers) #데이터를 계절, 추세, 랜덤으로 나눔
decom

decom$trend
decom$seasonal
decom$random
plot(decom)


#--------------
#날짜 포멧

date <-as.Date(c('2024-09-01','2024-09-11'))
date
date1 <-as.Date(c('24/09/01', '24/09/11'), format='%y/%m/%d')
date1

install.packages("zoo")
library(zoo)
as.yearmon("2007-02") #연,월만 출력해주는 함수
as.yearmon("2007-02-01") 
as.yearqtr("2007-2")#연,분기만 출력해주는 함수
as.yearqtr("2007-4")


#---------------

#ts: 시계열 데이터로 변형하는 명령어
a <-ts(1:10, frequency = 4, start = c(2017,3))
a #2017년 3분기 이후로 4분기별 데이터 생성

#다변량 시계열 자료 생성
z <-ts(matrix(rnorm(300), nrow = 100, ncol = 3),start=c(2000,1), frequency = 12)
z #3개의 변수로 100개씩 데이터 생성, 2000년도부터 월별로 데이터 생성
plot(z)
plot(z, plot.type="single",lty=1:3) #한장에 그래프를 라인종류만 다르게하여 겹쳐그림.

AirPassengers.q <-aggregate(AirPassengers, nfrequency = 4, FUN=sum)
AirPassengers.q  #분기별 자료
AirPassengers.y <-aggregate(AirPassengers, nfrequency = 1, FUN=sum)
AirPassengers.y  #연별 자료


#---------------

#시계열 데이터의 결측치 대체
install.packages("imputeTS")
library(imputeTS)
tsAirgap  #항공 승객수 데이터, 결측치가 있는 데이터
na_interpolation(tsAirgap) #보간법을 이용한 대체
na_kalman(tsAirgap) #칼만 평활에 의한 대치
na_mean(tsAirgap) #평균값에 의한 대치
na_remove(tsAirgap) #결측값 제거
inter1 <-na_interpolation(tsAirgap)
inter2 <-na_kalman(tsAirgap)
inter3 <-na_mean(tsAirgap)


install.packages("ggplot2")
library(ggplot2)
ggplot_na_imputations(tsAirgap, inter1, tsAirgapComplete)
ggplot_na_imputations(tsAirgap, inter2, tsAirgapComplete)
ggplot_na_imputations(tsAirgap, inter3, tsAirgapComplete)
ggplot_na_distribution(tsAirgap) #결측치의 위치


#install.packages("tidyverse")
library(tidyverse)
#install.packages("nycflights13")
library(nycflights13)
#install.packages("dplyr")
library(dplyr)
select(flights, year, month, day)
flights %>% select(year, month, day)


install.packages("lubridate")
library(lubridate)

date <-today() #오늘 날짜 출력
date
y <-year(date)
m <-month(date)
d <-day(date)
cbind(y,m,d)

wday(date) #몇번째 요일인지, 일요일=1
wday(date, week_start = 1)
wday(date, label = TRUE)

now <-now() #현재 시간 출력
now

hr <-hour(now)
min <-minute(now)
sec <-second(now)
cbond(hr,min,sec)

ymd("2024-09-12") #년,월,일 순 데이터 기본형태로 바꿔줌
ymd("2024-09-12")

dmy("1292024")
mdy("09-12-2024")

depart <-ymd("2024-09-12")
arrive <-depart + hours(3) #3시간 전 출발
depart;
arrive
difftime(arrive, depart)


#날짜 수열 생성
seq(ymd('2024-09-01'), ymd('2024-11-30'), by = '1 week')
seq(ymd('2024-09-01'), ymd('2024-11-30'), by = '2 week')
seq(ymd('2024-09-01'), ymd('2024-11-30'), by = '1 months')
seq(ymd('2024-09-01'), ymd('2024-11-30'), by = '1 years')


#---------------

#시계열 자료의 시각화
data("lakers")
str(lakers)
head(lakers)
lakers$date
ymd(lakers$date)
lakers$date <-ymd(lakers$date)

lakers %>% ggplot(aes(date, y=0, col = game_type)) + geom_point()
lakers %>% ggplot(aes(date, fill = game_type)) + geom_bar()
lakers %>% ggplot(aes(date, game_type, col = result)) + geom_point()
lakers %>% mutate(wday=wday(date, label = TRUE)) %>% ggplot() + geom_bar(aes(x=wday, fill=wday))
lakers %>% mutate(wday=wday(date, label = TRUE)) %>% ggplot(aes(x=wday, fill=wday)) + geom_bar()
lakers %>% ggplot(aes(date)) + geom_histogram()



install.packages("tsbox")
library(tsbox)
class(austres)

mdeaths
austres

ts_c(mdeaths, austres) #다중 시계열로 만들어줌
ts_bind(mdeaths, austres) #시계열을 결합하여 단일 시계열로 만들어줌.
ts_span(mdeaths, start='1975-01', end='1977-12') #시계열의 일부구간 추출
ts_span(mdeaths, start='1975-01')
ts_frequency() #시계령릐 주기 변환
ts_frequency(mdeaths, 'year', sum) #연별 자료변환


#시계열 자료의 시각화 openair 패키지
install.packages("openair")
library(openair)
data(mydata)
str(mydata)
head(mydata)
summaryPlot(mydata)
selectByDate(mydata, start="1998-01-01", end='1988-03-31', day="weekday", hour=1)


timePlot(mydata, pollutant = c("nox", "no2"))
timePlot(selectByDate(mydata, year=2003, month=9), pollutant=c('nox', 'o3', 'pm10', 'pm25', 'ws'), group=TRUE)
timePlot(selectByDate(mydata, year=2003, month=7), pollutant=c('nox', 'o3', 'pm10', 'pm25', 'ws'))
timePlot(selectByDate(mydata, year=2003, month=7), pollutant=c('nox', 'o3', 'pm10', 'pm25', 'ws'), y.relation = "free")


install.packages("timeDate")
library(timeDate)
mydata <-as.data.frame(mydata)
format(mydata["date"], "%Y") #연도로 포맷 변경
format(mydata["date"], "%%m") #월
format(mydata["date"], "%Y-%m") #연-월
format(mydata["date"], "%Y-%j") #연-일(몇년도-몇번째일)
format(mydata["date"], "%Y-%w") #연-주(몇년도-몇번째주)


plot(mydata$date, mydata$pm10, type="l")
#연도별 일평균 자료로 변환
means <-aggregate(mydata["pm10"], format(mydata["date"], "%Y-%j"), mean, na.rm=T)
means[,"date"] <-seq(min(mydata[,"date"]), max(mydata[,"date"]), length=nrow(means))
plot(means$date, mean$pm10, type="l")

#연도별 월평균 자료로 변환
means <-aggregate(mydata["pm10"], format(mydata["date"], "%Y-%m"), mean, na.rm=T)
means[,"date"] <-seq(min(mydata[,"date"]), max(mydata[,"date"]), length=nrow(means))
plot(means$date, mean$pm10, type="l")

#여러 열을 동시에 연도별 월평균 자료로 변환
means <-aggregate(mydata[-1], format(mydata["date"], "%Y-%m"), mean, na.rm=T)
means[,"date"] <-seq(min(mydata[,"date"]), max(mydata[,"date"]), length=nrow(means))


#--------------
#openair 패키지를 이용하여

#월평균 시계열 집계자료를 생성
library(openair)
means <-timeAverage(mydata, avg.time="month")
head(means)
view(means)

#연도별 일평균 시계열 집계자료를 생성
means <-timeAverage(mydata, avg.time="day")
head(means)

#연도별 월별 95%분위수 시계열 집계자료 생성
means <-timeAverage(mydata, avg.time="month", statistic="percentile", percentile=95)
head(means)

#시각화
#1-12월별 박스플랏
levels(as.factor(format(mydata$date, "%m")))
plots(as.factor(format(mydata$date, "%m")), mydata$nox, col=4)

#연도별 월별 박스플랏
levels(as.factor(format(mydata$date, "%Y-%m")))
plot(as.factor(format(mydata$date, "%Y-%m")), mydata$nox, col=4)


#openair 패키지 타 데이터 적용예시
library(DMwR2)
data(algae)
str(algae)
head(algae)

myalgae <-algae #test를 위해 임의로 날짜가 있는 데이터 생성
mydata2 <-selectByDate(mydata, start="1998-01-01", end="1999-03-31", hour=1)
myalgae$date <-as.Date(mydata2$date)[1:200]
selectByDate(myalgae, start="1998-05-01", end="1999-03-31", day="weekday")

timePlot(myalgae, pollutant = c("cl","NO3"))
timePlot(selectByDate(myalgae, year=1998, month=7), pollutant=c("cl","NO3"))
timePlot(selectByDate(myalgae, year=1998, month=7), pollutant=c("cl","NO3"), y.relation="free")

#CalendarPlot(): 캘린더 히트맵
library(openair)
str(mydata)
tail(mydata)
calendarPlot(mydata, pollutant="no2", year=2003)
calendarPlot(mydata, pollutant="no2", year=2003, annotate='wd') #풍향표시추가
calendarPlot(mydata, pollutant="Cl", year=1998)


