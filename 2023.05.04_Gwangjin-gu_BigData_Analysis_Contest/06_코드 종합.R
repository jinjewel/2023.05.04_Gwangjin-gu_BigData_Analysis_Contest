### 원본데이터불러오기
install.packages("readxl")
library(readxl)
data <- read_excel("C:\\Users\\CBNU\\Desktop\\공모전 자료\\자동차\\차량대수\\광진구등록자동차대수.xlsx")
data

### 원본데이터에서 많은 사람들이 개인적으로 사용하는 승용, 승합차만 데이터로 다룬다.
### 화물, 특수차는 분석에서 제외한다.
data1 <- data[,c(1,5,9,21,22)]

### 승용차을 기준으로 한 동향
data_car <- data1[,1:2]
data_car
par(mfrow=c(2,3))
plot(data_car$time[1:12] ,data_car$car_total[1:12], ylim=c(82500,84500), type="l", main="2019 car total"
     , xlab=("time"), ylab=("number of registrations"))
plot(data_car$time[13:24] ,data_car$car_total[13:24], ylim=c(82500,84500), type="l", main="2019 car total"
     , xlab=("time"), ylab=("number of registrations"))
plot(data_car$time[25:36] ,data_car$car_total[25:36], ylim=c(82500,84500), type="l", main="2020 car total"
     , xlab=("time"), ylab=("number of registrations"))
plot(data_car$time[37:48] ,data_car$car_total[37:48], ylim=c(82500,84500), type="l", main="2021 car total"
     , xlab=("time"), ylab=("number of registrations"))
plot(data_car$time[49:60] ,data_car$car_total[49:60], ylim=c(82500,84500), type="l", main="2022 car total"
     , xlab=("time"), ylab=("number of registrations"))
plot(data_car$time[61:nrow(data1)] ,data_car$car_total[61:nrow(data1)], ylim=c(82500,84500), type="l"
     , main="2023 car total", xlab=("time"), ylab=("number of registrations"))


### 승합차를 기준으로 한 동향
data_van <- data1[,c(1,3)]
data_van
par(mfrow=c(2,3))
plot(data_van$time[1:12] ,data_van$van_total[1:12], ylim=c(2700,4200), type="l", main="2018 van total"
     , xlab=("time"), ylab=("number of registrations"))
plot(data_van$time[13:24] ,data_van$van_total[13:24], ylim=c(2700,4200), type="l", main="2019 van total"
     , xlab=("time"), ylab=("number of registrations"))
plot(data_van$time[25:36] ,data_van$van_total[25:36], ylim=c(2700,4200), type="l", main="2020 van total"
     , xlab=("time"), ylab=("number of registrations"))
plot(data_van$time[37:48] ,data_van$van_total[37:48], ylim=c(2700,4200), type="l", main="2021 van total"
     , xlab=("time"), ylab=("number of registrations"))
plot(data_van$time[49:60] ,data_van$van_total[49:60], ylim=c(2700,4200), type="l", main="2022 van total"
     , xlab=("time"), ylab=("number of registrations"))
plot(data_van$time[61:nrow(data1)] ,data_van$van_total[61:nrow(data1)], ylim=c(2700,4200), type="l"
     , main="2023 car total", xlab=("time"), ylab=("number of registrations"))

### 총합(승용+승합_화물+특수차)을 기준으로 한 동향
data_total <- data1[,c(1,4)]
data_total
par(mfrow=c(2,3))
plot(data_total$time[1:12] ,data_total$total_total[1:12], ylim=c(97000,101000), type="l", main="2018 total total"
     , xlab=("time"), ylab=("number of registrations"))
plot(data_total$time[13:24] ,data_total$total_total[13:24], ylim=c(97000,101000), type="l", main="2019 total total"
     , xlab=("time"), ylab=("number of registrations"))
plot(data_total$time[25:36] ,data_total$total_total[25:36], ylim=c(97000,101000), type="l", main="2020 total total"
     , xlab=("time"), ylab=("number of registrations"))
plot(data_total$time[37:48] ,data_total$total_total[37:48], ylim=c(97000,101000), type="l", main="2021 total total"
     , xlab=("time"), ylab=("number of registrations"))
plot(data_total$time[49:60] ,data_total$total_total[49:60], ylim=c(97000,101000), type="l", main="2022 total total"
     , xlab=("time"), ylab=("number of registrations"))
plot(data_total$time[61:nrow(data1)] ,data_total$total_total[61:nrow(data1)], ylim=c(97000,101000), type="l"
     , main="2023 car total", xlab=("time"), ylab=("number of registrations"))

###############################################################################

### 승용차를 기준으로 한 회귀직선
## 회귀계수 구하기
data_car_tol <- data1[,c(2,5)]
data_car_tol_lm <- lm(data_car_tol$car_total~data_car_tol$num, data=data_car_tol)
data_car_tol_lm

## 회귀직선 그래프 그리기
plot(data_car_tol$num, data_car_tol$car_total, type="l", main="승용차등록 회귀직선", xlab="time", ylab="등록대수")
abline(data_car_tol_lm)

# 회귀직선을 통해서 23년~25년도 추정하기
calcul_car_23_25 = NULL
calcul_car_23_25_row = NULL

# 23년도
calcul_car_23 = NULL
for( i in 61:72){
  data_23 <- data_car_tol_lm$coefficients[1] + data_car_tol_lm$coefficients[2]*i
  print(data_23)
  calcul_car_23 <- cbind(calcul_car_23, data_23)
}
calcul_car_23
rownames(calcul_car_23) = c("2023")
colnames(calcul_car_23) = c("1월","2월","3월","4월","5월","6월","7월","8월","9월","10월","11월","12월")
calcul_car_23
calcul_car_23_25 = rbind(calcul_car_23_25,calcul_car_23)
calcul_car_23_25_row = cbind(calcul_car_23_25_row,calcul_car_23)

# 24년도
calcul_car_24 = NULL
for( i in 73:84){
  data_24 <- data_car_tol_lm$coefficients[1] + data_car_tol_lm$coefficients[2]*i
  print(data_23)
  calcul_car_24 <- cbind(calcul_car_24, data_24)
}
calcul_car_24
rownames(calcul_car_24) = c("2024")
colnames(calcul_car_24) = c("1월","2월","3월","4월","5월","6월","7월","8월","9월","10월","11월","12월")
calcul_car_24
calcul_car_23_25 = rbind(calcul_car_23_25,calcul_car_24)
calcul_car_23_25_row = cbind(calcul_car_23_25_row,calcul_car_24)

# 25년도
calcul_car_25 = NULL
for( i in 85:96){
  data_25 <- data_car_tol_lm$coefficients[1] + data_car_tol_lm$coefficients[2]*i
  print(data_25)
  calcul_car_25 <- cbind(calcul_car_25, data_25)
}
calcul_car_25
rownames(calcul_car_25) = c("2025")
colnames(calcul_car_25) = c("1월","2월","3월","4월","5월","6월","7월","8월","9월","10월","11월","12월")
calcul_car_25
calcul_car_23_25 = rbind(calcul_car_23_25,calcul_car_25)
calcul_car_23_25_row = cbind(calcul_car_23_25_row,calcul_car_25)

## 23년~25년도 추정 데이터
# 3*12 행렬
calcul_car_23_25
# 1*36 행렬
rownames(calcul_car_23_25_row)=c("2325")
calcul_car_23_25_row

#############################################################################################

### 승합차를 기준으로 한 회귀직선
## 회귀계수 구하기
data_van_tol <- data1[,c(3,5)]
data_van_tol_lm <- lm(data_van_tol$van_total~data_van_tol$num, data=data_van_tol)
data_van_tol_lm

## 회귀직선 그래프 그리기
plot(data_van_tol$num, data_van_tol$van_total, type="l", main="승합차등록 회귀직선", xlab="time", ylab="등록대수")
abline(data_van_tol_lm)

# 회귀직선을 통해서 23년~25년도 추정하기
calcul_van_23_25 = NULL
calcul_van_23_25_row = NULL

# 23년도
calcul_van_23 = NULL
for( i in 61:72){
  data_23 <- data_van_tol_lm$coefficients[1] + data_van_tol_lm$coefficients[2]*i
  print(data_23)
  calcul_van_23 <- cbind(calcul_van_23, data_23)
}
calcul_van_23
rownames(calcul_van_23) = c("2023")
colnames(calcul_van_23) = c("1월","2월","3월","4월","5월","6월","7월","8월","9월","10월","11월","12월")
calcul_van_23
calcul_van_23_25 = rbind(calcul_van_23_25,calcul_van_23)
calcul_van_23_25_row = cbind(calcul_van_23_25_row,calcul_van_23)

# 24년도
calcul_van_24 = NULL
for( i in 73:84){
  data_24 <- data_van_tol_lm$coefficients[1] + data_van_tol_lm$coefficients[2]*i
  print(data_23)
  calcul_van_24 <- cbind(calcul_van_24, data_24)
}
calcul_van_24
rownames(calcul_van_24) = c("2024")
colnames(calcul_van_24) = c("1월","2월","3월","4월","5월","6월","7월","8월","9월","10월","11월","12월")
calcul_van_24
calcul_van_23_25 = rbind(calcul_van_23_25,calcul_van_24)
calcul_van_23_25_row = cbind(calcul_van_23_25_row,calcul_van_24)

# 25년도
calcul_van_25 = NULL
for( i in 85:96){
  data_25 <- data_van_tol_lm$coefficients[1] + data_van_tol_lm$coefficients[2]*i
  print(data_25)
  calcul_van_25 <- cbind(calcul_van_25, data_25)
}
calcul_van_25
rownames(calcul_van_25) = c("2025")
colnames(calcul_van_25) = c("1월","2월","3월","4월","5월","6월","7월","8월","9월","10월","11월","12월")
calcul_van_25
calcul_van_23_25 = rbind(calcul_van_23_25,calcul_van_25)
calcul_van_23_25_row = cbind(calcul_van_23_25_row,calcul_van_25)

## 23년~25년도 추정 데이터
# 3*12 행렬
calcul_van_23_25
# 1*36 행렬
rownames(calcul_van_23_25_row)=c("2325")
calcul_van_23_25_row

#############################################################################################
### 승용+승합차를 기준으로 한 회귀직선
## 회귀계수 구하기
data_total_tol <- data1[,c(4,5)]
data_total_tol_lm <- lm(data_total_tol$total_total~data_total_tol$num, data=data_total_tol)
data_total_tol_lm

## 회귀직선 그래프 그리기
plot(data_total_tol$num, data_total_tol$total_total, type="l", main="전체(승용+승합+화물+특수차) 등록 회귀직선", xlab="time", ylab="등록대수")
abline(data_total_tol_lm)

# 회귀직선을 통해서 23년~25년도 추정하기
calcul_total_23_25 = NULL
calcul_total_23_25_row = NULL

# 23년도
calcul_total_23 = NULL
for( i in 61:72){
  data_23 <- data_total_tol_lm$coefficients[1] + data_total_tol_lm$coefficients[2]*i
  print(data_23)
  calcul_total_23 <- cbind(calcul_total_23, data_23)
}
calcul_total_23
rownames(calcul_total_23) = c("2023")
colnames(calcul_total_23) = c("1월","2월","3월","4월","5월","6월","7월","8월","9월","10월","11월","12월")
calcul_total_23
calcul_total_23_25 = rbind(calcul_total_23_25,calcul_total_23)
calcul_total_23_25_row = cbind(calcul_total_23_25_row,calcul_total_23)

# 24년도
calcul_total_24 = NULL
for( i in 73:84){
  data_24 <- data_total_tol_lm$coefficients[1] + data_total_tol_lm$coefficients[2]*i
  print(data_23)
  calcul_total_24 <- cbind(calcul_total_24, data_24)
}
calcul_total_24
rownames(calcul_total_24) = c("2024")
colnames(calcul_total_24) = c("1월","2월","3월","4월","5월","6월","7월","8월","9월","10월","11월","12월")
calcul_total_24
calcul_total_23_25 = rbind(calcul_total_23_25,calcul_total_24)
calcul_total_23_25_row = cbind(calcul_total_23_25_row,calcul_total_24)

# 25년도
calcul_total_25 = NULL
for( i in 85:96){
  data_25 <- data_total_tol_lm$coefficients[1] + data_total_tol_lm$coefficients[2]*i
  print(data_25)
  calcul_total_25 <- cbind(calcul_total_25, data_25)
}
calcul_total_25
rownames(calcul_total_25) = c("2025")
colnames(calcul_total_25) = c("1월","2월","3월","4월","5월","6월","7월","8월","9월","10월","11월","12월")
calcul_total_25
calcul_total_23_25 = rbind(calcul_total_23_25,calcul_total_25)
calcul_total_23_25_row = cbind(calcul_total_23_25_row,calcul_total_25)

## 23년~25년도 추정 데이터
# 3*12 행렬
calcul_total_23_25
# 1*36 행렬
rownames(calcul_total_23_25_row)=c("2325")
calcul_total_23_25_row

#############################################################################################

### 이동평균 사용
install.packages("zoo")
library("zoo")

### 승용차을 기준으로 한 이동평균
## 월별로 전체를 합침 
data_car_tol <- data1[,c(2,5)]
data_car_tol
## 표준편차
sd(data_car_tol$car_total)
qt(0.95,nrow(data_car_tol))
## 이동평균 후 회귀직선 추정
par(mfrow=c(2,3))
# m = 4 이동평균 
m = 4
rollmean_4 <- rollmean(data_car_tol[,1],m) # 중심화이동평균
plot(data_car_tol$num, data_car_tol$car_total, type="l", main="car_total data", xlab=("time"), ylab=("number of registrations"))
plot(rollmean_4, col="blue", lwd=1,type="l", main="car_total m=4-center data", xlab=("time"), ylab=("number of registrations"))
# m = 4 회귀직선 
time_4 <- c(1:length(rollmean_4))
rollmean_line_4 <- cbind(rollmean_4,time_4)
rollmean_line_4 <- data.frame(rollmean_line_4)
data_car_tol_m4_lm <- lm(rollmean_line_4$car_total~rollmean_line_4$time_4, data=rollmean_line_4)
data_car_tol_m4_lm
plot(rollmean_4, col="blue", lwd=1,type="l", main="car m=4-center regress-line", xlab=("time"), ylab=("number of registrations"))
abline(data_car_tol_m4_lm)

# m = 6 이동평균 
m = 6
rollmean_6 <- rollmean(data_car_tol[,1],m) # 중심화이동평균
plot(data_car_tol$num, data_car_tol$car_total, type="l", main="car_total data", xlab=("time"), ylab=("number of registrations"))
plot(rollmean_6, col="blue", lwd=1,type="l", main="car_total m=6-center data", xlab=("time"), ylab=("number of registrations"))
# m = 4 회귀직선 
time_6 <- c(1:length(rollmean_6))
rollmean_line_6 <- cbind(rollmean_6,time_6)
rollmean_line_6 <- data.frame(rollmean_line_6)
data_car_tol_m6_lm <- lm(rollmean_line_6$car_total~rollmean_line_6$time_6, data=rollmean_line_6)
data_car_tol_m6_lm
plot(rollmean_6, col="blue", lwd=1,type="l", main="car m=6-center regress-line", xlab=("time"), ylab=("number of registrations"))
abline(data_car_tol_m6_lm)

# m = 8
m = 8
rollmean_8 <- rollmean(data_car_tol[,1],m) # 중심화이동평균
plot(data_car_tol$num, data_car_tol$car_total, type="l", main="car_total data", xlab=("time"), ylab=("number of registrations"))
plot(rollmean_8, col="blue", lwd=1,type="l", main="car_total m=8-center data", xlab=("time"), ylab=("number of registrations"))
# m = 8 회귀직선 
time_8 <- c(1:length(rollmean_8))
rollmean_line_8 <- cbind(rollmean_8,time_8)
rollmean_line_8 <- data.frame(rollmean_line_8)
data_car_tol_m8_lm <- lm(rollmean_line_8$car_total~rollmean_line_8$time_8, data=rollmean_line_8)
data_car_tol_m8_lm
plot(rollmean_8, col="blue", lwd=1,type="l", main="car m=8-center regress-line", xlab=("time"), ylab=("number of registrations"))
abline(data_car_tol_m8_lm)

# m = 10
m = 10
rollmean_10 <- rollmean(data_car_tol[,1],m) # 중심화이동평균
plot(data_car_tol$num, data_car_tol$car_total, type="l", main="car_total data", xlab=("time"), ylab=("number of registrations"))
plot(rollmean_10, col="blue", lwd=1,type="l", main="car_total m=10-center data", xlab=("time"), ylab=("number of registrations"))
# m = 10 회귀직선 
time_10 <- c(1:length(rollmean_10))
rollmean_line_10 <- cbind(rollmean_10,time_10)
rollmean_line_10 <- data.frame(rollmean_line_10)
data_car_tol_m10_lm <- lm(rollmean_line_10$car_total~rollmean_line_10$time_10, data=rollmean_line_10)
data_car_tol_m10_lm
plot(rollmean_10, col="blue", lwd=1,type="l", main="car m=10-center regress-line", xlab=("time"), ylab=("number of registrations"))
abline(data_car_tol_m10_lm)

# m=4로한 회귀직선 데이터를 이용하여 23~25년 자동차 등록대수 예측
data_car_cal_m4 = NULL
data_m4 = NULL
for(i in 61:72){
  data_m4 <- data_car_tol_m4_lm$coefficients[1] + data_car_tol_m4_lm$coefficients[2]*i
  data_car_cal_m4 <- cbind(data_car_cal_m4, data_m4)
}
data_car_cal_m4
data_car_cal_m4 = NULL
data_m4 = NULL
for(i in 73:84){
  data_m4 <- data_car_tol_m4_lm$coefficients[1] + data_car_tol_m4_lm$coefficients[2]*i
  data_car_cal_m4 <- cbind(data_car_cal_m4, data_m4)
}
data_car_cal_m4
data_car_cal_m4 = NULL
data_m4 = NULL
for(i in 85:96){
  data_m4 <- data_car_tol_m4_lm$coefficients[1] + data_car_tol_m4_lm$coefficients[2]*i
  data_car_cal_m4 <- cbind(data_car_cal_m4, data_m4)
}
data_car_cal_m4
## 2025년 12월에 예상 차량 등록 대수의 95% 신뢰구간
83897.65 - qt(0.95,nrow(data_car_tol))*sd(data_car_tol$car_total)
83897.65 + qt(0.95,nrow(data_car_tol))*sd(data_car_tol$car_total)
###################################################################
### 승합차을 기준으로 한 이동평균
## 월별로 전체를 합침 
data_van_tol <- data1[,c(3,5)]
data_van_tol
## 표준편차
sd(data_van_tol$van_total)
qt(0.95,nrow(data_van_tol))
## 이동평균 후 회귀직선 추정
par(mfrow=c(2,3))
# m = 4 이동평균 
m = 4
rollmean_4 <- rollmean(data_van_tol[,1],m) # 중심화이동평균
plot(data_van_tol$num, data_van_tol$van_total, type="l", main="van_total data", xlab=("time"), ylab=("number of registrations"))
plot(rollmean_4, col="blue", lwd=1 ,type="l", main="van_total m=4-center data", xlab=("time"), ylab=("number of registrations"))
# m = 4 회귀직선 
time_4 <- c(1:length(rollmean_4))
rollmean_line_4 <- cbind(rollmean_4,time_4)
rollmean_line_4 <- data.frame(rollmean_line_4)
data_van_tol_m4_lm <- lm(rollmean_line_4$van_total~rollmean_line_4$time_4, data=rollmean_line_4)
data_van_tol_m4_lm
plot(rollmean_4, col="blue", lwd=1,type="l", main="van m=4-center regress-line", xlab=("time"), ylab=("number of registrations"))
abline(data_van_tol_m4_lm)

# m = 6 이동평균 
m = 6
rollmean_6 <- rollmean(data_van_tol[,1],m) # 중심화이동평균
plot(data_van_tol$num, data_van_tol$van_total, type="l", main="van_total data", xlab=("time"), ylab=("number of registrations"))
plot(rollmean_6, col="blue", lwd=1 ,type="l", main="van_total m=6-center data", xlab=("time"), ylab=("number of registrations"))
# m = 4 회귀직선 
time_6 <- c(1:length(rollmean_6))
rollmean_line_6 <- cbind(rollmean_6,time_6)
rollmean_line_6 <- data.frame(rollmean_line_6)
data_van_tol_m6_lm <- lm(rollmean_line_6$van_total~rollmean_line_6$time_6, data=rollmean_line_6)
data_van_tol_m6_lm
plot(rollmean_6, col="blue", lwd=1 ,type="l", main="van m=6-center regress-line", xlab=("time"), ylab=("number of registrations"))
abline(data_van_tol_m6_lm)

# m = 8
m = 8
rollmean_8 <- rollmean(data_van_tol[,1],m) # 중심화이동평균
plot(data_van_tol$num, data_van_tol$van_total, type="l", main="van_total data", xlab=("time"), ylab=("number of registrations"))
plot(rollmean_8, col="blue", lwd=1, type="l", main="van_total m=8-center data", xlab=("time"), ylab=("number of registrations"))
# m = 8 회귀직선 
time_8 <- c(1:length(rollmean_8))
rollmean_line_8 <- cbind(rollmean_8,time_8)
rollmean_line_8 <- data.frame(rollmean_line_8)
data_van_tol_m8_lm <- lm(rollmean_line_8$van_total~rollmean_line_8$time_8, data=rollmean_line_8)
data_van_tol_m8_lm
plot(rollmean_8, col="blue", lwd=1, type="l", main="van m=8-center regress-line", xlab=("time"), ylab=("number of registrations"))
abline(data_van_tol_m8_lm)

# m = 10
m = 10
rollmean_10 <- rollmean(data_van_tol[,1],m) # 중심화이동평균
plot(data_van_tol$num, data_van_tol$van_total, type="l", main="van_total data", xlab=("time"), ylab=("number of registrations"))
plot(rollmean_10, col="blue", lwd=1, type="l", main="van_total m=10-center data", xlab=("time"), ylab=("number of registrations"))
# m = 10 회귀직선 
time_10 <- c(1:length(rollmean_10))
rollmean_line_10 <- cbind(rollmean_10,time_10)
rollmean_line_10 <- data.frame(rollmean_line_10)
data_van_tol_m10_lm <- lm(rollmean_line_10$van_total~rollmean_line_10$time_10, data=rollmean_line_10)
data_van_tol_m10_lm
plot(rollmean_10, col="blue", lwd=1, type="l", main="van m=10-center regress-line", xlab=("time"), ylab=("number of registrations"))
abline(data_van_tol_m10_lm)

# m=4로한 회귀직선 데이터를 이용하여 23~25년 자동차 등록대수 예측
data_van_cal_m4 = NULL
data_m4 = NULL
for(i in 61:72){
  data_m4 <- data_van_tol_m4_lm$coefficients[1] + data_van_tol_m4_lm$coefficients[2]*i
  data_van_cal_m4 <- cbind(data_van_cal_m4, data_m4)
}
data_van_cal_m4
data_van_cal_m4 = NULL
data_m4 = NULL
for(i in 73:84){
  data_m4 <- data_van_tol_m4_lm$coefficients[1] + data_van_tol_m4_lm$coefficients[2]*i
  data_van_cal_m4 <- cbind(data_van_cal_m4, data_m4)
}
data_van_cal_m4
data_van_cal_m4 = NULL
data_m4 = NULL
for(i in 85:96){
  data_m4 <- data_van_tol_m4_lm$coefficients[1] + data_van_tol_m4_lm$coefficients[2]*i
  data_van_cal_m4 <- cbind(data_van_cal_m4, data_m4)
}
data_van_cal_m4
## 2025년 12월에 예상 차량 등록 대수의 95% 신뢰구간
1795.184 - qt(0.95,nrow(data_van_tol))*sd(data_van_tol$van_total)
1795.184 + qt(0.95,nrow(data_van_tol))*sd(data_van_tol$van_total)


#############################################################################################

### 전체(승용+승합_화물+특수차)를 기준으로 한 이동평균
## 월별로 전체를 합침 
data_total_tol <- data1[,c(4,5)]
data_total_tol
## 표준편차
sd(data_total_tol$total_total)
qt(0.95,nrow(data_total_tol))
## 이동평균 후 회귀직선 추정
par(mfrow=c(2,3))
# m = 4 이동평균 
m = 4
rollmean_4 <- rollmean(data_total_tol[,1],m) # 중심화이동평균
plot(data_total_tol$num, data_total_tol$total_total, type="l", main="total_total data", xlab=("time"), ylab=("number of registrations"))
plot(rollmean_4, col="blue", lwd=1,type="l", main="total_total m=4-center data", xlab=("time"), ylab=("number of registrations"))
# m = 4 회귀직선 
time_4 <- c(1:length(rollmean_4))
rollmean_line_4 <- cbind(rollmean_4,time_4)
rollmean_line_4 <- data.frame(rollmean_line_4)
data_total_tol_m4_lm <- lm(rollmean_line_4$total_total~rollmean_line_4$time_4, data=rollmean_line_4)
data_total_tol_m4_lm
plot(rollmean_4, col="blue", lwd=1, type="l", main="total m=4-center regress-line", xlab=("time"), ylab=("number of registrations"))
abline(data_total_tol_m4_lm)

# m = 6 이동평균 
m = 6
rollmean_6 <- rollmean(data_total_tol[,1],m) # 중심화이동평균
plot(data_total_tol$num, data_total_tol$total_total, type="l", main="total_total data", xlab=("time"), ylab=("number of registrations"))
plot(rollmean_6, col="blue", lwd=1, type="l", main="total_total m=6-center data", xlab=("time"), ylab=("number of registrations"))
# m = 4 회귀직선 
time_6 <- c(1:length(rollmean_6))
rollmean_line_6 <- cbind(rollmean_6,time_6)
rollmean_line_6 <- data.frame(rollmean_line_6)
data_total_tol_m6_lm <- lm(rollmean_line_6$total_total~rollmean_line_6$time_6, data=rollmean_line_6)
data_total_tol_m6_lm
plot(rollmean_6, col="blue", lwd=1, type="l", main="total m=6-center regress-line", xlab=("time"), ylab=("number of registrations"))
abline(data_total_tol_m6_lm)

# m = 8
m = 8
rollmean_8 <- rollmean(data_total_tol[,1],m) # 중심화이동평균
plot(data_total_tol$num, data_total_tol$total_total, type="l", main="total_total data", xlab=("time"), ylab=("number of registrations"))
plot(rollmean_8, col="blue", lwd=1, type="l", main="total_total m=8-center data", xlab=("time"), ylab=("number of registrations"))
# m = 8 회귀직선 
time_8 <- c(1:length(rollmean_8))
rollmean_line_8 <- cbind(rollmean_8,time_8)
rollmean_line_8 <- data.frame(rollmean_line_8)
data_total_tol_m8_lm <- lm(rollmean_line_8$total_total~rollmean_line_8$time_8, data=rollmean_line_8)
data_total_tol_m8_lm
plot(rollmean_8, col="blue", lwd=1, type="l", main="total m=8-center regress-line", xlab=("time"), ylab=("number of registrations"))
abline(data_total_tol_m8_lm)

# m = 10
m = 10
rollmean_10 <- rollmean(data_total_tol[,1],m) # 중심화이동평균
plot(data_total_tol$num, data_total_tol$total_total, type="l", main="total_total data", xlab=("time"), ylab=("number of registrations"))
plot(rollmean_10, col="blue", lwd=1, type="l", main="total_total m=10-center data", xlab=("time"), ylab=("number of registrations"))
# m = 10 회귀직선 
time_10 <- c(1:length(rollmean_10))
rollmean_line_10 <- cbind(rollmean_10,time_10)
rollmean_line_10 <- data.frame(rollmean_line_10)
data_total_tol_m10_lm <- lm(rollmean_line_10$total_total~rollmean_line_10$time_10, data=rollmean_line_10)
data_car_tol_m10_lm
plot(rollmean_10, col="blue", lwd=1, type="l", main="total m=10-center regress-line", xlab=("time"), ylab=("number of registrations"))
abline(data_total_tol_m10_lm)

# m=4로한 회귀직선 데이터를 이용하여 23~25년 자동차 등록대수 예측
data_total_cal_m4 = NULL
data_m4 = NULL
for(i in 61:72){
  data_m4 <- data_total_tol_m4_lm$coefficients[1] + data_total_tol_m4_lm$coefficients[2]*i
  data_total_cal_m4 <- cbind(data_total_cal_m4, data_m4)
}
data_total_cal_m4
data_total_cal_m4 = NULL
data_m4 = NULL
for(i in 73:84){
  data_m4 <- data_total_tol_m4_lm$coefficients[1] + data_total_tol_m4_lm$coefficients[2]*i
  data_total_cal_m4 <- cbind(data_total_cal_m4, data_m4)
}
data_total_cal_m4
data_total_cal_m4 = NULL
data_m4 = NULL
for(i in 85:96){
  data_m4 <- data_total_tol_m4_lm$coefficients[1] + data_total_tol_m4_lm$coefficients[2]*i
  data_total_cal_m4 <- cbind(data_total_cal_m4, data_m4)
}
data_total_cal_m4
## 2025년 12월에 예상 차량 등록 대수의 95% 신뢰구간
98111.98 - qt(0.95,nrow(data_total_tol))*sd(data_total_tol$total_total)
98111.98 + qt(0.95,nrow(data_total_tol))*sd(data_total_tol$total_total)
##########################################################################

data_mon_out <- matrix(c(58656, 61479, 56835, 60307, 57008, 60490),1,6)
colMeans(data_mon_out) = c("19년 유입","19년 유출","20년 유입","20년 유출","21년 유입","21년 유출")
data_mon_out

#######################################################

##지하철 2,5,7호선 승차총승객수 분석 코드

rm(list=ls())

setwd("C:\\Users\\USER\\OneDrive\\바탕 화면")

#데이터 불러오기
subway <- read.table("C:\\Users\\USER\\OneDrive\\바탕 화면\\subway_257.txt", skip = 1)
two <- subway[1:19, ]
five <- subway[20:38, ]
seven <- subway[39:57, ]

#변수 "년월" -> 문자형으로 바꿈
#subway$v3 <-as.Date(subway$v3, "%y-%d")

#열 추가 (년월 대신 순서대로 1~19)
two[, "v5"] <- c(1:19)
five[, "v5"] <- c(1:19)
seven[, "v5"] <- c(1:19)

two; five; seven
str(subway)


#그림그리기
par(mfrow=c(2, 2))
plot(two$v5, two$V4, type="l", main = "2호선")
points(two$v5, two$V4)
plot(five$v5, five$V4, type="l", main = "5호선")
points(five$v5, five$V4)
plot(seven$v5, seven$V4, type="l", main = "7호선")
points(seven$v5, seven$V4)


par(mfrow=c(1,2))

#시계열 x -> 이동평균 계산
#install.packages("zoo")
library(zoo)

#이동평균 2호선 m=4, m=6
temp.zoo <- zoo(two$V4, two$V3)
class(temp.zoo)
rollmean_two1 <- (rollmean(temp.zoo, 4, align="center"))
rollmean_two2 <- (rollmean(temp.zoo, 6, align="center"))

#이동평균 5호선 m=4, m=6
temp.zoo <- zoo(five$V4, five$V3)
class(temp.zoo)
rollmean_five1 <- (rollmean(temp.zoo, 4, align="center"))
rollmean_five2 <- (rollmean(temp.zoo, 6, align="center"))


#이동평균 7호선 m=4, m=6
temp.zoo <- zoo(seven$V4, seven$V3)
class(temp.zoo)
rollmean_seven1 <- (rollmean(temp.zoo, 4, align="center"))
rollmean_seven2 <- (rollmean(temp.zoo, 6, align="center"))



#이동평균 그림
plot(c(2:17), rollmean_two1, type='l', main="2호선 (m=4)")
plot(c(3:16), rollmean_two2, type='l', main="2호선 (m=6)")
plot(c(2:17), rollmean_five1, type='l', main="5호선 (m=4)")
plot(c(3:16), rollmean_five2, type='l', main="5호선 (m=6)")
plot(c(2:17), rollmean_seven1, type='l', main="7호선 (m=4)")
plot(c(3:16), rollmean_seven2, type='l', main="7호선 (m=6)")

#2호선 회귀
lm(rollmean_two1 ~ c(2:17))
lm(rollmean_two2 ~ c(3:16))

two_m4 <- function(x)
{
  y <- 46154115 + 7651*x
  
  return(y)
}

two_m6 <- function(x)
{
  y <- 46350296 + 3693*x
  
  return(y)
}


#5호선 회귀
lm(rollmean_five1 ~ c(2:17))
lm(rollmean_five2 ~ c(3:16))

five_m4 <- function(x)
{
  y <- 18080039 + 8262*x
  
  return(y)
}

five_m6 <- function(x)
{
  y <- (1.822e+07) + (7.304e+02)*x
  
  return(y)
}


#7호선 회귀
lm(rollmean_seven1 ~ c(2:17))
lm(rollmean_seven2 ~ c(3:16))

seven_m4 <- function(x)
{
  y <- 21235827 + 19506*x
  
  return(y)
}

seven_m6 <- function(x)
{
  y <- 21421291 + 9003*x
  
  return(y)
}

#2023년 2호선 (m=4)
two_m4_23 <- rep(0, 12)
for(i in 1:12)
  two_m4_23[i] <- two_m4(i+50)

#2023년 2호선 (m=6)
two_m6_23 <- rep(0, 12)
for(i in 1:12)
  two_m6_23[i] <- two_m6(i+50)

#2023년 5호선 (m=4)
five_m4_23 <- rep(0, 12)
for(i in 1:12)
  five_m4_23[i] <- five_m4(i+50)

#2023년 5호선 (m=6)
five_m6_23 <- rep(0, 12)
for(i in 1:12)
  five_m6_23[i] <- five_m6(i+50)

#2023년 7호선 (m=4)
seven_m4_23 <- rep(0, 12)
for(i in 1:12)
  seven_m4_23[i] <- seven_m4(i+50)

#2023년 7호선 (m=6)
seven_m6_23 <- rep(0, 12)
for(i in 1:12)
  seven_m6_23[i] <- seven_m6(i+50)


#---------------------------------------# 

#2024년 2호선 (m=4)
two_m4_24 <- rep(0, 12)
for(i in 1:12)
  two_m4_24[i] <- two_m4(i+62)

#2024년 2호선 (m=6)
two_m6_24 <- rep(0, 12)
for(i in 1:12)
  two_m6_24[i] <- two_m6(i+62)

#2024년 5호선 (m=4)
five_m4_24 <- rep(0, 12)
for(i in 1:12)
  five_m4_24[i] <- five_m4(i+62)

#2024년 5호선 (m=6)
five_m6_24 <- rep(0, 12)
for(i in 1:12)
  five_m6_24[i] <- five_m6(i+62)

#2024년 7호선 (m=4)
seven_m4_24 <- rep(0, 12)
for(i in 1:12)
  seven_m4_24[i] <- seven_m4(i+62)

#2024년 7호선 (m=6)
seven_m6_24 <- rep(0, 12)
for(i in 1:12)
  seven_m6_24[i] <- seven_m6(i+62)

#--------------------------------------------#

#2025년 2호선 (m=4)
two_m4_25 <- rep(0, 12)
for(i in 1:12)
  two_m4_25[i] <- two_m4(i+74)

#2025년 2호선 (m=6)
two_m6_25 <- rep(0, 12)
for(i in 1:12)
  two_m6_25[i] <- two_m6(i+74)

#2025년 5호선 (m=4)
five_m4_25 <- rep(0, 12)
for(i in 1:12)
  five_m4_25[i] <- five_m4(i+74)

#2025년 5호선 (m=6)
five_m6_25 <- rep(0, 12)
for(i in 1:12)
  five_m6_25[i] <- five_m6(i+74)

#2025년 7호선 (m=4)
seven_m4_25 <- rep(0, 12)
for(i in 1:12)
  seven_m4_25[i] <- seven_m4(i+74)

#2025년 7호선 (m=6)
seven_m6_25 <- rep(0, 12)
for(i in 1:12)
  seven_m6_25[i] <- seven_m6(i+74)

###########################################
### 견인 차량 데이터
data_chi <- read.table("C:\\Users\\CBNU\\Desktop\\월별차량단속대수.txt")
data_chi
## 월별차량단속횟수 및 견인횟수의 평균
mean(data_chi$V2)
mean(data_chi$V3)
# 월별차량단속횟수에 대한 견인횟수의 백분율
mean(data_chi$V3) / mean(data_chi$V2) * 100

# 그래프 그리기
plot(data_chi$V2, lwd=2, type="h", main="월별차량단속횟수 및 견인횟수", xlab="time", ylab="단속횟수", ylim=c(0,9200))
lines(data_chi$V3, col="blue")
legend("topright", legend= c("월별차량단속횟수","월별견인횟수"), col=c("black","blue"), pch=c(0,NA), lty=c(NA,1), box.lty=0 )

