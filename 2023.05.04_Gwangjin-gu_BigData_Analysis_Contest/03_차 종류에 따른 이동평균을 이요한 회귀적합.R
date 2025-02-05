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