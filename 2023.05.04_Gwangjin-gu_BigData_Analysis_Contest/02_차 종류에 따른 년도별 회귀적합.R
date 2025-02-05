### 원본데이터불러오기
install.packages("readxl")
library(readxl)
data <- read_excel("C:\\Users\\CBNU\\Desktop\\공모전 자료\\자동차\\차량대수\\광진구등록자동차대수.xlsx")
data

### 원본데이터에서 많은 사람들이 개인적으로 사용하는 승용, 승합차만 데이터로 다룬다.
### 화물, 특수차는 분석에서 제외한다.
data1 <- data[,c(1,5,9,21,22)]

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