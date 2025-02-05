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