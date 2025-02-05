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