rm(list=ls())

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

