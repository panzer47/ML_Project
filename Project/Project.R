training <- read.table("pendigits.tra", head=FALSE, sep=",")
orig_train<-read.table("pendigits-orig.tes", head=FALSE, sep=",")
training
i<-1
vector=logical(length=16)
for(i in 1:16){
  if(i%%2==1) vector[i]=TRUE
  
}
a<-training[1,-17]

x<-vector();
x[1]<-0
x[2]<-100
training[1,(vector17)]


a<-training[1,-17]
training[1,]
?plot
orig_train[2,]
par(mfrow=c(2,2))
?par
for(j in 1:1000) {
if(training[j,17]=="4"){
  plot(x,x, col="white", main=training[j,17])
  for(i in 1:8) points(training[j,2*i-1], training[j,2*i], col=i, pch=4);
}
}
?plot
?points
col

library(chemometrics)
?Moutlier
mahal<-Moutlier(training[,-17])
tail(order(mahal$rd, decreasing=TRUE))

?order
?tail
?subset
summary(mahal)
?plot
