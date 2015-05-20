rm(list=ls())
training <- read.table("pendigits.tra", head=FALSE, sep=",")
test<-read.table("pendigits.tes", head=FALSE, sep=",")
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
training<-round(7494*0.7)

training[,1:16]
?subset
?kmeans
train<-dataset[1:training,1:16]
clusters<-kmeans(train, centers=10, iter.max=1000 )
clusters$cluster
table( as.factor(dataset[1:training,"V17"])==9 )
?kmeans
install.packages("caret")
library(caret)
clusters$cluster<-clusters$cluster-1
table(clusters$cluster,dataset[1:training,17] )

i<-subset(clusters$cluster, clusters$cluster==2 )

labels<-clusters$cluster
?knn
attach(training)
resultsKnn<-knn(training[,1:16] , test[,1:16], as.factor(training[,"V17"]), k=5 )
resultsKnn
results2<-kknn(as.factor(V17)~. , training , test, kernel="gaussian" )
results2<-kknn(as.factor(V17)~. , training , test, kernel="optimal", k=7 )
results2$
head(results2$CL )
?kknn
?kknn
library(kknn)
confusionMatrix(resultsKnn , test[,"V17"])
confusionMatrix(results2$CL[,3], test[,"V17"])
labels[labels==8]<-10
labels[labels==9]<-11
labels[labels==3]<-12
labels[labels==7]<-13
labels[labels==6]<-14
labels[labels==0]<-15
labels[labels==2]<-16
labels[labels==5]<-17
labels[labels==4]<-18
labels[labels==1]<-19
labels<-labels-10
confusionMatrix(labels, dataset[1:training ,17])
as.data.table(clusters$cluster)
vfffv1

#CROSS VALIDATION
library(TunePareto)
k<-10
?generateCVRuns
cv.folds<-generateCVRuns( training[,1], ntimes=1, nfold=k, stratified=TRUE)

## prepare the structure to store the partial results

cv.results <- matrix (rep(0,4*k),nrow=k)
colnames (cv.results) <- c("k","fold","TR error","VA error")

cv.results[,"TR error"] <- 0
cv.results[,"VA error"] <- 0
cv.results[,"k"] <- k
for (j in 1:k)
{
  # get VA data
  va <- unlist(cv.folds[[1]][[1]])
  
  # train on TR data
  #my.lda.TR <- lda(target ~ X1 + X2, data = data[-va,], prior=priors, CV=FALSE)
  my.knn.TR<-kknn(as.factor(V17)~. , training[-va,] , training[va,], kernel="gaussian" ,k=1)
  # predict TR data
  #pred.va <- predict (my.lda.TR)$class
  pred.va<-predict(my.knn.TR$CL)
  tab <- table(training[va,17], my.knn.TR$CL)
  cv.results[j,"TR error"] <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
  
  # predict VA data
  pred.va <- predict (my.knn.TR, newdata=training[va,])$class
  
  tab <- table(data[va,]$target, pred.va)
  cv.results[j,"VA error"] <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
  
  cv.results[j,"fold"] <- j
}

?knn

?simulation
CVt<-simulation(as.factor(V17)~., training, runs=10, kernel="gaussian", k=1)
CVt
train.kknn( as.factor(V17)~., training, kcv=10)



###CONVERTING TO BITMAP
install.packages("pixmap")
library(pixmap)
?pixmap
image<-pixmap(as.vector(training[1,1:16]), nrow=100, ncol=100  )
?pixma
plot(image)
?pixmapGrey
install.packages("png")
library(png)
?writePNG
matrix<-matrix( c(0,1,0,1, 0,1,0,1,0,1,0,1,0,1,0,1), nrow=4, ncol=4)
matrix
writePNG(matrix, "lol.png", dpi=c(4,4),)

matrix[1,1]
midpoint<-function(x0, y0, xe, ye, value, matrix){
  dx<-xe-x0
  dy<-ye-x0
  d<-2*dy-dx
  incrE<-2*dy
  incrNE<-2*(dy-dx)
  x=x0
  y=y0
  matrix[x0,y0]<-value
  while(x<xe) {
    if(d<=0) {
      d<-d+incrE
      x<-x+1
    }else{
      d<-d+incrNE
      x<-x+1
      y<-y+1
    }
    matrix[x,y]<-value
  }  
  return(matrix)
}

matrix<-matrix( seq(from=0, to=0, length=64), nrow=8, ncol=8)
x0<-1
y0<-1
xe<-8
ye<-2
midpoint(x0,y0,xe,ye,1,matrix)
matrix[x0,y0]
matrix
?seq
