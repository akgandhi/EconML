n = 500
p = 2
set.seed(0)
x = matrix(runif(n*p),ncol=p)
y = rep(0,n)
for (i in 1:n) {
if (x[i,1]<=0.4 & x[i,2]>0.6) y[i]=1
if (x[i,1]>0.6 & x[i,2]>0.1 & x[i,2]<=0.5) y[i]=1
}

pdf(file="simpledata.pdf",height=5,width=5)
par(mar=c(4.5,4.5,0.5,0.5))
plot(x,col=y+1,xlab="x1",ylab="x2")
graphics.off()

library(rpart)
a = rpart(as.factor(y)~x)

pdf(file="simpletree.pdf",height=6,width=12)
par(mfrow=c(1,2))
par(mar=c(3,3,3,3))
plot(a)
text(a,use.n=TRUE)
par(mar=c(4.5,5,0.5,0.5))
plot(x,col=y+1,xlab="x1",ylab="x2")
abline(h=0.111,lwd=2)
segments(0.4028,0.111,0.4028,1.5,lwd=2)
segments(0.4028,0.4993,1.5,0.4993,lwd=2)
segments(-0.5,0.598,0.4028,0.598,lwd=2)
segments(0.5998,0.111,0.5998,0.4993,lwd=2)
graphics.off()

pdf(file="stop.pdf",height=5,width=5)
par(mar=c(4.5,5,0.5,0.5))
plot(x,col=y+1,xlab="x1",ylab="x2")
abline(h=0.111,lwd=2)
graphics.off()

set.seed(0)
n = 20
p = 2
xx = matrix(runif(n*p),ncol=p)
y = numeric(n)
for (i in 1:n) {
if (xx[i,1]>0.5 & xx[i,2]>0.5) y[i]=1
}

pdf(file="split1.pdf",height=5,width=5)
par(mar=c(4.5,4.5,0.5,0.5))
plot(xx,col=y+1,xlab="x1",ylab="x2")
graphics.off()

pdf(file="split2.pdf",height=5,width=5)
par(mar=c(4.5,4.5,0.5,0.5))
plot(xx,col=y+1,xlab="x1",ylab="x2")
sx = sort(xx[,1])
abline(v=sx+diff(sx)/2,lty=2)
graphics.off()


