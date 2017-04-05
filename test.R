library(matrixStats)


n <- 2;
r <- 2;

rr <- seq(1,r);
loops <- 10^7;
time0 <- proc.time() #we start the timer

X <- matrix(rnorm(n*r*loops),nrow = r*loops,ncol=n);#generate a random matrix

S <- rowVars(X); #row sample variances
dim(S)<-c(loops,r);#vector -> matrix

F <- sapply(1:loops,function(x) round(sum(sort(S[x,])*(2*rr -1) / r)/sum(S[x,]),3))

time <- proc.time()-time0 #we stop the timer

#pdf('Primer_1e+6_1.pdf')
x<-seq(0,10,0.01);
hist(F,freq = FALSE,xlim = c(min(F)-0.2,max(F)+0.2))
curve(dnorm(x,mean=mean(F),sd=sd(F)),add=TRUE, col = 'red')
#dev.off()