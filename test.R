library(matrixStats)


N <- c(seq(2,10),seq(12,20,2),25,30,40,60);
R <- c(seq(2,10),15,20);
alpha <- 0.01;
alpha_0.01 <- data.frame(row.names = N)

loops <- 10^6;
time0 <- proc.time() #we start the timer
for(r in R){
  for(n in N){
    rr <- seq(1,r);
    X <- matrix(rnorm(n*r*loops),nrow = r*loops,ncol=n);#generate a random matrix
    S <- rowVars(X); #row sample variances
    dim(S)<-c(loops,r);#vector -> matrix
    F <- sapply(1:loops,function(x) round(sum(sort(S[x,])*(2*rr -1) / r)/sum(S[x,]),3))
    alpha_0.01['N','R'] <- quantile(F,1-alpha); #95% jih je levo od te meje
  }
}

time <- proc.time()-time0 #we stop the timer
print(time)
#pdf('Primer_1e+6_1percent.pdf')
# x<-seq(0,10,0.01);
# hist(F,freq = FALSE,xlim = c(min(F)-0.2,max(F)+0.2))
# curve(dnorm(x,mean=mean(F),sd=sd(F)),add=TRUE, col = 'red')
# abline(v=quantile(F,0.99),col='green',lwd=2)
# dev.off()

# user  system elapsed 
# 14.95    0.04   15.06