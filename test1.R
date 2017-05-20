library(matrixStats)
# library(bigmemory)
# 
# #alpha_0.01 <- read.table("alfa_1.txt",header=TRUE,row.names = 1)
# #colnames(alpha_0.01)<-c(seq(2,10),15,20)
# alpha_0.05 <- read.table("alfa_2.txt",header=TRUE,row.names = 1)
# colnames(alpha_0.05)<-c(seq(2,10),15,20)
# alpha_0.1 <- read.table("alfa_3.txt",header=TRUE,row.names = 1)
# colnames(alpha_0.1)<-c(seq(2,10),15,20)

N <- 60;
R <- 2;#seq(3,10);#,20);
#alpha <- 0.01;
#alpha_0.01 <- matrix(as.numeric(0),nrow=length(N),ncol=length(R),dimnames = list(N,R))

loops <- 10^7;
# time0 <- proc.time() #we start the timer
for(r in R){
  print(r)
  for(n in N){
    print(n)
    rr <- seq(1,r);

    X <- matrix(rnorm(n*r*loops),nrow = r*loops,ncol=n);#generate a random matrix
    S <- rowVars(X);
    #S <- sapply(1:r*loops, function(y) rowVars(X[y,],dim.=c(1,n)))
    # S <- as.big.matrix(rowVars(X[,])); #row sample variances
    dim(S)<-c(loops,r);#vector -> matrix
    F <- sapply(1:loops,function(x) round(sum(sort(S[x,])*(2*rr -1) / r)/sum(S[x,]),3));
    #alpha_0.01[as.character(n),as.character(r)] <- quantile(F,1-0.01);
    # alpha_0.05[as.character(n),as.character(r)] <- quantile(F,1-0.05);
    # alpha_0.1[as.character(n),as.character(r)] <- quantile(F,1-0.1);
    # write.table(alpha_0.05, file="alfa_2.txt")
    # write.table(alpha_0.1, file="alfa_3.txt")
  }
}
#save(alpha_0.01,file="ALFA_1.csv")
#write.table(alfa_0.01, file="alfa_1.txt")
# write.table(alpha_0.05, file="alfa_2.txt")
# write.table(alpha_0.1, file="alfa_3.txt")

x<-seq(0,10,0.01);
hist(F,freq = FALSE,xlim = c(min(F)-0.2,max(F)+0.2))
curve(dnorm(x,mean=mean(F),sd=sd(F)),add=TRUE, col = 'red')