library(matrixStats)
library(bigmemory)

alpha_0.01 <- read.table("alfa_1.txt",header=TRUE,row.names = 1)
colnames(alpha_0.01)<-c(seq(2,10),15,20)
alpha_0.05 <- read.table("alfa_2.txt",header=TRUE,row.names = 1)
colnames(alpha_0.05)<-c(seq(2,10),15,20)
alpha_0.1 <- read.table("alfa_3.txt",header=TRUE,row.names = 1)
colnames(alpha_0.1)<-c(seq(2,10),15,20)
# N <- c(seq(12,16,2))#,25,30,40,60);
# R <- c(20)#,20);
#alpha <- 0.01;
#alpha_0.01 <- matrix(as.numeric(0),nrow=length(N),ncol=length(R),dimnames = list(N,R))

loops <- 10^6;
# time0 <- proc.time() #we start the timer

n <- 2;
r <- 2;


rr <- seq(1,r);
X <- big.matrix(r*loops,n,type="double");
X[,]<-rnorm(n*r*loops);
#X <- matrix(rnorm(n*r*loops),nrow = r*loops,ncol=n);#generate a random matrix
S <- big.matrix(loops,r,type="double");
S[,] <- rowVars(X[,]);
#S <- sapply(1:r*loops, function(y) rowVars(X[y,],dim.=c(1,n)))
# S <- as.big.matrix(rowVars(X[,])); #row sample variances
# dim(S)<-c(loops,r);#vector -> matrix
F <- sapply(1:loops,function(x) round(sum(sort(S[x,])*(2*rr -1) / r)/sum(S[x,]),3));
#alpha_0.01[as.character(n),as.character(r)] <- quantile(F,1-0.01);
alpha_0.05[as.character(n),as.character(r)] <- quantile(F,1-0.05);
alpha_0.1[as.character(n),as.character(r)] <- quantile(F,1-0.1);#95% jih je levo od te meje


#write.table(alpha_0.01, file="alfa_1.txt")
write.table(alpha_0.05, file="alfa_2.txt")
write.table(alpha_0.1, file="alfa_3.txt")