library(matrixStats)
library(bigmemory)

# alpha_0.01 <- read.table("alfa_1.txt",header=TRUE,row.names = 1)
# colnames(alpha_0.01)<-c(seq(2,10),15,20)
# N <- c(seq(12,16,2))#,25,30,40,60);
# R <- c(20)#,20);
alpha <- 0.01;
#alpha_0.01 <- matrix(as.numeric(0),nrow=length(N),ncol=length(R),dimnames = list(N,R))

loops <- 10^7;
# time0 <- proc.time() #we start the timer

n <- 60;
r <- 2;


rr <- seq(1,r);
X <- big.matrix(r*loops,n,type="double");
X[,]<-rnorm(n*r*loops);
#X <- matrix(rnorm(n*r*loops),nrow = r*loops,ncol=n);#generate a random matrix
S <- rowVars(as.matrix(X)); #row sample variances
dim(S)<-c(loops,r);#vector -> matrix
F <- sapply(1:loops,function(x) round(sum(sort(S[x,])*(2*rr -1) / r)/sum(S[x,]),3));
# alpha_0.01[as.character(n),as.character(r)] <- quantile(F,1-alpha); #95% jih je levo od te meje
# 
# save(alpha_0.01,file="ALFA_1.csv")
# write.table(alpha_0.01, file="alfa_1.txt")

#pdf('Primer_1e+6_1percent.pdf')
x<-seq(0,10,0.01);
hist(F,freq = FALSE,xlim = c(min(F)-0.2,max(F)+0.2))
curve(dnorm(x,mean=mean(F),sd=sd(F)),add=TRUE, col = 'red')
# abline(v=quantile(F,0.99),col='green',lwd=2)
# dev.off()

# user  system elapsed 
# 14.95    0.04   15.06

# user  system elapsed 
# 2021.92   30.10 2123.33 