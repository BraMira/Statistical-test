RowVar <- function(x) {
  rowSums((x - rowMeans(x))^2)/(dim(x)[2] - 1)
}


n <- 2;
r <- 2;
i <- 1;
F <- c();
rr <- seq(1,r);
loops <- 100000;

time0 <- proc.time()

while(i<=loops){

X <- matrix(rnorm(n),nrow = r, ncol = n);
S <- RowVar(X);

# for(j in 1:r){
#   S[j]<-var(X[j,])
# }

#SS <- sort(S); #vrstilne stat
F[i] <- round(sum(sort(S)*(2*rr -1) / r)/sum(S),3);
i <- i+1;

}
time <- proc.time()-time0
#pdf('Primer_10000.pdf')
x<-seq(0,10,0.01);
hist(F, freq = FALSE,xlim = c(min(F)-0.2,max(F)+0.2))
curve(dnorm(x,mean=mean(F),sd=sd(F)),add=TRUE, col = 'red')
#dev.off()