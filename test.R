n <- 2;
r <- 2;
i <- 1;
F <- c();
ponovitve <- 300000;

time0 <- proc.time()

while(i<=ponovitve){

X <- matrix(0,nrow = r, ncol = n);
S <- c();
F1 <- c();

for(j in 1:r){
  X[j,]<- rnorm(n);
  #S[j]<-sqrt(sum((X[j,] - mean(X[j,]))^2) / (n - 1))
  S[j]<-var(X[j,])
  F1[j] <- (2*j -1)/r; #uteži
}

SS <- sort(S); #vrstilne stat
F[i] <- round(sum(SS*F1)/sum(S),3);
i <- i+1;

}
time <- proc.time()-time0
#pdf('Primer_10000.pdf')
x<-seq(0,10,0.01);
hist(F, freq = FALSE,xlim = c(min(F)-0.2,max(F)+0.2))
curve(dnorm(x,mean=mean(F),sd=sd(F)),add=TRUE, col = 'red')
#dev.off()