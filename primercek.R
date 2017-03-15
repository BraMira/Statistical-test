n <- 2;
r <- 2;
F <- c();
i <- 1;

time0 <- proc.time()
while(i<=100000){
#vzorca
X_12 <- rnorm(n)
X_22 <- rnorm(n)

#cenilka variance
S_12 <- var(X_12)
S_22 <- var(X_22)
S <- sort(c(S_12,S_22))

F[i] <- round(((1/r)*S[1]+(3/r)*S[2])/(S_12+S_22),3)
i <- i+1;
}

#pdf('Primer_10000.pdf')
time <- proc.time()-time0
x<-seq(0,10,0.01);
hist(F, freq = FALSE,xlim = c(min(F)-0.2,max(F)+0.2))
curve(dnorm(x,mean=mean(F),sd=sd(F)),add=TRUE, col = 'red')
#dev.off()