n <- 2;
r <- 2;
F <- c();
i <- 1;

time0 <- proc.time()
while(i<=50000){
#vzorca
X_12 <- rnorm(n)
X_22 <- rnorm(n)

#cenilka variance
S_12 <- sqrt((1/(n-1))*((X_12[1]-mean(X_12))^2 + (X_12[2]-mean(X_12))^2))
S_22 <- sqrt((1/(n-1))* ((X_22[1]-mean(X_22))^2 + (X_22[2]-mean(X_22))^2))
S <- sort(c(S_12,S_22))

F[i] <- round(((1/r)*S[1]+(3/r)*S[2])/(S_12+S_22),2)
i <- i+1;
}

pdf('Primer_50000.pdf')
time <- proc.time()-time0
x<-seq(0,10,0.01);
hist(F, freq = FALSE, xlim = c(0,2),ylim = c(0,5))
curve(dnorm(x,mean=mean(F),sd=sd(F)),add=TRUE, col = 'red')
dev.off()