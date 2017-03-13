n <- 10;
r <- 2;
i <- 1;
F <- c();

while(i<=10){

X <- matrix(0,nrow = r, ncol = n);
S <- c();

for(j in 1:r){
  X[j,]<- rnorm(n)
  for(k in 1:n){
    S[j] <- sqrt((1/(n-1))*sum((X[j,k]-mean(X[j,]))^2))
  }
}
SS <- sort(S)
i <- i+1;
#Kako napišeš F vsoto po i?
}