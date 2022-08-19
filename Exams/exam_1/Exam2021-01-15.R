N_class1 <- 1500
N_class2 <- 1000
data_class1 <- NULL
for(i in 1:N_class1){
  a <- rbinom(n = 1, size = 1, prob = 0.3)
  b <- rnorm(n = 1, mean = 15, sd = 3) * a + (1-a) * rnorm(n = 1, mean = 4, sd = 2)
  data_class1 <- c(data_class1,b)
}
data_class2 <- NULL
for(i in 1:N_class2){
  a <- rbinom(n = 1, size = 1, prob = 0.4)
  b <- rnorm(n = 1, mean = 10, sd = 5) * a + (1-a) * rnorm(n = 1, mean = 15, sd = 2)
  data_class2 <- c(data_class2,b)
}


gaussian_kernel = function(x,h){
  return(exp(-(x**2)/(2*h*h)))
}
data_seq=seq(-5,25,0.1)

kern_prob=matrix(NA, nrow=length(xs), ncol=2)

for(i in 1:length(xs)){
  x=xs[i]
  diff_1=x-df1[,1]
  p1=mean(sapply(diff_1,FUN=gaussian_kernel, h=5))
}
