# simple randomization
sample_tailor <- function(n, ratio, trt, seed){
  set.seed(seed)
  u <- group <- numeric(n)
  prob <- cumsum(ratio)/sum(ratio)
  for (i in 1:50) {
    u[i] <- runif(1)
    k <-  1
    while (u[i] > prob[k]) {
      k <-  k +1
    }
    group[i] <- k
  }
  data.frame(obs = 1:n, 
             groups = trt[group])
}
sample_tailor(n = 50, 
              ratio = c(1,1), 
              trt = c("A", "B"),
              seed = 20210128)
