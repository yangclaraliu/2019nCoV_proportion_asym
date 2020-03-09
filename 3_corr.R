m <- 1000

random <- rep(NA, m)
for(j in 1:m){
  a <- sample(SI_tmp, m, replace = T)
  b <- sample(IP_tmp, m, replace = T)
  point <- rep(NA,m)
  for(i in 1:m){
    tmp_1 <- a[i]
    tmp_2 <- b[i]
    point[i] <- if_else(tmp_2 > tmp_1, "pre-symptomatic","symptomatic")
    #length(which(incubation_sample > tmp_1))/length(incubation_sample)
    #if_else(tmp_2 > tmp_1, "pre-symptomatic","symptomatic")
  }
  random[j] <- (which(point == "pre-symptomatic") %>% length)/length(point)
}
hist(random)
print(mean(random))

correlated <- rep(NA, m)
for(j in 1:m){
  a <- sort(sample(SI_tmp, m, replace = T))
  b <- sort(sample(IP_tmp, m, replace = T))
  point <- rep(NA,m)
  for(i in 1:m){
    tmp_1 <- a[i]
    tmp_2 <- b[i]
    point[i] <- if_else(tmp_2 > tmp_1, "pre-symptomatic","symptomatic")
    #length(which(incubation_sample > tmp_1))/length(incubation_sample)
    #if_else(tmp_2 > tmp_1, "pre-symptomatic","symptomatic")
  }
  correlated[j] <- (which(point == "pre-symptomatic") %>% length)/length(point)
}
hist(correlated)
print(mean(correlated))

m <- 1000
anticorrelated <- rep(NA, m)
for(j in 1:m){
  a <- sort(sample(SI_tmp, m, replace = T))
  b <- sort(sample(IP_tmp, m, replace = T)) %>% rev
  point <- rep(NA,m)
  for(i in 1:m){
    tmp_1 <- a[i]
    tmp_2 <- b[i]
    point[i] <- if_else(tmp_2 > tmp_1, "pre-symptomatic","symptomatic")
    #length(which(incubation_sample > tmp_1))/length(incubation_sample)
    #if_else(tmp_2 > tmp_1, "pre-symptomatic","symptomatic")
  }
  anticorrelated[j] <- (which(point == "pre-symptomatic") %>% length)/length(point)
}
hist(anticorrelated)
print(mean(anticorrelated))
