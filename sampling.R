library(tidyverse)
setwd("C:/Users/eideyliu/Desktop")
### generate incubation sample
incubation <- read.csv("nCoV_Incubation.csv")
incubation <- rbind(incubation,c(0,0)) %>% arrange(x) %>% mutate(x = round(x,1))

seq(0,21,by = 0.1) %>% 
  enframe %>% 
  left_join(., incubation, by = c("value" = "x")) %>% 
  distinct() %>% 
  mutate(Curve2 = imputeTS::na_interpolation(Curve1),
         tot = sum(Curve2),
         prop = Curve2/tot,
         n = round(prop*10000)) -> incubation_freq
incubation_sample <- list()
for(i in 1:nrow(incubation_freq)){
  incubation_sample[[i]] <- rep(incubation_freq$value[i], incubation_freq$n[i])
}

library(magrittr)
incubation_sample %<>% unlist
hist(incubation_sample)
mean(incubation_sample)
quantile(incubation_sample, c(0.025, 0.95))

####generate serial interval sample
serial <- read.csv("nCoV_Serial.csv")
serial <- serial %>% mutate(x = round(x,1))
serial[1,2] <- 0

seq(0,21,by = 0.1) %>% 
  enframe %>% 
  left_join(., serial, by = c("value" = "x")) %>% 
  distinct() %>% 
  mutate(Curve2 = imputeTS::na_interpolation(Curve1),
         tot = sum(Curve2),
         prop = Curve2/tot,
         n = round(prop*10000)) -> serial_freq
serial_sample <- list()
for(i in 1:nrow(serial_freq)){
  serial_sample[[i]] <- rep(serial_freq$value[i], serial_freq$n[i])
}

serial_sample %<>% unlist
density(serial_sample, bw = 0.2) %>% plot
mean(serial_sample)
quantile(serial_sample, c(0.025, 0.95))

###sampling
n = 10000
random <- rep(NA,n)
for(i in 1:n){
  tmp_1 <- sample(serial_sample, length(incubation_sample))
  tmp_2 <- length(which(incubation_sample > tmp_1))/length(incubation_sample)
  random[i] <- tmp_2 #if_else(tmp_2 > tmp_1, "pre-symptomatic","symptomatic")
}
#(which(random == "pre-symptomatic") %>% length)/n
quantile(random, c(0.25, 0.75))

correlated <- rep(NA,n)
for(i in 1:n){
  tmp_1 <- sort(sample(serial_sample, 10000,replace = T))
  #tmp_2 <- sort(incubation_sample)[1]
  correlated[i] <- length(which(incubation_sample > tmp_1))/length(incubation_sample)
    #if_else(tmp_2 > tmp_1, "pre-symptomatic","symptomatic")
}
(which(correlated == "pre-symptomatic") %>% length)/n
