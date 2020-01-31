library(tidyverse)
#setwd("C:/Users/eideyliu/Desktop")
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
         n = round(prop*10000)) %>%
  mutate(revcum = 1 - (cumsum(Curve2)/sum(Curve2))) -> incubation_freq
incubation_sample <- list()
for(i in 1:nrow(incubation_freq)){
  incubation_sample[[i]] <- rep(incubation_freq$value[i], incubation_freq$n[i])
}

library(magrittr)
incubation_sample %<>% unlist
hist(incubation_sample)
mean(incubation_sample)
quantile(incubation_sample, c(0.025, 0.95))

incubation_sample

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
finding.pre.sym.prob <- function(x){
  res = rep(NA,length(x))
  for (i in 1:length(x)){
    y = round(x[i],1)
    if (dim(filter(incubation_freq, value == y))[1] == 0 ){res[i]=NA
      }else{res[i] = filter(incubation_freq, value == y)$revcum}
  }
  return(res)
}
  
n = 10000
data.frame(sample = 1:n) %>%
  mutate(serial.interval = sample(serial_sample,n, replace=T)) %>%
  mutate(prob.presym = finding.pre.sym.prob(serial.interval)) -> df

df %>% 
  summarise(median = median(prob.presym, na.rm=T),
            mean = mean(prob.presym, na.rm=T),
            lo = quantile(prob.presym, probs=.025, na.rm=T),
            hi = quantile(prob.presym, probs=.975, na.rm=T)
            )
df$prob.presym %>% hist()

