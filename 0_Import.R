
# Load Packages
library(tidyverse)
library(googlesheets4)
library(openxlsx)
library(magrittr)

# Set working directory
#setwd("C:/Users/eideyliu/Desktop")
### generate incubation sample
setwd("~/GitHub/2019nCoV_proportion_asym")

# Load data from Li et al. (2020) NEJM
# Early Transmission Dynamics of Wuhan, China, of Novel Coronavirus - Infected
# Pneumonia

incubation <- read.csv("nCoV_Incubation.csv")
incubation <- rbind(incubation,c(0,0)) %>% arrange(x) %>% mutate(x = round(x,1))
seq(0,21,by = 0.1) %>% 
  enframe %>% 
  left_join(., incubation, by = c("value" = "x")) %>% 
  distinct() %>% 
  mutate(Curve2 = imputeTS::na_interpolation(Curve1),
         tot = sum(Curve2),
         prop = Curve2/tot,
         n = round(prop*n)) -> incubation_freq

incubation_sample_Li <- list()
for(i in 1:nrow(incubation_freq)){
  incubation_sample_Li[[i]] <- rep(incubation_freq$value[i], incubation_freq$n[i])
}

incubation_sample_Li %<>% unlist
hist(incubation_sample_Li)

mean(incubation_sample_Li)
quantile(incubation_sample_Li, c(0.025, 0.975))
# density(incubation_sample_Li) %>% plot

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
         n = round(prop*n)) -> serial_freq

serial_sample_Li <- list()
for(i in 1:nrow(serial_freq)){
  serial_sample_Li[[i]] <- rep(serial_freq$value[i], serial_freq$n[i])
}

serial_sample_Li %<>% unlist
# density(serial_sample_Li, bw = 0.2) %>% plot
mean(serial_sample_Li)
quantile(serial_sample_Li, c(0.025, 0.975))

# Load data from Bi et al. (2020)
# Epidemiology and Transmission of COVID-19 in Shenzhen China: Analysis of 391
# cases and 1,286 of their close contacts.

# Overall
# Incubation Period, lognormal distribution
# parameter 1: mean = 1.57, CI = 1.44-1.69
# parameter2: mean = 0.65, CI = 0.56, 0.73
# mean = 5.95, 4.94-7.11
param1 <- 1.57
param2 <- 0.65
incubation_sample_Bi <- list()
incubation_sample_Bi <- rlnorm(n, param1, param2) %>% 
  round(., 1) %>% 
  sort 
incubation_sample_Bi %>% mean
incubation_sample_Bi %>% log %>% sd
quantile(incubation_sample_Bi, c(0.025,0.975))

# Serial Interval, gamma distribution
# parameter 1: mean = 2.29, CI = 1.77-3.34
# parameter 2: mean = 0.36, CI = 0.26-0.57
param1 <- 2.29
param2 <- 0.36
serial_sample_Bi <- list()
serial_sample_Bi[["all"]] <- rgamma(n, param1, param2) %>% 
  round(., 1) %>% 
  sort
serial_sample_Bi[["early"]] <-  rnorm(n, 3.6, 0.6) %>% 
  round(., 1) %>% 
  sort
serial_sample_Bi[["mid"]] <-  rnorm(n, 8.1, 2.9) %>% 
  round(., 1) %>% 
  sort
serial_sample_Bi[["late"]] <-  rnorm(n, 8, 1.7) %>% 
  round(., 1) %>% 
  sort

mean(serial_sample_Bi$all)
quantile(serial_sample_Bi$all, c(0.025, 0.975))

mean(serial_sample_Bi$late)
quantile(serial_sample_Bi$late, c(0.025, 0.975))


# Load data from Nishiura, Linton, Akhmetzhanov (2020)
# parameter 1: mean = 2.305, sd = 0.439
# parameter 2: mean = 5.452, sd = 0.674
param1 <- 2.305
param2 <- 5.452
serial_sample_Nishiura <- rweibull(n, param1, param2) %>% 
  round(., 1) %>% 
  sort

mean(serial_sample_Nishiura)
quantile(serial_sample_Nishiura, c(0.025, 0.975))
