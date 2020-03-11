m <- 1000

random <- rep(NA, m)
random_diff <- list()
for(j in 1:m){
  a <- sample(SI_tmp, m, replace = T)
  b <- sample(IP_tmp, m, replace = T)
  point <- rep(NA,m)
  random_diff[[j]] <- list()
  for(i in 1:m){
    tmp_1 <- a[i]
    tmp_2 <- b[i]
    point[i] <- if_else(tmp_2 > tmp_1, "pre-symptomatic","symptomatic")
    random_diff[[j]][[i]] <- tmp_1 - tmp_2
    #length(which(incubation_sample > tmp_1))/length(incubation_sample)
    #if_else(tmp_2 > tmp_1, "pre-symptomatic","symptomatic")
  }
  random[j] <- (which(point == "pre-symptomatic") %>% length)/length(point)
}

hist(random)
print(mean(random))
random_diff_perc <- list()
for(j in 1:1000){
  seq(0,10, by = 0.1) %>% 
    enframe %>% 
    mutate(proportion = NA) -> tmp
  for(i in 1:nrow(tmp)){
    tmp$proportion[i] <-length(which(unlist(random_diff[[j]]) >= tmp$value[i]))/length(unlist(random_diff[[j]]))
  }
  random_diff_perc[[j]] <- tmp$proportion
  rm(tmp)
}

names(random_diff_perc) <- 1:1000
random_diff_perc %>% 
  bind_rows(.) %>% 
  t %>% 
  as_tibble %>% 
  mutate(group = 1:1000) %>% 
  pivot_longer(cols = paste0("V",1:101),
               names_to = "time") %>% 
  mutate(time = gsub("V","",time) %>% as.numeric()) %>% 
  ggplot(., aes(x = time, y = value, group = group))+
  geom_line()+
  scale_x_continuous(breaks = seq(0,100,by = 10),
                     labels = c(0:10) %>% as.character) +
  labs(x = "Days After Symptoms Onset \n& Before Case Isolation",
       y = "Proportion Preventable through Case Isolation")+
  theme_cowplot()

# ggsave("preventable_proportion_Li.png",
#        width = 6,
#        height = 6,
#        dpi = 800)
  
random_diff_perc %>% 
  bind_rows(.) %>% 
  t %>% 
  as_tibble %>% 
  dplyr::select(V1, V11, V21, V31, V71) %>% 
  colMeans() %>% 
  setNames(paste0("day",c(0:3,7)))

library(cowplot)
random_diff_perc %>% 
  ggplot(., aes(x = value, y = proportion))+
  geom_point()+
  theme_cowplot()+
  scale_x_continuous(breaks = c(0:10),
                     labels = c(0:10) %>% as.character) +
  labs(x = "Days After Symptoms Onset \n& Before Case Isolation",
       y = "Proportion Preventable through Case Isolation")

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
