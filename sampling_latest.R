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

density(incubation_sample) %>% plot

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

###overlay cdf
# seq(0,21,by=0.1) %>% 
#   enframe %>% 
#   mutate(cdf = NA,
#          type = "Serial Interval") -> serial_cdf
# for(i in 1:nrow(serial_cdf)) {
#   serial_cdf$cdf[i] <- length(which(serial_sample > serial_cdf$value[i]))/length(serial_sample)
# }
# seq(0,21,by=0.1) %>% 
#   enframe %>% 
#   mutate(cdf = NA,
#          type = "Incubation Period") -> incubation_cdf
# for(i in 1:nrow(incubation_cdf)) {
#   incubation_cdf$cdf[i] <- length(which(incubation_sample > incubation_cdf$value[i]))/length(incubation_sample)
# }
# 
# incubation_cdf %>% 
#   bind_rows(serial_cdf) %>% 
#   ggplot(., aes(x = value,
#                 y = cdf,
#                 color = type)) +
#   geom_line(size = 1.1)+
#   cowplot::theme_cowplot() +
#   scale_color_brewer("", palette = "Set1") +
#   xlab("Days") + ylab("Reverse Cumulative Distribution") +
#   theme(legend.position = "top")

###sampling
n = 10000
#random <- rep(NA,n)

### Stacked barplot (but really it's segment plot)
expand.grid(seq(0,21,by=0.1),c("Symptomatic", "Pre-symptomatic")) %>% 
  as_tibble %>% 
  setNames(c("duration","status")) %>% 
  arrange(duration) %>% 
  mutate(prob = NA) -> stacked

serial_sample %>%
  enframe %>% 
  group_by(value) %>% 
  tally %>% 
  right_join(stacked, by = c("value" = "duration")) %>% 
  filter(!is.na(n))  -> stacked

val <- unique(stacked$value) %>% as.numeric %>% sort
for(i in 1:length(val)){
  stacked[stacked$value == val[i], "prob"] <- 
    c(1-length(which(incubation_sample > val[i]))/length(incubation_sample),
      length(which(incubation_sample > val[i]))/length(incubation_sample))
}

stacked %>% 
  mutate(height = prob*n) %>% 
  group_by(status) %>% 
  group_split() -> tmp
  
lo <- smooth.spline(x = tmp[[1]]$value, y = tmp[[1]]$height, df = 20)
 # plot(tmp[[1]]$height, type = "l")
 # plot(lo$y, type = "l")
lo$y[lo$y<0] <- 0
tmp[[1]]$height <- lo$y
lo <- smooth.spline(x = tmp[[2]]$value, y = tmp[[2]]$height)
tmp[[2]]$height <- lo$y
tmp %>% bind_rows() -> stacked

stacked %>%   
  ggplot(.)+
  #geom_line(size = 2)+
  geom_area(position = "stack",
            alpha = 0.25,
            size = 1,
            aes(x = value, 
                y = height,
                group = status,
                color = status,
                fill = status))+
  geom_segment(aes(x = 0,
                   xend = 22,
                   y = 0,
                   yend = 0),
               color = "white",
               size = 1)+
  cowplot::theme_cowplot() +
  scale_fill_manual(values = c("#00b159","#d11141"))+
  scale_color_manual(values = c("#00b159","#d11141"))+
  xlab("Serial Interval (days)") + ylab("Relative Frequency") +
  theme(legend.position = "top",
        legend.title = element_blank()) +
  scale_y_continuous(breaks = c(0, 50, 100),
                     labels = c(0, 50, 100)/10000)

# stacked %<>% 
#   mutate(height = prob*n,
#          bottom = 0) %>% 
#   mutate(height = if_else(status == "Symptomatic",
#                           as.double(n),
#                           height))
# for(i in 1:length(val)){
#   stacked[which(stacked$value == val[i] & stacked$status == "Symptomatic"),"bottom"] <- 
#     stacked[which(stacked$value == val[i] & stacked$status == "Pre-symptomatic"),"height"]
#}


stacked %>% 
  group_by(status) %>% 
  group_split() -> tmp

lo <- smooth.spline(tmp[[1]]$height~tmp[[1]]$value)
tmp[[1]]$height <- lo$y
lo <- smooth.spline(tmp[[2]]$height~tmp[[2]]$value)
tmp[[2]]$height <- lo$y
tmp %>% bind_rows() -> stacked

stacked %>% 
  ggplot(., aes(x = value, y = height, 
                group = status, 
                #color = status,
                fill = status))+
  #geom_area(alpha = 0.5)+
  #geom_line(size = 1.2)+
  geom_area()+
  cowplot::theme_cowplot() +
  scale_fill_manual(values = c("#00b159","#d11141"))+
  xlab("Serial Interval (days)") + ylab("Relative Frequency") +
  theme(legend.position = "top") +
  scale_y_continuous(breaks = c(0, 50, 100),
                     labels = c(0, 50, 100)/10000)

stacked %>%   
ggplot(., aes(x = value, 
              xend = value,
                yend = bottom,
                y = height,
                col = status)) +
  geom_segment(size = 1.2) +
  cowplot::theme_cowplot() +
  scale_color_manual(values = c("#00b159","#d11141"))+
#  scale_color_brewer("", palette = "Paired") +
  xlab("Serial Interval (days)") + ylab("Relative Frequency") +
  theme(legend.position = "top") +
  scale_y_continuous(breaks = c(0, 50, 100),
                     labels = c(0, 50, 100)/10000)
  
serial_sample %>% 
  table %>% 
  enframe %>% 
  mutate(tot = sum(value)) %>% View()


# for(i in 1:n){
#   tmp_1 <- sample(serial_sample,1)
#   tmp_2 <- length(which(incubation_sample > tmp_1))/length(incubation_sample)
#   random[i] <- tmp_2 #if_else(tmp_2 > tmp_1, "pre-symptomatic","symptomatic")
# }
# a <- sample(serial_sample,length(serial_sample), replace = T)
# b <- sample(incubation_sample,length(serial_sample), replace = T)
# for(i in 1:n){
#   tmp_1 <- a[i]
#   tmp_2 <- b[i]
#   random[i] <- if_else(tmp_2 > tmp_1, "pre-symptomatic","symptomatic")
# }


random %>% table
2641/10000


#(which(random == "pre-symptomatic") %>% length)/n

quantile(random, c(0.025, 0.975))
median(random)
mean(random)


m <- 1000
correlated <- rep(NA, m)
for(j in 1:m){
  a <- sort(sample(serial_sample, n,replace = T))
  b <- sort(sample(incubation_sample, n,replace = T))
  point <- rep(NA,n)
  for(i in 1:n){
    tmp_1 <- a[i]
    tmp_2 <- b[i]
    point[i] <- if_else(tmp_2 > tmp_1, "pre-symptomatic","symptomatic")
    #length(which(incubation_sample > tmp_1))/length(incubation_sample)
    #if_else(tmp_2 > tmp_1, "pre-symptomatic","symptomatic")
  }
  correlated[j] <- (which(point == "pre-symptomatic") %>% length)/n
}
hist(correlated)

m <- 1000
anticorrelated <- rep(NA, m)
for(j in 1:m){
  a <- sort(sample(serial_sample,
                   n,
                   replace = T))
  b <- rev(sort(sample(incubation_sample, 
                       n,
                       replace = T)))
  point <- rep(NA,n)
  for(i in 1:n){
    tmp_1 <- a[i]
    tmp_2 <- b[i]
    point[i] <- if_else(tmp_2 > tmp_1, "pre-symptomatic","symptomatic")
    #length(which(incubation_sample > tmp_1))/length(incubation_sample)
    #if_else(tmp_2 > tmp_1, "pre-symptomatic","symptomatic")
  }
  anticorrelated[j] <- (which(point == "pre-symptomatic") %>% length)/n
}
hist(anticorrelated)

save(random, correlated, anticorrelated, file = "pre-symtompatic.RData")
load("pre-symtompatic.RData")

plot(density(random, from=0,to=1, bw=0.1), 
     col = "blue",
     lwd = 2,
     main = "",
     xlab = "Probability of a Secondary Case Infected by Pre-symptomatic Individual", cex.lab = 1.2)

# logspline::logspline(random, lbound = 0) -> random_rescaled
# x <- density(random, bw = 0.0)
# x$y[x$x < 0] <- 0
# x$y[x$x > 1] <- 0
# mark <- which(x$y != 0)
# mark <- sort(unique(c(mark,mark-1)))
# x$x <- x$x[mark]
# x$y <- x$y[mark]
# 
# plot(x, main = "", xlab = "Probability of a Secondary Case Infected During Pre-Symptomatic Phase", lwd = 2)


quantile(correlated,c(0.025, 0.5, 0.975))
quantile(anticorrelated,c(0.025, 0.5, 0.975))
quantile(random, c(0.25, 0.75))

# correlated <- rep(NA,n)
# for(i in 1:n){
#   tmp_1 <- sort(sample(serial_sample, 10000,replace = T))
#   #tmp_2 <- sort(incubation_sample)[1]
#   correlated[i] <- length(which(incubation_sample > tmp_1))/length(incubation_sample)
#     #if_else(tmp_2 > tmp_1, "pre-symptomatic","symptomatic")
# }
# (which(correlated == "pre-symptomatic") %>% length)/n
