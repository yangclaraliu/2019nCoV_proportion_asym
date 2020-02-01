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