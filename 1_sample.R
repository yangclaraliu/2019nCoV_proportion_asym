###sampling
### Stacked barplot (but really it's segment plot)
expand.grid(seq(0,35,by=0.1),c("Symptomatic", "Pre-symptomatic")) %>% 
  as_tibble %>% 
  setNames(c("duration","status")) %>% 
  arrange(duration) %>% 
  mutate(prob = NA) -> stacked

SI_tmp %>%
  enframe %>% 
  group_by(value) %>% 
  tally %>% 
  right_join(stacked, by = c("value" = "duration")) %>% 
  filter(!is.na(n))  -> stacked

val <- unique(stacked$value) %>% as.numeric %>% sort

for(i in 1:length(val)){
  stacked[stacked$value == val[i], "prob"] <- 
    c(1-length(which(IP_tmp > val[i]))/length(IP_tmp),
      length(which(IP_tmp > val[i]))/length(IP_tmp))
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
