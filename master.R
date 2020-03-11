
library(tidyverse)
library(ggpubr)
rm(list=ls())

setwd("~/GitHub/2019nCoV_proportion_asym")
n <- 10000
source("0_Import.R")
SI_tmp <- serial_sample_Bi$late #serial_sample_Bi$late #serial_sample_Bi$all
IP_tmp <- incubation_sample_Bi
  #incubation_sample_Bi
source("1_sample.R")
source("2_plot.R")

# mean(SI_tmp)
# mean(IP_tmp)
# p1 <- p
# p1
# 
# rm(list=setdiff(ls(), "p1"))
# n <- 10000
# source("0_Import.R")
# SI_tmp <- serial_sample_Bi$all#serial_sample_Bi$late #serial_sample_Bi$all
# IP_tmp <- incubation_sample_Bi
# #incubation_sample_Bi
# source("1_sample.R")
# source("2_plot.R")
# p2 <- p
# 
# ggarrange(p1, p2, ncol = 1,
#           common.legend = T,
#           labels = c("A",
#                      "B")) -> p
# 
# annotate_figure(p,
#                 left = "Probability Density",
#                 bottom = "Serial Interval (Days)")
# 
# ggsave("Prob_Asym_Stacked.png",
#        width = 6.77,
#        height = 5,
#        dpi = 800)

source("3_corr.R")
