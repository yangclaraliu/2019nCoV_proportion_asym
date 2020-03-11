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
  # geom_hline(yintercept = 0.1,
  #            color = "white",
  #            size = 2) +
  # geom_vline(xintercept = 21,
  #            color = "white",
  #            size = 2)+
  cowplot::theme_cowplot() +
  scale_fill_manual(values = c("#56B4E9","#D55E00")) +
  scale_color_manual(values = c("#56B4E9","#D55E00")) +
  xlab(" ") + ylab(" ") +
  theme(legend.position = "top",
        legend.title = element_blank(),
        plot.margin = margin(0.1,0.1,0.1,0.1,"cm")) +
  scale_y_continuous(breaks = c(0, 50, 100, 150, 200, 250),
                     labels = c("0.00", "0.05", "0.10", "0.15", "0.20", 0.25),
                     limits = c(0,250)) +
  xlim(0, 21)-> p
p
