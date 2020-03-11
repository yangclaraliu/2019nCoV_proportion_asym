density(serial_sample_Li) -> tmp
cbind(tmp$x, tmp$y, "Li", "SI", "no_intervention") -> part_1

density(serial_sample_Bi$late) -> tmp
cbind(tmp$x, tmp$y, "Bi", "SI", "no_intervention") -> part_2

density(serial_sample_Bi$all) -> tmp
cbind(tmp$x, tmp$y, "Bi", "SI","intervention") -> part_6

density(serial_sample_Nishiura) -> tmp
cbind(tmp$x, tmp$y, "Nishiura", "SI","intervention") -> part_3

density(incubation_sample_Bi) -> tmp
cbind(tmp$x, tmp$y, "Bi", "IP","intervention") -> part_4

density(incubation_sample_Li) -> tmp
cbind(tmp$x, tmp$y, "Li", "IP","no_intervention") -> part_5

metric_labels <- c("Incubation Period",
                   "Serial Interval")
names(metric_labels) <- c("IP", "SI")

rbind(part_1, part_2, part_3, part_4, part_5, part_6) %>% 
  as_tibble %>% 
  setNames(c("x", "y", "study","metric","status")) %>% 
  mutate(x = as.numeric(x),
         y = as.numeric(y),
         study = factor(study),
         metric = factor(metric),
         status = factor(status)) %>%
  ggplot(.) +
  geom_line(aes(x = x, 
                y = y, 
                group = interaction(study, status),
                color = study,
                linetype = status), 
            size = 1.05) +
  cowplot::theme_cowplot() +
  scale_color_manual(values = c("#56B4E9",
                                "#0072B2",
                                "#009E73"),
                     labels = c("Bi et al.",
                                "Li et al.", 
                                "Nishiura, Linton \n& Akhmetzhanov")) +
  # scale_color_npg(labels = c("Bi et al.", 
  #                            "Li et al.",
  #                            "Nishiura, Linton & Akhmetzhanov")) +
  xlim(0,21) +
  labs(x = "Duration (Days)",
       y = "Probability Density") +
  theme(legend.position = "top",
        legend.title = element_blank()) +
  facet_wrap(~metric, 
             ncol = 1,
             labeller = labeller(metric = metric_labels)) +
  scale_linetype_manual(values = c("dashed", "solid"),
                        labels = c("w/ Intervention",
                                   "w/o Intervention")) + 
  guides(linetype = guide_legend(override.aes = list(size = 0.5))) +
  ylim(0, 0.25)

  # geom_vline(xintercept = 7.5, 
  #            linetype = 2,
  #            color = "#56B4E9",
  #            size = 1) +
  # geom_vline(xintercept = 8, 
  #            linetype = 2,
  #            color = "#0072B2",
  #            size = 1) +
  # geom_vline(xintercept = 4.6, 
  #            linetype = 2,
  #            color = "#009E73",
  #            size = 1)

ggsave("pdf_ByStudy.png",
       width = 8,
       height = 6,
       dpi = 800)
