#Skeptical prior parameters
mean_skeptical_prior = 0 # Prior ARD mean
sd_skeptical_prior = sd_data_prior # Prior SD

#Optimistic prior parameters

mean_optimistic_prior = -0.05   # Prior ARD mean
sd_optimistic_prior = sd_data_prior # Prior SD

#Pessimistic prior parameters

mean_pessimistic_prior = 0.05  # Prior ARD mean
sd_pessimistic_prior = sd_data_prior # Prior SD

#Non-informative prior parameters

mean_noninformative_prior = 0  # Prior ARD mean
sd_noninformative_prior = 10 # Prior SD

#### Plot

pal = c("Cético" = "#D49352", 
        "Otimista" = "#479ED0",
        "Pessimista" = "#A64E49",
        "Não informativa" = "#85AD99") 

plot_prior = ggplot(data = data.frame(x = c(-0.2, 0.2)), aes(x)) +
  
  stat_function(fun = dnorm, n = 1000,
                args = list(mean = mean_skeptical_prior,
                            sd = sd_skeptical_prior), aes(colour = "Cético"),
                linetype = 1, size = 1) + 
  
  stat_function(fun = dnorm, n = 1000,
                args = list(mean = mean_optimistic_prior,
                            sd = sd_optimistic_prior), aes(colour = "Otimista"),
                linetype = 1, size = 1) +
  
  stat_function(fun = dnorm, n = 1000,
                args = list(mean = mean_pessimistic_prior,
                            sd = sd_pessimistic_prior), aes(colour = "Pessimista"),
                linetype = 1, size = 1) +
  
  stat_function(fun = dnorm, n = 1000,
                args = list(mean =mean_noninformative_prior,
                            sd = sd_noninformative_prior), aes(colour = "Não informativa"),
                linetype = 1, size = 1) + 
  geom_vline(xintercept = 0, color="grey70", linetype = 2) +
  
  scale_colour_manual("Priors",values = pal) +   # legend
  
  # to split legend in two rows https://ggplot2.tidyverse.org/reference/guide_legend.html
  guides(col = guide_legend(nrow = 2)) +  
  
  labs(x="Diferença entre os grupos", y = "Densidade") +
  
  scale_x_continuous(breaks=seq(from = -0.2, to = 0.2, 0.1)) +
  scale_y_continuous(limits=c(0, 13)) +
  
  theme_classic() +
  theme(axis.line.y=element_blank(),
        axis.title = element_text(size = 13),
        axis.text.y=element_blank(),
        axis.text.x = element_text(size = 11),
        axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = "top",
        plot.margin = margin(25,25,10,25)) +
  
  coord_cartesian(expand = T, clip = 'off')

plot_prior