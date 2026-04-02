
ic_plot <- function(x) {

x %>% 
  #dplyr::select(4:8) %>% 
  separate(Title, c("Model", "Class"), sep = "With") %>% 
  separate(Class, "Class", sep = "Classes") %>% 
  pivot_longer(`BIC`:`AWE`,
               names_to = "Index",
               values_to = "ic_value") %>%  
mutate(Index = factor(Index,
                      levels = c ("AWE", "CAIC", "BIC", "aBIC"))) %>% 
  ggplot(aes(x = Class, y = ic_value,
             color = Index, shape = Index,
             group = Index, lty = Index)) +
  geom_point(size = 2.0) + geom_line(linewidth = .8) +
  scale_colour_grey(end = .5) +
  theme_cowplot() +
  labs(x = "Number of Profiles", y = "Information Criteria Value") +
  theme(legend.title = element_blank(),
        legend.position = "top") +
  facet_wrap(~Model, scales = "free_y")

}