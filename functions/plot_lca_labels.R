
    library(MplusAutomation)
    library(tidyverse)
    library(reshape2)
    library(cowplot)
    library(glue)
    


plot_lca_labels <-
  function(model_name,item_labels) {

    
pp_plots <- data.frame(model_name$parameters$probability.scale) %>% 
  mutate(LatentClass = sub("^","Class ", LatentClass)) %>% 
  filter(category == 2) %>% 
  dplyr::select(est, LatentClass, param) %>% 
  pivot_wider(names_from = LatentClass, values_from = est) %>% 
  relocate(param, .after = last_col())

pp_plots$param <- tolower(pp_plots$param)
pp_plots$param <- item_labels[pp_plots$param]

c_size <- as.data.frame(model_name$class_counts$modelEstimated$proportion) %>% 
  rename("cs" = 1) %>% 
  mutate(cs = round(cs * 100, 2))

colnames(pp_plots) <- paste0(colnames(pp_plots[,1:ncol(pp_plots)-1]), glue(" ({c_size[1:ncol(pp_plots)-1,]}%)")) 

plot_data <- pp_plots %>% 
  rename("param" = ncol(pp_plots)) %>% 
  reshape2::melt(id.vars = "param") %>% 
  mutate(param = fct_inorder(param),
         value = as.numeric(value))

name <- model_name$input$title

p <- plot_data %>%
  ggplot(aes(
    x = param,
    y = value,
    shape = variable,
    colour = variable,
    lty = variable,
    group = variable
  )) + geom_point(size = 4) +
  geom_line() + 
  scale_x_discrete(
    "",
    labels = function(x)
      str_wrap(x, width = 10)
  ) +
  labs(title = glue("{name} Probability Plot"), y = "Probability") + theme_cowplot()  + 
  theme(
    text = element_text(family = "serif"),
    legend.text = element_text(family = "serif"),
    legend.key.width = unit(0, "line"),
    legend.title = element_blank(),
    axis.text.x = element_text(vjust = 1),
    plot.subtitle = element_text(face = "italic"),
    plot.title = element_text(size = 15),
    legend.position = "top"
  )
p
return(p)
}