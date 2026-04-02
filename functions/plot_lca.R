

library(MplusAutomation)
library(tidyverse)
library(reshape2)
library(cowplot)
library(glue)



plot_lca <- function(model_name) {
  
  pp_plots <- data.frame(model_name$parameters$probability.scale) %>%
    mutate(LatentClass = sub("^", "Class ", LatentClass)) %>%
    filter(category == 2) %>%
    dplyr::select(est, LatentClass, param) %>%
    pivot_wider(names_from = LatentClass, values_from = est) %>%
    relocate(param, .after = last_col())
  
  c_size <- as.data.frame(model_name$class_counts$modelEstimated$proportion) %>%
    rename("cs" = 1) %>%
    mutate(cs = round(cs * 100, 2))
  
  colnames(pp_plots) <- paste0(colnames(pp_plots[, 1:ncol(pp_plots) - 1]),
                               glue(" ({c_size[1:ncol(pp_plots)-1,]}%)"))
  
  plot_data <- pp_plots %>%
    rename("param" = ncol(pp_plots)) %>%
    reshape2::melt(id.vars = "param") %>%
    mutate(param = fct_inorder(param), value = as.numeric(value))
  
  name <- model_name$input$title
  
  p <- plot_data %>%
    ggplot(
      aes(
        x = param,
        y = value,
        colour = variable,
        group = variable
      )
    ) +
    geom_point(size = 4) + geom_line() +
    geom_text(aes(label = round(value, 2)),  
              vjust = -1,                  
              size = 4,
              show.legend = FALSE) +
    ylim(0, 1) +
    scale_x_discrete("") +
    labs(title = glue("{name} Probability Plot"), y = "Probability") +
    theme_cowplot() +
    theme(
      text = element_text(family = "serif", size = 15),
      legend.text = element_text(family = "serif", size = 15),
      legend.key.width = unit(0, "line"),
      legend.title = element_blank(),
      legend.position = "top",
      axis.text.x = element_text(vjust = 1)
    ) +
    guides(
      colour = guide_legend(nrow = 2, byrow = TRUE),
      shape  = guide_legend(nrow = 2, byrow = TRUE),
      linetype = guide_legend(nrow = 2, byrow = TRUE)
    )
  p
  return(p)
}