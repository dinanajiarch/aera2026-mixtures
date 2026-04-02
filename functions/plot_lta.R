
library(MplusAutomation)
library(tidyverse)
library(reshape2)
library(cowplot)
library(glue)
library(patchwork)
    


plot_lta <- function(model_name) {
  
  # Extract probabilities
  pp_plots <- model_name$parameters$probability.scale %>% 
    mutate(
      time = str_replace(str_extract(LatentClass, "C\\d+"), "C", "T"),
      class = paste("Class", str_extract(LatentClass, "(?<=#)\\d+"))
    ) %>% 
    filter(category == 2) %>% 
    dplyr::select(est, time, class, param) %>% 
    relocate(param, .after = last_col()) %>%
    mutate(key = paste0(time, "C", str_extract(class, "\\d+")))

  # Extract class size  
  c_size <- as.data.frame(model_name$class_counts$modelEstimated) %>%
    mutate(variable = str_replace(variable, "^C", "T")) %>% 
    rename("cs" = 4) %>% 
    mutate(cs = round(cs * 100, 2)) %>%
    mutate(class = paste0("C", class)) %>% 
    mutate(key = paste0(variable, class))
  
  # Join probability data with class sizes
  plot_data <- pp_plots %>%
    left_join(c_size %>% select(key, cs), by = "key") %>%
    mutate(class = paste0(class, " (", cs, "%)")) %>%
    select(-key, -cs) %>%  
    mutate(param = fct_inorder(param),
           est = as.numeric(est))

  name <- model_name$input$title

  # Function to make individual plots
  make_plot <- function(df, time_label) {
    ggplot(df, aes(
      x = param,
      y = est,
      colour = class,
      group = class
    )) +
      geom_point(size = 4) +
      geom_line() +
      geom_text(aes(label = round(est, 2)),  
                vjust = -1,                  
                size = 4,
                show.legend = FALSE) +
      ylim(0, 1) +
      scale_x_discrete("") +
      labs(title = glue("{name} Probability Plot - {time_label}"), y = "Probability") +
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
  }
  
  # Split data by time
  plot_list <- split(plot_data, plot_data$time)
  
  # Create one plot per time point dynamically
  plots <- lapply(names(plot_list), function(t) {
    make_plot(plot_list[[t]], t)
  })
  
  names(plots) <- names(plot_list)
  
  # Combine all plots side by side
  combined_plot <- wrap_plots(plots, ncol = length(plots))
  
  return(combined_plot)
}

