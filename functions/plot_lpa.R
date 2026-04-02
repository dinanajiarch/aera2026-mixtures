
plot_lpa <-
  function(model_name) {
    library(MplusAutomation)
    library(tidyverse)
    library(reshape2)
    library(cowplot)
    library(glue)
    
    extract_plot_data <- data.frame(model_name$parameters$unstandardized) %>%
      mutate(LatentClass = sub("^", "Class ", LatentClass)) %>%
      filter(paramHeader == "Means") %>%
      filter(LatentClass != "Class Categorical.Latent.Variables") %>%
      select(est, se, LatentClass, param) %>%
      mutate(class_num = as.integer(str_extract(LatentClass, "\\d+")))
    
    c_size <- as.data.frame(model_name$class_counts$modelEstimated$proportion) %>%
      rename(cs = 1) %>%
      mutate(cs = round(cs * 100, 2), class_num = row_number())
    
    plot_data <- extract_plot_data %>%
      left_join(c_size, by = "class_num") %>%
      mutate(class = paste0("Class ", class_num, " (", cs, "%)")) %>%
      select(est, se, class, param) %>%
      mutate(param = fct_inorder(param))
    
    min <- as.data.frame(model_name$sampstat$univariate.sample.statistics) %>% 
      select(Minimum) %>% 
      slice(1) %>% 
      as.matrix()
    
    max <- as.data.frame(model_name$sampstat$univariate.sample.statistics) %>% 
      select(Maximum) %>% 
      slice(1) %>% 
      as.matrix()
    
    title <- str_to_title(model_name$input$title)
    
 p <-   plot_data %>%
      ggplot(aes(
        x = param,
        y = est,
        colour = class,
        group = class
      )) +
      geom_point(size = 4) + geom_line() +
      geom_errorbar(aes(ymin = est - se, ymax = est + se), width = 0.2) +
      scale_x_discrete(
        "",
        labels = function(x)
          str_wrap(x, width = 10)
      ) +
      ylim(min,max) +
      labs(title = glue("{title}"), y = "Means") +
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
        colour = guide_legend(nrow = 1, byrow = TRUE),
        shape  = guide_legend(nrow = 1, byrow = TRUE),
        linetype = guide_legend(nrow = 1, byrow = TRUE)
      )
    return(p)
  }
  
  
  