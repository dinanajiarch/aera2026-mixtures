
# This code is adapted from the source code for the `plotLTA` function found in from the `MplusAutomation` package:
# Link to MplusAuotomation source code:
# https://github.com/michaelhallquist/MplusAutomation/blob/995d1ecfae3656524153456ce647f86fe8c1cf1e/R/mixtures.R

# mplus_model <- readModels(filefilter = "sci_attitude", quiet = TRUE)

plot_transition <- function(model_name,facet_labels,timepoint_labels, class_labels){
  node_stroke <- 2
  edges <- model_name$class_counts$transitionProbs
  
  # Build coordinates
  all_classes      <- unique(c(edges$from, edges$to))
  latent_variables <- unique(gsub("\\..+$", "", all_classes))
  edges$x    <- as.numeric(factor(gsub("\\..+$", "", edges$from), levels = latent_variables))
  edges$xend <- as.numeric(factor(gsub("\\..+$", "", edges$to),   levels = latent_variables))
  edges$y    <- as.numeric(gsub("^.+\\.", "", edges$from))
  edges$yend <- as.numeric(gsub("^.+\\.", "", edges$to))
  
  # Compute midpoints and labels
  edges <- edges %>%
    dplyr::mutate(
      trans_perc = round(probability * 100, 0),
      y_mid      = y + ((yend - y) / 2)
    ) %>%
    # add class_label so ribbons can be colored
    dplyr::mutate(
      class_label = factor(
        as.character(y),
        levels = as.character(1:4),
        #labels = names(color_palette)
      )
    )
  
  # Build node list from both ends
  nodes <- rbind(
    edges[, c("from","x","y")],
    setNames(edges[, c("to","xend","yend")], c("from","x","y"))
  ) %>% dplyr::distinct()
  names(nodes)[1] <- "nodeID"
  
  # Pull in class proportions
  n_prop <- model_name$class_counts$modelEstimated %>%
    dplyr::mutate(percent = round(proportion * 100, 0))
  n_prop$proportion <- node_stroke * n_prop$proportion * inverse.rle(list(
    lengths = rle(n_prop$variable)$lengths,
    values  = rle(n_prop$variable)$lengths
  ))
  n_prop$nodeID <- paste(n_prop$variable, n_prop$class, sep=".")
  nodes <- merge(nodes, n_prop, by="nodeID")
  nodesize <- max(max(nchar(nodes$nodeID)) * 4.5, 6)
  
  nodes2 <- nodes %>%
    dplyr::rename(x1 = x, y1 = y) %>%
    dplyr::mutate(class = factor(class)) %>%
    dplyr::mutate(
      class_label = factor(as.character(class),
                           levels = as.character(1:4),
                           #labels = names(color_palette)
                           )
    )
  
p <-  ggplot() +
    geom_segment(
      data = edges,
      aes(
        x = x, y = y, xend = xend, yend = yend,
        size  = probability,
        alpha = probability,
        color = class_label    # color ribbons by starting class
      ),
      show.legend = FALSE
    ) +
    #scale_color_manual(values = rev(color_palette)) +
    scale_alpha_continuous(range = c(.05, .5)) +
    facet_wrap(
      ~ y,
      labeller = labeller(y = function(i)
        stringr::str_wrap(facet_labels[as.character(i)], width = 40))
    ) +
    scale_x_continuous(
      expand = c(.1, .1),
      labels = timepoint_labels,
      breaks = seq_along(unique(timepoint_labels))
      
    ) +
    scale_y_reverse(
      expand = c(.15, .15),
      breaks = seq_along(unique(class_labels)),
      labels = class_labels,
      position = "right"
    ) +
    ggrepel::geom_text_repel(
      data = edges,
      aes(x = xend, y = y_mid, label = glue::glue("{trans_perc}%")),
      segment.colour = NA, nudge_x = -.5
    ) +
    geom_point(
      data = nodes2,
      shape = 21, size = nodesize, color = "black",
      aes(x = x1, y = y1, stroke = proportion, fill = class_label),
      show.legend = FALSE
    ) +
    #scale_fill_manual("", values = rev(color_palette)) +
    geom_text(
      data = nodes2,
      aes(x = x1, y = y1, label = paste0(percent, "%"))
    ) +
    theme_classic() + 
    labs(x = NULL, y = NULL) +
    theme(
      legend.position        = "none",
      panel.grid.minor.x     = element_blank()
    ) +
    theme(
      legend.position = "none",
      panel.grid.minor.x = element_blank(),
      text = element_text(family = "serif", size = 20, color = "black"),
      axis.text.x = element_text(vjust = 1, color = "black"),
      plot.subtitle = element_text(face = "italic", size = 20),
      plot.title = element_text(size = 20),
      axis.line.y=element_blank(),
      axis.text.y = element_text(color = "black"),
    ) 
  p
  return(p) 
}

# plot_transitions_function(
#   model_name = mplus_model,
#   color_pallete = pnw_palette("Bay", n=4, type = "discrete"),
#   facet_labels =c(
#     `1` = "Transitions to 10th Grade from the Pro-Science w/Elevated Utility Class",
#     `2` = "Transitions to 10th Grade from the Ambivalent w/Elevated Utility Class",
#     `3` = "Transitions to 10th Grade from the Ambivalent w/Minimal Utility Class",
#     `4` = "Transitions to 10th Grade from the Anti-Science w/Minimal Utility Class"),
#   timepoint_labels = c('1' = "7th Grade", '2' = "10th Grade"),
#   class_labels = c(
#     "Pro-Science / Elev. Utility",
#     "Ambivalent / Elev. Utility",
#     "Ambivalent / Min. Utility",
#     "Anti-Science / Min. Utility")
# )






