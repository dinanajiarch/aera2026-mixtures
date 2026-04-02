create_flextable <- function(data, sample_size) {
library(flextable)
  library(officer)
    
  # Keep structure consistent with your other tables
  data <- data %>%
    select(
      Class_Model, Best_LogLikelihood, npar, 
      Random_Start1, Random_Start2, 
      Converged, Perc_Convergence, 
      Replicated_LL, Replicated_LL_Perc, 
      Smallest_Class, Smallest_Class_Perc
    ) %>%
    as.data.frame()
  
  # Create the flextable
  summary_table <- flextable(data) %>%
    set_header_labels(
      Class_Model = "Model",
      Best_LogLikelihood = "Best LL",
      npar = "npar",
      Random_Start1 = "Initial",
      Random_Start2 = "Final",
      Converged = "𝒇",
      Perc_Convergence = "%",
      Replicated_LL = "𝒇",
      Replicated_LL_Perc = "%",
      Smallest_Class = "𝒇",
      Smallest_Class_Perc = "%"
    )
  
  summary_table <- compose(summary_table, part = "header", j = "Converged", value = as_paragraph(as_i("f")))
  summary_table <- compose(summary_table, part = "header", j = "Perc_Convergence", value = as_paragraph("%"))
  summary_table <- compose(summary_table, part = "header", j = "Replicated_LL", value = as_paragraph(as_i("f")))
  summary_table <- compose(summary_table, part = "header", j = "Replicated_LL_Perc", value = as_paragraph("%"))
  summary_table <- compose(summary_table, part = "header", j = "Smallest_Class", value = as_paragraph(as_i("f")))
  summary_table <- compose(summary_table, part = "header", j = "Smallest_Class_Perc", value = as_paragraph("%"))
  
  
  # **Apply column widths**
  summary_table <- summary_table %>%
    width(j = "Class_Model", width = 0.7) %>%
    width(j = "Best_LogLikelihood", width = 0.8) %>%
    width(j = "npar", width = 0.4) %>%
    width(j = c("Random_Start1", "Random_Start2"), width = 0.5) %>%
    width(j = c("Converged", "Perc_Convergence"), width = 0.5) %>%
    width(j = c("Replicated_LL", "Replicated_LL_Perc"), width = 0.5) %>%
    width(j = c("Smallest_Class", "Smallest_Class_Perc"), width = 0.5) %>%
    
    # **Center align everything**
    align(align = "center", part = "all") %>%
    valign(valign = "bottom", part = "header") %>%
    
    # **Apply font**
    font(fontname = "Avenir Next", part = "all")
  
  # **🚀 Add Superheader**
  summary_table <- add_header_row(
    summary_table,
    values = c(glue("N = {sample_size}"), 
               "Random Starts", 
               "Final starting value sets converging",
               "LL Replication", 
               "Smallest Class"),
    colwidths = c(3, 2, 2, 2, 2)  # ✅ Fixed colwidths
  )
  # Left-align only the sample size in the superheader
  summary_table <- align(summary_table, j = 1, align = "left", part = "header")
  
  # Italicize N and ensure sample_size is displayed as an integer
  summary_table <- compose(summary_table, part = "header", i = 1, j = 1, 
                           value = as_paragraph(as_i("N"), " = ", (as.character(sample_size))))
  
  ##*Fully remove the horizontal line under the entire superheader**
  no_border <- fp_border(color = "transparent", width = 0)
  summary_table <- hline(summary_table, i = 1, j = 1:11, border = no_border, part = "header")  
  
  # **Apply percentage formatting to the percentage columns**
  summary_table <- colformat_num(
    summary_table,
    j = c("Perc_Convergence", "Replicated_LL_Perc", "Smallest_Class_Perc"),  # Columns to apply %
    suffix = "%"  # Add percentage symbol to these columns
  )
  
  # **Ensure the entire superheader row has a gray background**
  summary_table <- bg(summary_table, i = 1, bg = "#f0f0f0", part = "header")  # ✅ Light gray for the entire first row
  
  # **Define alternating row colors (light gray every other row after superheader)**
  total_rows <- nrow(data)  # Get the total number of rows
  color_rows <- rep(FALSE, total_rows)  # Initialize logical vector
  
  # Start alternating from the first row **after** the superheader
  color_rows[seq(1, total_rows, by = 2)] <- TRUE  # ✅ Every other row after superheader
  
  # **Apply background color to every other row**
  summary_table <- bg(summary_table, i = color_rows, bg = "#f0f0f0", part = "body")
  
  # **🔥 Apply Heatmap to `Perc_Convergence`**
  coral_shades <- colorRampPalette(c("#E44D26", "#FFECE6"))  # Coral → Light Coral
  
  # Normalize values from 0 to 1 for color scaling
  perc_min <- min(data$Perc_Convergence, na.rm = TRUE)
  perc_max <- max(data$Perc_Convergence, na.rm = TRUE)
  perc_scaled <- (data$Perc_Convergence - perc_min) / (perc_max - perc_min)
  
  # Convert normalized values to hex colors
  perc_colors <- coral_shades(100)[as.numeric(cut(perc_scaled, breaks = 100))]
  
  # **🚀 Adjust font color dynamically for Perc_Convergence**
  font_colors <- ifelse(perc_scaled < 0.5, "white", "black")  # Dark background = White text
  
  # **Ensure 100% stays gray in alternating rows for Perc_Convergence**
  perc_colors[data$Perc_Convergence == 100 & color_rows] <- "#f0f0f0"
  font_colors[data$Perc_Convergence == 100] <- "black"
  
  # Apply the heatmap colors & font colors to Perc_Convergence
  summary_table <- bg(summary_table, j = "Perc_Convergence", bg = perc_colors, part = "body")
  summary_table <- color(summary_table, j = "Perc_Convergence", color = font_colors, part = "body")
  
  
  # **🔥 Apply Heatmap to `Replicated_LL_Perc`**
  coral_shades_ll <- colorRampPalette(c("#E44D26", "#FFECE6"))  # Coral → Light Coral
  
  # Normalize values from 0 to 1 for color scaling
  ll_min <- min(data$Replicated_LL_Perc[data$Replicated_LL_Perc < 100], na.rm = TRUE)
  ll_max <- max(data$Replicated_LL_Perc[data$Replicated_LL_Perc < 100], na.rm = TRUE)
  ll_scaled <- (data$Replicated_LL_Perc - ll_min) / (ll_max - ll_min)
  
  # Convert normalized values to hex colors
  ll_colors <- coral_shades_ll(100)[as.numeric(cut(ll_scaled, breaks = 100))]
  
  # **🚀 Adjust font color dynamically for Replicated_LL_Perc**
  font_colors_ll <- ifelse(ll_scaled < 0.5, "white", "black")  # Dark background = White text
  
  # **Ensure 100% stays gray in alternating rows for Replicated_LL_Perc**
  ll_colors[data$Replicated_LL_Perc == 100 & color_rows] <- "#f0f0f0"
  ll_colors[data$Replicated_LL_Perc == 100 & !color_rows] <- "white"
  font_colors_ll[data$Replicated_LL_Perc == 100] <- "black"
  
  # Apply the heatmap colors & font colors to Replicated_LL_Perc
  summary_table <- bg(summary_table, j = "Replicated_LL_Perc", bg = ll_colors, part = "body")
  summary_table <- color(summary_table, j = "Replicated_LL_Perc", color = font_colors_ll, part = "body")
  # **🔥 Apply Heatmap to `Replicated_LL_Perc`**
coral_shades_ll <- colorRampPalette(c("#E44D26", "#FFECE6"))  # Coral → Light Coral

# Normalize values from 0 to 1 for color scaling
ll_min <- min(data$Replicated_LL_Perc[data$Replicated_LL_Perc < 100], na.rm = TRUE)
ll_max <- max(data$Replicated_LL_Perc[data$Replicated_LL_Perc < 100], na.rm = TRUE)
ll_scaled <- (data$Replicated_LL_Perc - ll_min) / (ll_max - ll_min)

# Convert normalized values to hex colors
ll_colors <- coral_shades_ll(100)[as.numeric(cut(ll_scaled, breaks = 100))]

# **🚀 Adjust font color dynamically for Replicated_LL_Perc**
font_colors_ll <- ifelse(ll_scaled < 0.5, "white", "black")  # Dark background = White text

# **Ensure 100% stays gray in alternating rows for Replicated_LL_Perc**
ll_colors[data$Replicated_LL_Perc == 100 & color_rows] <- "#f0f0f0"
ll_colors[data$Replicated_LL_Perc == 100 & !color_rows] <- "white"
font_colors_ll[data$Replicated_LL_Perc == 100] <- "black"

# Apply the heatmap colors & font colors to Replicated_LL_Perc
summary_table <- bg(summary_table, j = "Replicated_LL_Perc", bg = ll_colors, part = "body")
summary_table <- color(summary_table, j = "Replicated_LL_Perc", color = font_colors_ll, part = "body")

  return(summary_table)  # ✅ Now return the properly formatted flextable
  
}
