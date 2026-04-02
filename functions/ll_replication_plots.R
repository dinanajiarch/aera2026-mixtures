generate_ll_replication_plots <- function(final_data) {
  ll_replication_tables <- list()
  
  for (i in seq_len(nrow(final_data))) {
    file_name <- final_data$File_Name[i]
    class_model <- final_data$Class_Model[i]
    log_likelihoods <- final_data$LogLikelihoods[[i]]
    
    value_counts <- tibble(LogLikelihood = log_likelihoods) %>%
      group_by(LogLikelihood) %>%
      summarise(Count = n(), .groups = "drop") %>%
      mutate(Percentage = Count / sum(Count)) %>%
      arrange(desc(LogLikelihood))
    
    table <- gt(value_counts) %>%
      tab_header(
        title = paste0("Log Likelihood Replications: ", class_model),
        subtitle = paste0("Source File: ", file_name)
      ) %>%
      cols_label(
        LogLikelihood = "Log Likelihood",
        Count = "Replication Count",
        Percentage = "% of Valid Replications"
      ) %>%
      fmt_number(columns = c(LogLikelihood, Count), decimals = 3) %>%
      fmt_percent(columns = c(Percentage), decimals = 2) %>%
      cols_align(align = "center", columns = everything()) %>%
      tab_options(
        data_row.padding = px(2),
        table.font.size = px(12),
        table_body.hlines.style = "none"
      ) %>%
      tab_style(
        style = cell_borders(sides = "bottom", weight = px(1)),
        locations = cells_body(rows = nrow(value_counts))
      )
    
    ll_replication_tables[[file_name]] <- table
  }
  
  return(ll_replication_tables)
}

