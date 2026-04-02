# Define a function that takes a data frame as input
create_ll_replication_table_all <- function(data) {
  library(dplyr)
  library(tidyr)
  library(flextable)
  library(stringr)
  
  # ----- STEP 1: Extract, count, and compute replication percentage -----
  ll_data <- data %>%
    select(File_Name, Class_Model, LogLikelihoods) %>%
    unnest(LogLikelihoods) %>%
    count(File_Name, Class_Model, LogLikelihoods, name = "ReplicationCount") %>%
    arrange(File_Name, desc(LogLikelihoods)) %>%
    group_by(Class_Model) %>%
    mutate(ReplicationPerc = round((ReplicationCount / sum(ReplicationCount)) * 100, 1)) %>%
    ungroup()
  
  # ----- STEP 2: Pivot wider (LogLikelihoods, ReplicationCount, ReplicationPerc) -----
  ll_replication_df <- ll_data %>%
    select(-File_Name) %>%
    group_by(Class_Model) %>%
    mutate(id = row_number()) %>%
    pivot_wider(
      names_from = Class_Model,
      values_from = c(LogLikelihoods, ReplicationCount, ReplicationPerc),
      names_glue = "{.value}_{Class_Model}"
    ) %>%
    ungroup() %>%
    select(-id)
  
  # ----- STEP 3: Forcibly reorder columns by class number (LL → N → Perc) -----
  orig_cols <- colnames(ll_replication_df)
  class_num <- as.numeric(str_extract(orig_cols, "(?<=_)(\\d+)(?=-Class)"))
  col_type <- case_when(
    grepl("^LogLikelihoods_", orig_cols) ~ "LL",
    grepl("^ReplicationCount_", orig_cols) ~ "N",
    grepl("^ReplicationPerc_", orig_cols) ~ "Perc"
  )
  col_info <- data.frame(
    orig_name = orig_cols,
    class_num = class_num,
    col_type = col_type,
    stringsAsFactors = FALSE
  )
  type_order <- c("LL" = 1, "N" = 2, "Perc" = 3)
  col_info <- col_info %>%
    arrange(class_num, type_order[col_type])
  new_names <- mapply(function(cnum, ctype) {
    if (ctype == "LL") {
      paste0("LL_c", cnum)
    } else if (ctype == "N") {
      paste0("n_c", cnum)
    } else {
      paste0("perc_c", cnum)
    }
  }, col_info$class_num, col_info$col_type)
  ll_replication_df <- ll_replication_df[col_info$orig_name]
  colnames(ll_replication_df) <- new_names
  
  # ----- STEP 4: Replace NAs with "—" -----
  ll_replication_df <- ll_replication_df %>%
    mutate(across(everything(), ~ ifelse(is.na(.), "—", .)))
  
  # ----- STEP 5: Create the flextable -----
  all_cols <- names(ll_replication_df)
  num_cols <- length(all_cols)
  num_classes <- num_cols / 3  # 3 columns per class
  
  header_labels <- setNames(
    ifelse(grepl("^LL_", all_cols), "LL",
           ifelse(grepl("^n_", all_cols), "N", "%")),
    all_cols
  )
  
  ll_replication_table_all <- flextable(ll_replication_df) %>%
    set_header_labels(.list = header_labels) %>%
    align(align = "center", part = "all") %>%
    font(fontname = "Avenir Next", part = "all") %>%
    fontsize(size = 10, part = "body") %>%
    fontsize(size = 11, part = "header") %>%
    bold(part = "header", bold = FALSE)
  
  ll_cols <- grep("^LL_", all_cols, value = TRUE)
  n_cols  <- grep("^n_", all_cols, value = TRUE)
  p_cols  <- grep("^perc_", all_cols, value = TRUE)
  
  ll_replication_table_all <- ll_replication_table_all %>%
    width(j = ll_cols, width = 0.7) %>%
    width(j = n_cols, width = 0.3) %>%
    width(j = p_cols, width = 0.3) %>%
    set_table_properties(layout = "fixed", width = 1) %>%
    padding(part = "header", padding = 3) %>%
    padding(part = "body", padding = 3)
  
#  for (col in ll_cols) {
#    ll_replication_table_all <- compose(
#      ll_replication_table_all, 
#      part = "header", j = col, 
#
  #      value = as_paragraph(as_i("LL"))
#    )
 # }
#  for (col in n_cols) {
#   ll_replication_table_all <- compose(
#      ll_replication_table_all, 
#      part = "header", j = col, 
#      value = as_paragraph(as_i("N"))
#    )
#  }
#  for (col in p_cols) {
#    ll_replication_table_all <- compose(
#      ll_replication_table_all, 
#      part = "header", j = col, 
#      value = as_paragraph(as_i("%"))
#    )
#  }
  
  super_headers <- paste0(seq_len(num_classes), "-Class")
  ll_replication_table_all <- add_header_row(
    ll_replication_table_all,
    values = super_headers,
    colwidths = rep(3, num_classes)
  )
  
  ll_replication_table_all <- hline(
    ll_replication_table_all, i = 1, 
    border = fp_border(color = "transparent"), 
    part = "header"
  )
  
  ll_replication_table_all <- ll_replication_table_all %>%
    set_table_properties(layout = "fixed") %>%
    flextable::keep_with_next()
  
  return(ll_replication_table_all)
}
