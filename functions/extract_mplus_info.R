# Function to extract model information from one Mplus output file
extract_mplus_info_extended <- function(file_path) {
  
  # Read the file as lines
  output <- read_lines(file_path)
  file_name <- basename(file_path)
  
  #### 1. CLASS MODEL
  # Try to extract the model name from the TITLE section (e.g., "3-Class" or "3 Class")
  title_idx <- grep("^\\s*TITLE:", output, ignore.case = TRUE)
  
  if (length(title_idx) > 0) {
    # Find the next section header (DATA:, VARIABLE:, ANALYSIS:, MODEL:)
    end_idx <- grep("^\\s*(DATA|VARIABLE|ANALYSIS|MODEL)\\s*:", output, ignore.case = TRUE)
    next_section <- end_idx[end_idx > title_idx[1]][1]
    
    # Extract all lines between TITLE: and next section
    title_block <- output[(title_idx[1] + 1):(next_section - 1)]
    
    # Collapse multiple lines into a single string and trim whitespace
    class_model <- paste(title_block, collapse = " ") %>% str_squish()
  } else {
    class_model <- NA_character_
  }
  
  
  #### 2. LOG-LIKELIHOOD VALUES (Best, Converged Count, and All Values)
  rs_header_idx <- grep("RANDOM STARTS RESULTS RANKED", output, ignore.case = TRUE)
  best_ll <- NA_real_
  converged <- NA_integer_
  log_likelihoods <- list()  # Store all LL values
  
  if (length(rs_header_idx) > 0) {
    term_idx <- grep("THE MODEL ESTIMATION TERMINATED NORMALLY", output, ignore.case = TRUE)
    if (length(term_idx) == 0) term_idx <- length(output)
    
    # Extract section with log-likelihood values
    rs_block <- output[rs_header_idx[1]:term_idx[1]]
    
    # Look for lines that start with a (possibly negative) decimal number (assumed to be the table rows)
    table_lines <- rs_block[grepl("^\\s*-?\\d+\\.\\d+", rs_block) & grepl("\\s+\\d+", rs_block)]
    
    if (length(table_lines) > 0) {
      parsed <- lapply(table_lines, function(line) {
        parts <- str_split(str_trim(line), "\\s+")[[1]]
        if (length(parts) >= 3) return(parts[1:3]) else return(NULL)
      })
      parsed <- Filter(Negate(is.null), parsed)
      
      if (length(parsed) > 0) {
        parsed_mat <- do.call(rbind, parsed)
        
        # Extract best LL (first row of ranked results)
        best_ll <- as.numeric(parsed_mat[1, 1])
        
        # Count number of converged models (all LL values)
        converged <- nrow(parsed_mat)
        
        # Extract all log-likelihood values
        log_likelihoods <- as.numeric(parsed_mat[, 1])
        
        # Remove extreme logit thresholds
        log_likelihoods <- log_likelihoods[!(log_likelihoods == -15 | log_likelihoods == 15)]
      }
    }
  }
  
  
  #### 2.5 COUNT REPLICATED LOG-LIKELIHOODS AND PERCENTAGE
  replicated_ll <- NA_integer_
  replicated_ll_perc <- NA_real_
  
  if (!is.na(best_ll) && length(parsed_mat) > 0) {
    # Count occurrences of the best log-likelihood
    replicated_ll <- sum(as.numeric(parsed_mat[, 1]) == best_ll, na.rm = TRUE)
    
    # Compute percentage of total LL values
    total_ll_values <- nrow(parsed_mat)
    if (!is.na(total_ll_values) && total_ll_values > 0) {
      replicated_ll_perc <- round((replicated_ll / total_ll_values) * 100, 1)
    }
  }
  
  #### 3. RANDOM STARTS (from the ANALYSIS section) and % CONVERGENCE
  rs_analysis1 <- NA_integer_
  rs_analysis2 <- NA_integer_
  total_starts <- NA_integer_
  analysis_idx <- grep("^\\s*ANALYSIS:", output, ignore.case = TRUE)
  if (length(analysis_idx) > 0) {
    analysis_block <- output[analysis_idx[1]:(analysis_idx[1] + 20)]
    starts_line <- analysis_block[grep("starts\\s*=\\s*\\d+", analysis_block, ignore.case = TRUE)]
    if (length(starts_line) > 0) {
      start_nums <- str_extract_all(starts_line[1], "\\d+")[[1]]
      if (length(start_nums) >= 2) {
        rs_analysis1 <- as.numeric(start_nums[1])
        rs_analysis2 <- as.numeric(start_nums[2])
        total_starts <- rs_analysis2
      } else if (length(start_nums) == 1) {
        rs_analysis1 <- as.numeric(start_nums[1])
        rs_analysis2 <- NA
        total_starts <- NA
      }
    }
  }
  perc_convergence <- if (!is.na(total_starts) && total_starts > 0 && !is.na(converged))
    (converged / total_starts) * 100 else NA_real_
  
  #### 4. npar (NUMBER OF FREE PARAMETERS)
  npar <- NA_integer_
  npar_line <- output[grep("Number of Free Parameters", output, ignore.case = TRUE)]
  if (length(npar_line) > 0) {
    npar <- as.numeric(str_extract(npar_line[1], "\\d+"))
  }
  
  
  #### 5. SMALLEST CLASS and its Percentage (Function Definition)
  extract_smallest_class <- function(output) {
    # Initialize values
    smallest_class <- NA_integer_
    smallest_class_perc <- NA_real_
    
    # Locate the "BASED ON ESTIMATED POSTERIOR PROBABILITIES" section
    fc_idx <- grep("FINAL CLASS COUNTS AND PROPORTIONS FOR THE LATENT CLASSES", output, ignore.case = TRUE)
    posterior_idx <- grep("BASED ON ESTIMATED POSTERIOR PROBABILITIES", output, ignore.case = TRUE)
    
    # Keep only the indices where "BASED ON ESTIMATED POSTERIOR PROBABILITIES" follows immediately
    valid_idx <- intersect(fc_idx + 1, posterior_idx)
    
    if (length(valid_idx) == 0) {
      return(tibble(Smallest_Class = smallest_class, Smallest_Class_Perc = smallest_class_perc))
    }
    
    # Extract the class count block
    fc_block <- output[(valid_idx[1] + 3):(valid_idx[1] + 20)]  # Start 3 lines after header
    
    # Extract rows with three numbers (Class, Count, Proportion)
    fc_lines <- fc_block[grepl("^\\s*\\d+\\s+\\d+\\.\\d+\\s+\\d+\\.\\d+", fc_block)]
    
    if (length(fc_lines) > 0) {
      # Parse extracted lines
      fc_data <- lapply(fc_lines, function(line) {
        parts <- str_split(str_trim(line), "\\s+")[[1]]
        if (length(parts) >= 3) {
          tibble(
            Class = as.integer(parts[1]),
            Count = as.numeric(parts[2]),
            Proportion = as.numeric(parts[3])
          )
        } else {
          NULL
        }
      })
      
      fc_data <- bind_rows(fc_data)
      
      if (nrow(fc_data) > 0) {
        # Identify the smallest class
        min_row <- fc_data %>% filter(Count == min(Count)) %>% slice(1)
        smallest_class <- round(min_row$Count)
        smallest_class_perc <- round(min_row$Proportion * 100, 1)
      }
    }
    
    return(tibble(Smallest_Class = smallest_class, Smallest_Class_Perc = smallest_class_perc))
  }
  
  #### 5.5 SMALLEST CLASS and its Percentage
  smallest_class_info <- extract_smallest_class(output)
  
  smallest_class <- smallest_class_info$Smallest_Class
  smallest_class_perc <- smallest_class_info$Smallest_Class_Perc
  
  #### 6. CONDITION NUMBER
  condition_number <- NA_real_
  cond_idx <- grep("Condition Number for the Information Matrix", output, ignore.case = TRUE)
  if (length(cond_idx) > 0) {
    cond_line <- output[cond_idx[1]]
    condition_number <- as.numeric(str_extract(cond_line, "[0-9]+\\.?[0-9]*E?-?[0-9]*"))
  }
  
  #### 7. SAMPLE SIZE (N) - Moved inside function
  sample_size <- NA_integer_
  n_idx <- grep("Number of observations", output, ignore.case = TRUE)
  if (length(n_idx) > 0) {
    sample_size <- as.numeric(str_extract(output[n_idx[1]], "\\d+"))
  }
  
  
  #### 8. ERROR EXTRACTION ####
  error_messages <- list()
  start_idx <- grep("RANDOM STARTS RESULTS RANKED FROM THE BEST TO THE WORST LOGLIKELIHOOD VALUES", 
                    output, ignore.case = TRUE)
  end_idx <- grep("MODEL FIT INFORMATION", output, ignore.case = TRUE)
  if (length(start_idx) > 0 && length(end_idx) > 0 && start_idx[1] < end_idx[1]) {
    section <- output[(start_idx[1] + 1):(end_idx[1] - 1)]
    not_table_lines <- !grepl(
      "^\\s*-?\\d+\\.\\d+\\s+(\\d+|unperturbed)\\s+\\d+$|^Final stage loglikelihood values|^\\s*perturbed starting value run\\(s\\)|^\\s*$", 
      section, ignore.case = TRUE
    )
    relevant_section <- section[not_table_lines]
    relevant_lines <- relevant_section[
      !grepl("^\\s*$|^[A-Z\\s]+:$|THE MODEL ESTIMATION TERMINATED NORMALLY|THE BEST LOGLIKELIHOOD VALUE", 
             relevant_section, ignore.case = TRUE)
    ]
    if (length(relevant_lines) > 0) {
      message_list <- character()
      current_message <- character()
      for (line in relevant_lines) {
        trimmed_line <- trimws(line)
        if (nchar(trimmed_line) > 0) {
          if (length(current_message) > 0 && 
              grepl("^(Unperturbed|\\d+ perturbed|WARNING:)", trimmed_line, ignore.case = TRUE)) {
            message_list <- c(message_list, paste(current_message, collapse = " "))
            current_message <- character()
          }
          current_message <- c(current_message, trimmed_line)
        } else if (length(current_message) > 0) {
          message_list <- c(message_list, paste(current_message, collapse = " "))
          current_message <- character()
        }
      }
      if (length(current_message) > 0) {
        message_list <- c(message_list, paste(current_message, collapse = " "))
      }
      if (length(message_list) > 0) {
        error_messages <- list(unique(message_list))
      }
    }
  }
  errors <- if (length(error_messages) > 0) {
    tibble(Error_Message = error_messages)
  } else {
    tibble(Error_Message = list(NA_character_))
  }
  error_count <- if (length(error_messages) > 0) length(unique(unlist(error_messages))) else 0
  
  #### 9. WARNINGS EXTRACTION ####
  warning_idx <- grep("^\\*\\*\\* WARNING", output)  # Find all warning lines
  warnings <- character(0)  # Initialize empty vector
  
  if (length(warning_idx) > 0) {
    for (idx in warning_idx) {
      warning_msg <- output[idx]  # Capture the warning line
      full_warning <- c(warning_msg)  # Store full warning text
      
      # Collect additional lines if they are part of the warning
      next_idx <- idx + 1
      while (next_idx <= length(output) && grepl("^[^*]", output[next_idx])) {
        full_warning <- c(full_warning, output[next_idx])
        next_idx <- next_idx + 1
      }
      
      # Combine into a single warning message
      warnings <- c(warnings, paste(full_warning, collapse = " "))
    }
  }
  
  # Count the total number of warnings
  warning_count <- length(warnings)
  
  
  #### ✅ Return Data Frame (Including Warnings) ####
  tibble(
    File_Name          = file_name,
    Class_Model        = class_model,
    Best_LogLikelihood = best_ll,
    npar               = npar,
    Random_Start1      = rs_analysis1,
    Random_Start2      = rs_analysis2,
    Total_Starts       = total_starts,
    Converged          = converged,
    Perc_Convergence   = perc_convergence,
    Replicated_LL      = replicated_ll,
    Replicated_LL_Perc = replicated_ll_perc,
    Smallest_Class     = smallest_class,
    Smallest_Class_Perc = smallest_class_perc,
    Condition_Number   = condition_number,
    LogLikelihoods     = list(log_likelihoods),
    Sample_Size        = sample_size,
    Error_Count        = error_count, 
    Errors             = list(errors),  
    Warning_Count      = warning_count,  
    Warnings           = list(warnings)  
  )
  
  
}
