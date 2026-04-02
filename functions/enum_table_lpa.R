enum_table <- function(x, rows_m1, rows_m2, rows_m3, rows_m4, rows_m5, rows_m6) {  

  
  x2 <- x %>%
    mutate(model_group = case_when(
      row_number() %in% rows_m1 ~ "Model 1",
      row_number() %in% rows_m2 ~ "Model 2",
      row_number() %in% rows_m3 ~ "Model 3",
      row_number() %in% rows_m4 ~ "Model 4",
      row_number() %in% rows_m5 ~ "Model 5",
      row_number() %in% rows_m6 ~ "Model 6"
    ))
  
  x2_flags <- x2 %>%
    mutate(SIC = -.5 * BIC) %>%
    drop_na(SIC) %>% 
    group_by(model_group) %>% 
    mutate(expSIC = exp(SIC - max(SIC))) %>%
    mutate(cmPk = expSIC / sum(expSIC)) %>%
    select(-expSIC, -SIC) %>% 
    ungroup() %>% 
    group_by(model_group) %>%
    mutate(
      best_BIC  = BIC  == min(BIC, na.rm = TRUE),
      best_aBIC = aBIC == min(aBIC, na.rm = TRUE),
      best_CAIC = CAIC == min(CAIC, na.rm = TRUE),
      best_AWE  = AWE  == min(AWE, na.rm = TRUE),
      best_cmPk = cmPk == max(cmPk, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # 2. Remove the flags before creating the table
  x2_clean <- x2_flags %>%
    select(-starts_with("best_"), -row)  # <- drop the helper columns
  
  # 3. Create gt table using the flags from the original x2_flags
  fit_table <- x2_clean %>%
    gt(groupname_col = "model_group") %>%
    tab_header(title = md("**Model Fit Summary Table**")) %>%
    cols_label(
      Title = "Classes",
      Parameters = md("Par"),
      LL = md("*LL*"),
      BLRT_PValue = "BLRT",
      T11_VLMR_PValue = "VLMR",
      BF = md("BF"),
      cmPk = md("*cmPk*")
    ) %>%
    tab_footnote(
      footnote = md(
        "*Note.* Par = Parameters; *LL* = model log likelihood;
      BIC = Bayesian information criterion; aBIC = sample size adjusted BIC;
      CAIC = consistent Akaike information criterion; AWE = approximate weight of evidence;
      BLRT = bootstrapped LRT p-value; cmPk = approximate model probability."
      ),
      locations = cells_title()
    ) %>%
    tab_options(column_labels.font.weight = "bold") %>%
    
    # numeric formatting
    fmt_number(10, decimals = 2, drop_trailing_zeros = TRUE, suffixing = TRUE) %>%
    fmt_number(c(3:7, 11), decimals = 2) %>%
    sub_missing(everything(), missing_text = "--") %>%
    fmt(
      columns = c(8, 9, 11),
      fns = function(x) {
        ifelse(x < 0.001, "<.001", scales::number(x, accuracy = 0.001))
      }
    ) %>% fmt(
      columns = BF,
      fns = function(x) {
        ifelse(x > 100, ">100", scales::number(x, accuracy = 0.1))
      }
    ) %>% 
    
    # bold formatting using flags from x2_flags
    tab_style(
      style = cell_text(weight = "bold"),
      locations = list(
        cells_body(columns = BIC,  rows = x2_flags$best_BIC),
        cells_body(columns = aBIC, rows = x2_flags$best_aBIC),
        cells_body(columns = CAIC, rows = x2_flags$best_CAIC),
        cells_body(columns = AWE,  rows = x2_flags$best_AWE),
        cells_body(columns = cmPk, rows = x2_flags$best_cmPk),
        cells_body(columns = BF,   rows = BF > 10),
        cells_body(columns = BLRT_PValue,
                   rows = BLRT_PValue < .05 & dplyr::lead(BLRT_PValue) > .05),
        cells_body(columns = T11_VLMR_PValue,
                   rows = T11_VLMR_PValue < .05 & dplyr::lead(T11_VLMR_PValue) > .05)
      )
    )
  
  fit_table
  
}


enum_fit <- function(x) {

# Extract model fit data
enum_extract <- LatexSummaryTable(x,                                 
                keepCols=c("Title", "Parameters", "LL", "BIC", "aBIC",
                           "BLRT_PValue", "T11_VLMR_PValue","Observations")) 

# Calculate Indices Derived from the Log Likelihood (LL)
enum_fit <- enum_extract %>%
  mutate(CAIC = -2 * LL + Parameters * (log(Observations) + 1)) %>%
  mutate(AWE = -2 * LL + 2 * Parameters * (log(Observations) + 1.5)) %>%
  separate(Title, c("Model", "Class"), sep = "with") %>% 
  mutate(SIC = -.5 * BIC) %>%
  drop_na(SIC) %>% 
  group_by(Model) %>% 
  mutate(expSIC = exp(SIC - max(SIC))) %>%
  mutate(BF = exp(SIC - lead(SIC))) %>%
  mutate(cmPk = expSIC / sum(expSIC)) %>%
  ungroup() %>% 
  unite(Title, c("Model", "Class"), sep = "with", remove = TRUE) %>% 
  dplyr::select(1:5, 9:10, 6:7, 13, 14) %>%
  mutate(Title = str_to_title(Title)) %>% 
  arrange(Title) %>% 
  mutate(row = row_number()) %>%
  select(row, everything())
  
  enum_fit

}