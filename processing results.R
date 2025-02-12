
# get input data
source("./data_import.R")

# get results of LLMs
output_groq_llama33 <- read_rds("./Data/output_groq_llama33.rds")
output_openai_gpt_4o_mini <- read_rds("./Data/output_openai_gpt_4o_mini.rds")
output_openai_gpt_4o <- read_rds("./Data/output_openai_gpt_4o.rds")
output_deepseek_r1 <- read_rds("./Data/output_deepseek_r1.rds")
output_gemini <- read_rds("./Data/output_gemini.rds")

# add human version of categorisation
human_fp <- r"{C:\Users\paulb\R\Projects\cause_of_death_with_LLM\Data\Human Classification v2.xlsx}"
output_human <-  readxl::read_excel(human_fp, sheet = "category_human") 


output_openai_gpt_4o_mini <- output_openai_gpt_4o_mini |> json_list_to_df() |> unique() |> mutate(across(everything(), tolower))
output_openai_gpt_4o <- output_openai_gpt_4o |> json_list_to_df() |> unique() |> mutate(across(everything(), tolower))
output_groq_llama33 <- output_groq_llama33 |> json_list_to_df() |> unique() |> mutate(across(everything(), tolower))
output_deepseek_r1 <- output_deepseek_r1 |> json_list_to_df() |> unique() |> mutate(across(everything(), tolower))
output_gemini <- output_gemini |> json_list_to_df() |> unique() |> mutate(across(everything(), tolower))

cod_classified_res <-
data.frame(cause_of_death = cod_vector, category = NA) |> 
  unique() |> 
  mutate(across(everything(), tolower)) |> 
  left_join(output_openai_gpt_4o_mini, by = "cause_of_death", suffix = c("", "_gpt_4o_mini" )) |> 
  left_join(output_openai_gpt_4o, by = "cause_of_death", suffix = c("", "_gpt_4o" )) |> 
  left_join(output_groq_llama33, by = "cause_of_death", suffix = c("", "_groq_llama33" )) |>  
  left_join(output_deepseek_r1, by = "cause_of_death", suffix = c("", "_deepseek_r1" )) |>  
  left_join(output_gemini, by = "cause_of_death", suffix = c("", "_gemini" )) |>  
  select(-category)
  
# --------- get the consensus for category across the llm's, and a flag `without_consensus` ---------

results_consensus <- 
cod_classified_res |> 
  tidyr::pivot_longer(
    tidyselect::starts_with("category_"),
    names_to = "llm_type",
    values_to = "category"
  ) |> 
  summarise(
    llm_types = list(llm_type),
    consensus_no = dplyr::n(),
    .by = c("cause_of_death", "category")
  ) |> 
  dplyr::filter(
    consensus_no == max(consensus_no),
    .by = c("cause_of_death")
  ) 

cause_of_death_without_consensus <- 
results_consensus |> 
  dplyr::filter(
    dplyr::row_number() > 1,
    .by = cause_of_death
  ) |> 
  dplyr::pull(cause_of_death)
  
  
results_consensus <- 
results_consensus |> 
  dplyr::filter(
    dplyr::row_number() == 1,
    .by = cause_of_death
  ) |> 
  dplyr::mutate(
    without_consensus = cause_of_death %in% cause_of_death_without_consensus
  ) |> 
  select(cause_of_death, 
         consensus_category = category,
         consensus_no,
         without_consensus)




cod_classified_res <- 
cod_classified_res |> 
  # --------- add consesnsus to results df ---------
  left_join(
    results_consensus,
    by = "cause_of_death",
    relationship = "one-to-one"
  ) |> 
  # --------- add human results to df ---------
  left_join(output_human, by = "cause_of_death") |>  
  mutate(category_human = coalesce(category_human, "none")) 
  
  
cod_classified_res |> filter(consensus_category != category_human) |> arrange(desc(total)) |> View()
  

cod_classified_res <-
cod_data |> 
  mutate(cause_of_death = tolower(cause_of_death)) |> 
  inner_join(
    cod_classified_res,
    by = "cause_of_death"
  ) 
  
  ## |> dplyr::filter(letter == "R") |> View()
  # select(cause_of_death, total, consensus_category) |> 
  # split(~consensus_category) |> 
  # pluck("pulmonary disease")


  
cod_classified_res |> count(consensus_category, category_human, wt = total) |> View()




cod_classified_res |> 
  dplyr::filter(dplyr::if_any(
    tidyselect::starts_with("category_"), 
    function(x) ((coalesce(x, "none")) != "none")
    )
    ) |> View()











results_df |>
  count(category_consensus, category_human) |> 
  pivot_wider(names_from = category_human, values_from = n, values_fill = 0) |> 
  arrange(category_consensus) |> 
  select(dplyr::all_of(c("category_consensus", options))) |> 
  gt(rowname_col = "category_consensus") |>
  # headers blue
  tab_style(
    style = list(
      cell_fill(color = "blue"),
      cell_text(color = "white")
    ),
    locations = list(
      cells_column_labels(),
      cells_stub()
    )
  ) |>
  # diagonals grey
  reduce(1:length(options),
         .f = function(x, y) {
           x |> tab_style(
             style = list(
               cell_fill(color = "darkgray"),
               cell_text(color = "white")
             ),
             locations = cells_body(
               columns = y + 1,
               rows = y 
             )
           )
         },
         .init = _
  )  |>  
  cols_align("center") |> 
  tab_options(
    data_row.padding = px(30),
    heading.padding = px(30)
    ) |> 
  cols_width(everything() ~ px(100))


fn_tp <-
  results_df |>
    mutate(correct = category_human == category_consensus) |>
    count(category_human, correct) |>
    pivot_wider(names_from = correct, values_from = n, values_fill = 0) |>
    select(category = category_human, TP = `TRUE`, FN = `FALSE`)
  
fp <-
results_df |>
    mutate(correct = category_human == category_consensus) |>
    count(category_consensus, correct) |>
    pivot_wider(names_from = correct, values_from = n, values_fill = 0) |>
    select(category = category_consensus, FP = `FALSE`)

merge(fn_tp, fp, by = "category") |> 
  mutate(precision = TP / (TP + FP),
         recall = TP / (TP + FN),
         f1 = 2 * precision * recall / (precision + recall)) |> 
  arrange(category)




results_df |>
  summarise(
    across(
      starts_with("category_"),
      function(x) sum(category_human == x, na.rm = TRUE) / sum(!is.na(x))
  )
  ) |> 
  pivot_longer(cols = everything()) |> 
  arrange(desc(value)) |> 
  select(model = name, accuracy = value) |> 
  mutate(model = stringr::str_remove(model, "category_")) |> 
  filter(model != "human") |> 
  gt() |> 
  gt::fmt_percent("accuracy", decimals = 1) |> 
  tab_footnote(
    "consensus is most common guess across all 5 models",
    cells_body(
      "model",
      model == "consensus"
    )
  )

