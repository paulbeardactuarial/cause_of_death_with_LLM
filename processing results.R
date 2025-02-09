
source("./data_import.R")

output_groq_llama33 <- read_rds("./Data/output_groq_llama33.rds")
output_openai_gpt_4o_mini <- read_rds("./Data/output_openai_gpt_4o_mini.rds")
output_openai_gpt_4o <- read_rds("./Data/output_openai_gpt_4o.rds")
output_deepseek_r1 <- read_rds("./Data/output_deepseek_r1.rds")
output_gemini <- read_rds("./Data/output_gemini.rds")

json_list_to_df <- function(output_list) {
  output_list |> purrr::map(function(x) {
    has_ticks <- stringr::str_detect(x, "```")
    if(has_ticks) {
      x <- x |> stringr::str_extract_all( "(?s)```(.*?)```") |> pluck(1) |> tail(1)
      }
    x |> 
      stringr::str_remove_all("\`") |>
      stringr::str_remove("json") |>
      jsonlite::fromJSON()
  }) |> dplyr::bind_rows()
}


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


# --------- add consesnsus to results df ---------

cod_classified_res <- 
cod_classified_res |> 
  left_join(
    results_consensus,
    by = "cause_of_death",
    relationship = "one-to-one"
  ) 

cod_classified_res |> filter(without_consensus) |> glimpse()


cod_data |> 
  mutate(cause_of_death = tolower(cause_of_death)) |> 
  inner_join(
    cod_classified_res,
    by = "cause_of_death"
  ) |> 
  select(cause_of_death, total, consensus_category) |> 
  split(~consensus_category) 



cod_classified_res |> 
  dplyr::filter(dplyr::if_any(
    tidyselect::starts_with("category_"), 
    function(x) ((coalesce(x, "none")) != "none")
    )
    ) |> View()
