
output_groq_llama33 <- read_rds("./Data/output_groq_llama33.rds")
output_openai_gpt_4o_mini <- read_rds("./Data/output_openai_gpt_4o_mini.rds")
output_openai_gpt_4o <- read_rds("./Data/output_openai_gpt_4o.rds")
output_deepseek_r1 <- read_rds("./Data/output_deepseek_r1.rds")

json_list_to_df <- function(output_list) {
  output_list |> purrr::map(function(x) {
    has_ticks <- stringr::str_detect(x, "```")
    if(has_ticks) {
      #x <- x |> stringr::str_extract( "(?s)```(.*?)```")
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


cod_classified_res <-
data.frame(cause_of_death = x, category = NA) |> 
  mutate(across(everything(), tolower)) |> 
  left_join(output_openai_gpt_4o_mini, by = "cause_of_death", suffix = c("", "_gpt_4o_mini" )) |> 
  left_join(output_openai_gpt_4o, by = "cause_of_death", suffix = c("", "_gpt_4o]" )) |> 
  left_join(output_groq_llama33, by = "cause_of_death", suffix = c("", "_groq_llama33" )) |>  
  select(-category)
  

cod_classified_res |> 
  dplyr::filter(dplyr::if_any(
    tidyselect::starts_with("category_"), 
    function(x) ((coalesce(x, "none")) != "none")
    )
    ) |> View()
