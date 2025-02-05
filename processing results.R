
output_groq_llama33 <- read_rds("./Data/output_groq_llama33.rds")
output_openai_gpt_4o_mini <- read_rds("./Data/output_openai_gpt_4o_mini.rds")
output_openai_gpt_4o <- read_rds("./Data/output_openai_gpt_4o.rds")


json_list_to_df <- function(output_list) {
  output_list |> purrr::map(function(x) {
    x |>
      stringr::str_remove_all("\`") |>
      stringr::str_remove("json") |>
      jsonlite::fromJSON()
  }) |> dplyr::bind_rows()
}


output_openai_gpt_4o_mini <- output_openai_gpt_4o_mini |> json_list_to_df()
output_openai_gpt <- output_openai_gpt |> json_list_to_df()
output_groq_llama33 <- output_groq_llama33 |> json_list_to_df()
