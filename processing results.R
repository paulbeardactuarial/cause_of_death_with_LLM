
output_groq_llama33 <- read_rds("./Data/output_groq_llama33.rds")
output_openai_gpt_4o_mini <- read_rds("./Data/output_openai_gpt_4o_mini.rds")
output_openai_gpt_4o <- read_rds("./Data/output_openai_gpt_4o.rds")
output_deepseek_r1 <- read_rds("./Data/output_deepseek_r1.rds")

json_list_to_df <- function(output_list) {
  output_list |> purrr::map(function(x) {
    has_ticks <- stringr::str_detect(x, "```")
    if(has_ticks) {x <- x |> stringr::str_extract( "(?s)```(.*?)\\n```")}
    x |> 
      stringr::str_remove_all("\`") |>
      stringr::str_remove("json") |>
      jsonlite::fromJSON()
  }) |> dplyr::bind_rows()
}


output_openai_gpt_4o_mini <- output_openai_gpt_4o_mini |> json_list_to_df()
output_openai_gpt_4o <- output_openai_gpt_4o |> json_list_to_df()
output_groq_llama33 <- output_groq_llama33 |> json_list_to_df()
output_deepseek_r1 <- output_deepseek_r1 |> json_list_to_df()


llm_chat$chat("how do i extract the pattern inside a string using stringr::str_extract() to capture everything inside ``` (i.e. triple) marks")


output_deepseek_r1[[2]] |>
  str_extract( "(?s)```(.*?)\\n```") |> 
  stringr::str_remove_all("\`") |>
  stringr::str_remove("json") |>
  jsonlite::fromJSON()
