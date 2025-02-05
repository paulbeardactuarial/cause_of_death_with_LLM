library(tidyverse)
library(ellmer)

# Note for use of the LLM you must have an API with groq. And set GROQ_API_KEY using usethis::edit_r_environ()

# source functions and data written for this work
source("./data_import.R")
source("./prompt_writing_functions.R")

# set max chunk size... this might need refning based on what API seems to accept
max_chunk_size <- 10

# get our prompts of x for processing
list_x <- x |> split_vector(max_chunk_size)
prompts_list <- list_x |> purrr::map(glue_to_json)

vectors <- seq_along(prompts_list)
output_openai_gpt_4o_mini <- vector("list", length = length(prompts_list))


model <- chat_openai(model = "gpt-4o-mini-2024-07-18",
                     system_prompt = write_intial_prompt(options))

#for (i in vectors) {
for (i in 250:360) {
  output_openai_gpt_4o_mini[[i]] <- model$chat(list_x[[i]], echo = FALSE) 
  saveRDS(output_openai_gpt_4o_mini, "output_openai_gpt_4o_mini.rds")
  # waiting x seconds between requests to try and reduce token use
  #Sys.sleep(1)
}
# convert the output back into dataframes...
mapping_df <- 
  output_openai_gpt_4o_mini |> purrr::map(function(x) {
    x |>
      stringr::str_remove_all("\`") |>
      stringr::str_remove("json") |>
      jsonlite::fromJSON()
  }) |> dplyr::bind_rows()


# check the length of each output vector
purrr::map_dbl(output_openai_gpt_4o_mini, length)



