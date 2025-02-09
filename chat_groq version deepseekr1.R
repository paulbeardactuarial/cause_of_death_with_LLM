library(tidyverse)
library(ellmer)

# source functions and data written for this work
source("./data_import.R")
source("./prompt_writing_functions.R")

# set max chunk size... this might need refining based on what API seems to accept
max_chunk_size <- 30
sleep_time_between_chunks <- 0
chat_function <- chat_groq 
model <- "deepseek-r1-distill-llama-70b"
output_name <- "output_deepseek_r1"

# get our prompts of cod_vector for processing
list_x <- cod_vector |> split_vector(max_chunk_size)
prompts_list <- list_x |> purrr::map(glue_to_json)
vectors <- seq_along(prompts_list)

# create output vector
llm_output <- vector("list", length = length(prompts_list))

# loop through and collect results!
for (i in vectors) {
  
  llm_chat <- do.call(chat_function, list(model = model, system_prompt = write_initial_prompt_v3(options)))
  
  llm_output[[i]] <- llm_chat$chat(prompts_list[[i]], echo = FALSE) 
  
  cli::cli_alert_info(glue::glue("completed {i} of {length(vectors)}"))
  
  saveRDS(llm_output, glue::glue("./Data/{output_name}.rds"))
  
  Sys.sleep(sleep_time_between_chunks)
}
