library(tidyverse)
library(ellmer)

# Note for use of the LLM you must have an API with groq. And set GROQ_API_KEY using usethis::edit_r_environ()

# source functions and data written for this work
source("./data_import.R")
source("./prompt_writing_functions.R")

# set max chunk size... this might need refning based on what API seems to accept
max_chunk_size <- 50

# get our prompts of x for processing
list_x <- x |> split_vector(max_chunk_size)
prompts_list <- list_x |> purrr::map(glue_to_json)

vectors <- seq_along(prompts_list)
output_groq_llama33 <- vector("list", length = length(prompts_list))


# prime the LLM with the intial prompt for classifcation
# set up llama model via groq
# to do this, you must have an API with groq. And set GROQ_API_KEY using usethis::edit_r_environ()
model <- chat_groq(model = "llama-3.3-70b-versatile",
                   #model = "llama-3.3-70b-specdec",
                   system_prompt = write_intial_prompt(options))


# model <- chat_groq(model = "deepseek-r1-distill-llama-70b",
#                    system_prompt = write_intial_prompt(options))

#jsonlite::fromJSON(prompts_list[[1]])


for (i in vectors) {
  output_groq_llama33[[i]] <- model$chat(list_x[[i]], echo = FALSE) 
  # we are capped at 30 requests or 6000 tokens per minute under Groq free service
  # waiting x seconds between requests to try and reduce token use
  saveRDS(output_groq_llama33, "output_groq_llama33.rds")
  Sys.sleep(1)
}
# convert the output back into dataframes...
mapping_df <- 
output_groq_llama33 |> purrr::map(function(x) {
  x |>
    stringr::str_remove_all("\`") |>
    stringr::str_remove("json") |>
    jsonlite::fromJSON()
}) |> dplyr::bind_rows()


# check the length of each output vector
purrr::map_dbl(output_groq_llama33, length)

# collect results into a dataframe
data.frame(
  list_x |> unlist(),
  output_groq_llama33 |> unlist()
)


inner_join(
  cod_data,
  mapping_df,
  by = c("D" = "cause_of_death")
) |> 
  arrange(category) |> 
  View()
