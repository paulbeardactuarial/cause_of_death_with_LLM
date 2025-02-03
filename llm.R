library(tidyverse)
library(ellmer)

# Note for use of the LLM you must have an API with groq. And set GROQ_API_KEY using usethis::edit_r_environ()

# source functions written for this work
source(".\prompt_writing_functions.R")

# get our causes of death for categorizing
x <- cod_data$D
x <- x |> stringr::str_remove_all("[)(:,]")

# get our options for placing into categories
# the options are taken from the following paper...
# https://pmc.ncbi.nlm.nih.gov/articles/PMC3229033/#:~:text=Current%20smokers%20had%20significantly%20higher,23.93)%2C%20smoking%2Drelated%20cancers
options <-
  c(
    "coronary heart disease",
    "cerebrovascular disease",
    "sudden death",
    "pulmonary disease",
    "lung cancer",
    "colorectal cancer",
    "larynx cancer",
    "kidney cancer",
    "acute myeloid leukemia",
    "mouth cancer",
    "esophageal cancer",
    "pancreatic cancer",
    "bladder cancer",
    "stomach cancer",
    "prostate cancer",
    "none"
  )

# get our prompts of x for processing
list_x <- x |> split_vector(20)
prompts_list <- list_x |> purrr::map(glue_to_json)

vectors <- seq_along(prompts_list)
output_list <- vector("list", length = length(prompts_list))


# prime the LLM with the intial prompt for classifcation
# set up llama model via groq
# to do this, you must have an API with groq. And set GROQ_API_KEY using usethis::edit_r_environ()
model <- chat_groq(#model = "llama-3.3-70b-versatile",
                   model = "llama-3.3-70b-specdec",
                   system_prompt = write_intial_prompt(options))


# model <- chat_groq(model = "deepseek-r1-distill-llama-70b",
#                    system_prompt = write_intial_prompt(options))

jsonlite::fromJSON(prompts_list[[1]])


for (i in vectors) {
  output_list[[i]] <- model$chat(list_x[[i]], echo = FALSE) 
  # we are capped at 30 requests or 6000 tokens per minute under Groq free service
  # waiting x seconds between requests to try and reduce token use
  Sys.sleep(10)
}

# convert the output back into dataframes...
output_list[1:4] |> purrr::map(function(x) x |> stringr::str_remove_all("\`") |> stringr::str_remove("json") |> jsonlite::fromJSON())



# check the length of each output vector
purrr::map_dbl(output_list, length)

# collect results into a dataframe
data.frame(
  list_x |> unlist(),
  output_list |> unlist()
)

output_list[[i]] 
