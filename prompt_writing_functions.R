
# function to write the intial prompt for classification
# write_intial_prompt <- function(options) {
# 
#   glue::glue(
#     "You are a classification LLM. For every line of text you receive, you will only return which of the options best matches. ",
#     "\n\nThe options are:\n",
#     paste0("\"", options, collapse = "\"\n"), "\"",
#     "\n\nNo capitalization. No explanations. Write only one of the above options for each line of text you are given."
#   )
#   
# }

write_intial_prompt <- function(options) {
  
  glue::glue(
    "You are a classification LLM. You will receive a JSON file. The file will contain a list of items with cause_of_death.
    You must only return the edited version of this JSON file. Please add 'category' to each item, which can only ever have one of the following values:\n",
    paste0("\"", options, collapse = "\"\n"), "\"",
    "\n\nNo capitalization. No explanations. Return only the data in a structured JSON format."
  )
  
}

# function to convert vector of strings into line-by-line text of strings
glue_chr_vector <- function(vec) {
  glue::glue(paste0("\"", vec, collapse = "\"\n"), "\"")
}

# function to split vector into list of vectors (for chunking)
split_vector <- function(vec, max_length_per_vec) {
  split(vec, ceiling(seq_along(vec) / max_length_per_vec))
}


glue_chr_vector <- function(vec) {
  glue::glue(paste0("\"", vec, collapse = "\"\n"), "\"")
}

glue_to_json <- function(vec) {
  glue::glue('[\n{glue::glue_collapse(glue::glue(\'  {{ "cause_of_death": "{vec}" }}\'), ",\n")}\n]')
}



my_json <-  "{\n  \"diseases\": [\"typhoid fever\", \"salmonella enteritis\", \"salmonella sepsis\", \"localized salmonella infections\", \"salmonella infection unspecified\"],\n  \"category\": \"none\"\n}"
jsonlite::fromJSON(my_json)
# 
# 
# 
# 
# # function to create send classification request to LLM and return results in vector
# classification_prompt <- function(x, options, model = llama_model, delimiter = "\n") {
#   
#   model$chat(options |> write_intial_prompt())
#     
#   output_vector <- out |> stringr::str_split("\n") |> unlist() |> trimws()
#   
#   if(length(output_vector) != length(x)) {
#     stop(paste0("output vector length is ", length(output_vector), " but expect length ", length(x)))
#   }
#   
#   return(output_vector)
#   
# }
# 
# 
# 
#  
# cod_subset <- x |> sample(10) |> stringr::str_remove_all("[)(:,]")
# 
# out <- model$chat(items_prompt(cod_subset))
# 
# x <- x |> sample(10) |> stringr::str_remove_all("[)(:,]")
# 
# classifier_vec <- out |> stringr::str_split("\n") |> unlist()
