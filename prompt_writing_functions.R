
x <- x |> stringr::str_remove_all("[)(:,]")

model = llama_model

# function to write the intial prompt for classification
write_intial_prompt <- function(options) {

  glue::glue(
    "You are a classification AI. Determine, for each text sent, return only which of the options best matches. ",
    "\n\nThe options are:\n",
    paste0("\"", options, collapse = "\"\n"), "\"",
    "\n\nNo capitalization. No explanations. Write only one of the above options for each line of text you are given."
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

model$chat(options |> write_intial_prompt())

length(x) 

list_x <- x |> split_vector(10)

prompts_list <- list_x |> purrr::map(glue_chr_vector)

vectors <- seq_along(prompts_list)
output_list <- vector("list", length = length(prompts_list))

for (i in vectors) {
  output_list[[i]] <- model$chat(prompts_list[[i]]) |> stringr::str_split("\n") |> unlist() |> trimws()
  Sys.sleep(2)
}





# function to create send classification request to LLM and return results in vector
classification_prompt <- function(x, options, model = llama_model, delimiter = "\n") {
  
  model$chat(options |> write_intial_prompt())
    
  output_vector <- out |> stringr::str_split("\n") |> unlist() |> trimws()
  
  if(length(output_vector) != length(x)) {
    stop(paste0("output vector length is ", length(output_vector), " but expect length ", length(x)))
  }
  
  return(output_vector)
  
}



 
cod_subset <- x |> sample(10) |> stringr::str_remove_all("[)(:,]")

out <- model$chat(items_prompt(cod_subset))

x <- x |> sample(10) |> stringr::str_remove_all("[)(:,]")

classifier_vec <- out |> stringr::str_split("\n") |> unlist()
