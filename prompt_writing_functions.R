
write_intial_prompt <- function(options) {
  
  glue::glue(
    "You are a classification LLM. You will receive a JSON file. The file will contain a list of items with cause_of_death.
    You must only return the edited version of this JSON file. Please add 'category' to each item, which can only ever have one of the following values:\n",
    paste0("\"", options, collapse = "\"\n"), "\"",
    "\n\nNo capitalization. No explanations. Return only the data in a structured JSON format."
  )
  
}

write_initial_prompt_v2 <- function(options) {
  
  glue::glue(
    "You are a classification LLM. You will receive a JSON file. The file will contain a list of items with cause_of_death.
    It is important that you return only an edited version of the JSON file. Add 'category' to each item, which can only ever pick one of the values below. If none are suitable choose the category of \"none\":\n\n",
    paste0("\"", options, collapse = "\"\n"), "\"",
    "\n\nNo explanations. Return only the data in a structured JSON format. Your final JSON code must begin with ``` and end with ```"
  )
  
}

write_initial_prompt_v3 <- function(options) {
  
  glue::glue(
    "You are a classification LLM. You will receive a JSON file. The file will contain a list of items with cause_of_death.
    It is important that you return only an edited version of the JSON file. Add 'category' to each item, which can only ever pick one of the values below. If none are suitable choose the category of \"none\":\n\n",
    paste0("\"", options, collapse = "\"\n"), "\"",
    "\n\nNo explanations. Return only the data in a structured JSON format. Your final JSON code must begin with ``` and end with ```.
    If a cause of death cannot be linked to smoking in any way, for example if it is an infectious disease, a genetic disorder, or has an external cause provided in the cause_of_death text (e.g. asbestos), then assign the category as \"none\". 
    "
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

# function to convert character vector into JSON file
glue_to_json <- function(vec) {
  glue::glue('[\n{glue::glue_collapse(glue::glue(\'  {{ "cause_of_death": "{vec}" }}\'), ",\n")}\n]')
}

