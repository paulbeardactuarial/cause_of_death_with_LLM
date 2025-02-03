
library(ellmer)

# set up llama model via groq
# to do this, you must have an API with groq. And set GROQ_API_KEY using usethis::edit_r_environ()
llama_model <- chat_groq(model = "llama-3.3-70b-versatile")

delimiter <- "\n"

# function to create send classification request to LLM and return results in vector
classification_prompt <- function(x, options, model, delimiter = "\n") {
  
  # prompt <-
  # paste(
  #   "You are a classification AI", 
  #   "Determine, for each text separated by `,", delimiter,"`, if the text for each item of the input vector refers to one of the following options:",
  #   paste(options, collapse = ", "), 
  #   "No capitalization. No explanations. Write only one option for each text.",
  #   "The answer is based on the following input:",
  #   paste(x, collapse = delimiter)
  # )
  #   
  prompt <-
    glue::glue(
      "You are a classification AI. Determine, for each of the following ", length(x) ," texts which of the options best matches. ",
      "The texts are:\n\n",
      paste0("\"", x, collapse = "\"\n"), "\"",
      "\n\nThe options are:\n",
      paste(options, collapse = "\n"), 
      "\n\nNo capitalization. No explanations. Write only one option for each text, i.e. ", length(x), " responses."
    )
  
  
    out <- model$chat(prompt)
    
    output_vector <- out |> stringr::str_split("\n") |> unlist() |> trimws()
    
    if(length(output_vector) != length(x)) {
      stop(paste0("output vector length is ", length(output_vector), " but expect length ", length(x)))
    }
    
    return(output_vector)
    
}


x <- cod_data$D 

# smoking categories of hazard ratio

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
"oral cavity cancer", 
"esophageal cancer", 
"pancreatic cancer", 
"bladder cancer", 
"stomach cancer",
"prostate cancer",
"none"
)


classifcations <- classification_prompt(x = x, options = options, model = llama_model)

data.frame(
  x,
  classifiers
) |> View()
