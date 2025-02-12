
set.seed(1)

# cause of death data can be downloaded from https://www.nomisweb.co.uk/query/construct/summary.asp?menuopt=200&subcomp=
# the file below was downloaded from this site
fp <- r"{./Data/34506561512084822.csv}"

data <- readr::read_csv(fp, skip = 10)

data <- data[2:8225,] |> janitor::clean_names()

cod_data <-
  data |> 
  dplyr::mutate(
    total = dplyr::pick(.cols = -cause_of_death) |> rowSums()  
  ) |> 
  dplyr::select(cause_of_death_full = cause_of_death, total) |> 
  dplyr::filter(total > 0) |> 
  dplyr::mutate(
    letter = cause_of_death_full |> stringr::str_extract("^[A-Z]"),
    big_number = cause_of_death_full |> stringr::str_extract("^[A-Z]\\d\\d") |> readr::parse_number(),
    sub_number = cause_of_death_full |> stringr::str_extract("^[A-Z]\\d\\d\\.\\d") |> readr::parse_number(),
    cause_of_death = cause_of_death_full |> stringr::str_sub(7, -1) |> stringr::str_remove_all("[)(:,]")
  ) 
#, B = "^[A-Z]\\d\\d", C = "^[A-Z]\\d\\d\\.\\d")


# get our causes of death for categorizing
cod_vector <- cod_data$cause_of_death |> unique()

# shuffle the `cod_vector` ...just to make it harder for the robots :)
cod_vector <- cod_vector |> sample(size = length(cod_vector), replace = FALSE)

# get our options for placing into categories
# the options are taken from the following paper...
# https://pmc.ncbi.nlm.nih.gov/articles/PMC3229033/#:~:text=Current%20smokers%20had%20significantly%20higher,23.93)%2C%20smoking%2Drelated%20cancers
options <-
  c(
    "ischaemic heart disease", #https://archive.datadictionary.nhs.uk/DD%20Release%20March%202021/Covid19PRA/Coronary_Heart.html
    "cerebrovascular disease",
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


# note CDH is not the same as aortic aneurysm, though both are related
# https://pmc.ncbi.nlm.nih.gov/articles/PMC7711307/#:~:text=Even%20though%20abdominal%20aortic%20aneurysm,but%20also%20several%20important%20differences.







