
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
  dplyr::select(cause_of_death, total) |> 
  dplyr::filter(total > 0) |> 
  dplyr::mutate(
    A = cause_of_death |> stringr::str_extract("^[A-Z]"),
    B = cause_of_death |> stringr::str_extract("^[A-Z]\\d\\d") |> readr::parse_number(),
    C = cause_of_death |> stringr::str_extract("^[A-Z]\\d\\d\\.\\d") |> readr::parse_number(),
    D = cause_of_death |> stringr::str_sub(7, -1) |> stringr::str_remove_all("[)(:,]")
  ) 
#, B = "^[A-Z]\\d\\d", C = "^[A-Z]\\d\\d\\.\\d")


# get our causes of death for categorizing
x <- cod_data$D

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
    "oral cavity cancer",
    "esophageal cancer",
    "pancreatic cancer",
    "bladder cancer",
    "stomach cancer",
    "prostate cancer",
    "none"
  )

cod_data |> filter(D |> stringr::str_detect(
" unspecified motor vehicles in traffic accident")) |> dplyr::pull(cause_of_death)

x |> janitor::tabyl() |> arrange(desc(n)) |> head()
