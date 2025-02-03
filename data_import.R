
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
    D = cause_of_death |> stringr::str_sub(7, -1)
  ) 
#, B = "^[A-Z]\\d\\d", C = "^[A-Z]\\d\\d\\.\\d")