## code to prepare `look_up_table_LE` for package
look_up_table_LE <- readxl::read_excel("./data-raw/BodyMapR_look_up_table_lower_extremity.xlsx")
usethis::use_data(look_up_table_LE, overwrite = TRUE)

