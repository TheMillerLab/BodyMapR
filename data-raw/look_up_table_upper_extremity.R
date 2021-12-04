## code to prepare `look_up_table_upper_extremity` for package
look_up_table_upper_extremity <- readxl::read_excel("./data-raw/BodyMapR_look_up_table_upper_extremity.xlsx")
usethis::use_data(look_up_table_upper_extremity, overwrite = TRUE)

