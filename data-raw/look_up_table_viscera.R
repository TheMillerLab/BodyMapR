## code to prepare `look_up_table_viscera` for package
look_up_table_viscera <- readxl::read_excel("./data-raw/BodyMapR_look_up_table_viscera.xlsx")
usethis::use_data(look_up_table_viscera, overwrite = TRUE)

