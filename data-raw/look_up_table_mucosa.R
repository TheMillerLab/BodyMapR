## code to prepare `look_up_table_mucosa` for package
look_up_table_mucosa <- readxl::read_excel("./data-raw/BodyMapR_look_up_table_mucosa.xlsx")
usethis::use_data(look_up_table_mucosa, overwrite = TRUE)

