## code to prepare `look_up_table_lymphatics` for package
look_up_table_lymphatics <- readxl::read_excel("./data-raw/BodyMapR_look_up_table_lymphatics.xlsx")
usethis::use_data(look_up_table_lymphatics, overwrite = TRUE)

