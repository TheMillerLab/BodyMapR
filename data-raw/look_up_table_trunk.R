## code to prepare `look_up_table_trunk` for package
look_up_table_trunk <- readxl::read_excel("./data-raw/BodyMapR_look_up_table_trunk.xlsx")
usethis::use_data(look_up_table_trunk, overwrite = TRUE)
