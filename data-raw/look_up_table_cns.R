## code to prepare `look_up_table_cns` for package
look_up_table_cns <- readxl::read_excel("./data-raw/BodyMapR_look_up_table_cns.xlsx")
usethis::use_data(look_up_table_cns, overwrite = TRUE)

