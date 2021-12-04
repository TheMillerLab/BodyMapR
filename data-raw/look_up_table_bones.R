## code to prepare `look_up_table_bones` for package
look_up_table_bones <- readxl::read_excel("./data-raw/BodyMapR_look_up_table_bones.xlsx")
usethis::use_data(look_up_table_bones, overwrite = TRUE)

