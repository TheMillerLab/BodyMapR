## code to prepare `look_up_table_upper_extremity` for package
look_up_table_liver_lesions <- readxl::read_excel("./data-raw/BodyMapR_look_up_table_liver_lesions.xlsx")
usethis::use_data(look_up_table_liver_lesions, overwrite = TRUE)

