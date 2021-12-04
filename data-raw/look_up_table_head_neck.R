## code to prepare `look_up_table_head_neck` for package
look_up_table_head_neck <- readxl::read_excel("./data-raw/BodyMapR_look_up_table_head_neck.xlsx")
usethis::use_data(look_up_table_head_neck, overwrite = TRUE)

