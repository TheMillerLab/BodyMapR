## code to prepare `body_map_png` for package
BodyMapR_biorender.png <- png::readPNG("./data-raw/BodyMapR_biorender.png")
usethis::use_data(BodyMapR_biorender.png, overwrite = TRUE)
