## code to prepare `genes` dataset goes here
genes <- readr::read_tsv("data-raw/genes.txt")
usethis::use_data(genes, overwrite = TRUE)
