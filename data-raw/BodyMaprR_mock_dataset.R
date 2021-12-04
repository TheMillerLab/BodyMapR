## code to prepare `BodyMapR_mock_dataset for package
BodyMapR_mock_dataset <- readr::read_csv(
  file = "./data-raw/BodyMapR_mock_dataset.csv"
)
usethis::use_data(BodyMapR_mock_dataset, overwrite = TRUE)

