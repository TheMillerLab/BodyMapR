#' Create a data frame with genomic alterations listed in wide format with all genomic alterations in one cell
#' @description
#' `genomics.df.unite()` wrangles and processes genomics data from a REDCap project that has incorporated the Genomics Instrument. This allows for expedited analysis of patient-level data from REDCap. Genetic alterations are listed in wide format with a concatenation of genomic alterations in one cell.
#'
#' @param data data frame/tibble
#'
#' @return A data frame with the list of genes altered in a single cell
#' @export
#'
#' @examples
#' # Create a new DF with sample data set
#' BodyMapR::BodyMapR_mock_dataset %>%
#'   genomics.df.unite()
#'
###########################################################################################################################
# Function Script
##########################################################################################################################
genomics.df.unite <- function(data){

  ##########################################################################################################################
  # Load Data
  ##########################################################################################################################
  # Load Data
  dt <- data

  ##########################################################################################################################
  # Create a tibble of the processed data in long format using Genomics::genomics.df.long()
  ##########################################################################################################################
  genomics.df.long <- BodyMapR::genomics.df.long(dt)

  ##########################################################################################################################
  # Process the data to prepare for conversion to wide format
  ##########################################################################################################################
  ## Select the relevant columns from genomics.df.long
  genomics.full_join.select <- genomics.df.long %>%
    select(record_id,
           lesion_tag_genomics,
           genomics_tissue_type,
           genomics_platform,
           gene)
  ##########################################################################################################################
  # Create a tibble of the genomics date data so we can combine these two tables
  ##########################################################################################################################
  genomics.date <- dt %>%
    select(record_id,
           lesion_tag_genomics,
           genomics_date) %>%
    drop_na(lesion_tag_genomics)
  ##########################################################################################################################
  # Combine both the genomics.full_join.select with the genomics.date tibbles
  ##########################################################################################################################
  genomics.full_join.select.date <- left_join(genomics.full_join.select,
                                              genomics.date,
                                              by = c("record_id",
                                                     "lesion_tag_genomics"))

  ##########################################################################################################################
  # Pivot Wider to create the wide format table and collapse those genetic alterations into one cell "genomic_alterations"
  ##########################################################################################################################
  genomics.pivot.wider <- genomics.full_join.select.date %>%
    group_by(record_id,
             lesion_tag_genomics,
             genomics_date) %>%
    mutate(genomic_alterations = paste0(gene,
                                        collapse = ", ")) %>%
    ungroup()
  ## Replace NA with None Detected for the genomic_alterations
  genomics.pivot.wider$genomic_alterations[genomics.pivot.wider$genomic_alterations == "NA"] <- "None Detected"
  ## Slice and keep only the top row of each observation grouped by record_id, lesion_tag_genomics and genomics_date
  genomics.pivot.wider.slice <- genomics.pivot.wider %>%
    group_by(record_id,
             lesion_tag_genomics,
             genomics_date) %>%
    slice_head() %>%
    ungroup() %>%
    select(-gene)

  ##########################################################################################################################
  # Return the relevant Tibble for the function
  ##########################################################################################################################
  return(genomics.pivot.wider.slice)
}
