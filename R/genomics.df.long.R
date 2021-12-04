#' Create a data frame with genomic alterations listed in long format
#' @description
#' `genomics.df.long()` wrangles and processes genomics data from a REDCap project that has incorporated the Genomics Instrument. This allows for expedited analysis of patient-level data from REDCap. Genetic alterations are listed in long format.
#'
#' @param data tibble or data frame
#'
#' @return A tibble containing record_id, lesion_tag_genomics, genomics_tissue_type, gene, nucleotide, protein, cnv_yn with the gene arranged in long format
#' @export
#'
#' @examples
#' # Create a new DF with sample data set
#' BodyMapR::BodyMapR_mock_dataset %>%
#'   genomics.df.long()
#'
###########################################################################################################################
# Function Script
##########################################################################################################################
genomics.df.long <- function(data){

  ##########################################################################################################################
  # Load Data
  ##########################################################################################################################
  # Load Data
  dt <- data

  ##########################################################################################################################
  # Replace integers with clinically relevant strings from the data dictionary
  ##########################################################################################################################

  dt$genomics_tissue_type <- replace(dt$genomics_tissue_type, dt$genomics_tissue_type == 1, "Primary Cutaneous")
  dt$genomics_tissue_type <- replace(dt$genomics_tissue_type, dt$genomics_tissue_type == 2, "Metastasis")
  dt$genomics_tissue_type <- replace(dt$genomics_tissue_type, dt$genomics_tissue_type == 3, "MCCUP")
  dt$genomics_tissue_type <- replace(dt$genomics_tissue_type, dt$genomics_tissue_type == 4, "Local Recurrence")
  dt$genomics_tissue_type <- replace(dt$genomics_tissue_type, dt$genomics_tissue_type == 5, "Liquid Biopsy")

  dt$genomics_platform <- replace(dt$genomics_platform, dt$genomics_platform == 1, "BWH OncoPanel")
  dt$genomics_platform <- replace(dt$genomics_platform, dt$genomics_platform == 2, "MGH SNaPshot")
  dt$genomics_platform <- replace(dt$genomics_platform, dt$genomics_platform == 3, "MSK Impact")
  dt$genomics_platform <- replace(dt$genomics_platform, dt$genomics_platform == 4, "Foundation One CDx")
  dt$genomics_platform <- replace(dt$genomics_platform, dt$genomics_platform == 5, "Tempus xT Gene Panel")
  dt$genomics_platform <- replace(dt$genomics_platform, dt$genomics_platform == 6, "Guardant 360")

  ##########################################################################################################################
  # Process the data to create data frames of the gene variants
  ##########################################################################################################################
  ## Select the relevant variables defined by the prefix "variant_gene"
  ### This df is still in "wide format"
  variants.gene <- dt %>%
    select(record_id,
         lesion_tag_genomics,
         genomics_tissue_type,
         contains("variant_gene"))
  ## Gather these variables and transpose to "long format"
  variants.gene.g <- variants.gene %>%
    gather(key = "variable",
           value = "gene",
           -record_id, # do not include the record_id in the value, but rather keep it as a separate column
           -lesion_tag_genomics, # do not include lesion_tag_genomics in the value, but rather keep it as a separate column
           -genomics_tissue_type) %>% # do not include the genomics_tissue_type in the value, but rather keep it as a separate column
    drop_na("gene")
  ## Filter out those observations with percentage circulating free DNA
  variants.gene.g.1 <- variants.gene.g %>%
    filter(!str_detect(string = variants.gene.g$variable,
                       pattern = "perc"))
  ## ## Separate the variable column to isolate the number
  variants.gene.g.split.pre <- separate(data = variants.gene.g.1,
                                        col = variable,
                                        into = c("text","number"),
                                        sep = "variant_gene_")
  ## let's get rid of this "text" column as it is empty
  variants.gene.g.split <- variants.gene.g.split.pre[,-4]

  ##########################################################################################################################
  #  Process the data to create data frames of the amino acid variants
  ##########################################################################################################################
  ## build table for variant protein ("wide" format)
  variants.protein <- dt %>%
    select(record_id,
           lesion_tag_genomics,
           genomics_tissue_type,
           contains("variant_protein"))
  ## Gather variants.protein into "long" format
  variants.protein.g <- variants.protein %>%
    gather(key = "variable",
           value = "protein",
           -record_id,
           -lesion_tag_genomics,
           -genomics_tissue_type) %>%
    drop_na("protein")
  ## Separate the variable column to isolate the protein
  variants.protein.g.split.pre <- separate(data = variants.protein.g,
                                           col = variable,
                                           into = c("text","number"),
                                           sep = "variant_protein_")
  ## remove the "text" column as it is empty
  variants.protein.g.split <- variants.protein.g.split.pre[,-4]

  ##########################################################################################################################
  #  Process the data to create data frames of the nucleic acid variations
  ##########################################################################################################################
  ## build table for nucleotide variants ("wide" format)
  variants.nucleotide <- dt %>%
    select(record_id,
           lesion_tag_genomics,
           genomics_tissue_type,
           contains("variant_nucleotide"))
  ## Gather nucleotide variants into "long" format
  variants.nucleotide.g <- variants.nucleotide %>%
    gather(key = "variable",
           value = "nucleotide",
           -record_id, -lesion_tag_genomics,
           -genomics_tissue_type) %>%
    drop_na("nucleotide")
  ## Separate the variable column to isolate the nucleotide
  variants.nucleotide.g.split.pre <- variants.nucleotide.g %>%
    separate(variable,
             c("text","number"),
             "variant_nucleotide_")
  ## remove the "text" column as it is empty
  variants.nucleotide.g.split <- variants.nucleotide.g.split.pre[,-4]

  ##########################################################################################################################
  #  Meta Data Table
  ##########################################################################################################################
  ## Let's create a table of metadata, which we will use to combine for our final table at the end
  genomics.meta <- dt %>%
    dplyr::select(record_id,
                  lesion_tag_genomics,
                  genomics_tissue_type,
                  genomics_platform) %>%
    drop_na(lesion_tag_genomics)

  ##########################################################################################################################
  #  Combine gene, protein and nucleotide tibbles
  ##########################################################################################################################
  ## Now let's combine variant gene and variant nucleotide tables that are in the long format
  snv <- left_join(variants.gene.g.split,
                   variants.nucleotide.g.split,
                   by =c("record_id" = "record_id",
                         "lesion_tag_genomics" = "lesion_tag_genomics",
                         "genomics_tissue_type" = "genomics_tissue_type",
                         "number" = "number"))
  ## Now let's combine the above table with the protein variant long table
  snv <- left_join(snv,
                   variants.protein.g.split,
                   by =c("record_id" = "record_id",
                         "lesion_tag_genomics" = "lesion_tag_genomics",
                         "genomics_tissue_type" = "genomics_tissue_type",
                         "number" = "number"))
  ##########################################################################################################################
  #  Clean the snv table
  ##########################################################################################################################
  ## arrange snv by gene
  snv <- snv %>%
    arrange(gene)
  ## Delete everything before the period in the nucleotide column
  ### Because some of the data has c. and p. before the relevant sequences, we will remove those that do
  snv$nucleotide <- str_replace(string = snv$nucleotide,
                                pattern = regex("^.*\\."),
                                replacement = "")
  ## Delete everything before the period in the protein column
  snv$protein  <- str_replace(string = snv$protein,
                              pattern = regex("^.*\\."),
                              replacement = "")
  ## Drop the number column as it is not relevant for our final product
  snv <- snv %>%
    select(-contains("number"))

  ##########################################################################################################################
  # Create a table of cnvs
  ##########################################################################################################################
  ## Count number of genes with ONLY CNVs
  cnv.1 <- dt %>%
    select(record_id,
           lesion_tag_genomics,
           genomics_tissue_type,
           contains("cnv_gene"))
  ## gather the cnv table into "long format"
  cnv.g <- cnv.1 %>%
    gather(key = "variable",
           value = "gene",
           -record_id,
           -lesion_tag_genomics,
           -genomics_tissue_type) %>%
    drop_na("gene")
  ## Add a vector for "Yes" to help ID which genes had CNVs as well
  cnv.g$cnv_yn <- "Yes"
  ## drop the "variable" column
  cnv <- cnv.g[,-4]

  ##########################################################################################################################
  # Join to create the final tibble the function will return
  ##########################################################################################################################
  ## Full join CNV to SNV
  genomics.full_join <- full_join(snv, cnv,
                        by = c("record_id" = "record_id",
                               "lesion_tag_genomics" = "lesion_tag_genomics",
                               "genomics_tissue_type" = "genomics_tissue_type",
                               "gene" = "gene")) %>%
    dplyr::arrange(gene)  # were using a full join b/c we want to be able to see rows for CNV only

  ## Replace NA with "No" for cnv_yn
  genomics.full_join$cnv_yn <- replace(genomics.full_join$cnv_yn,
                             is.na(genomics.full_join$cnv_yn),
                             "No")
  ## Join full_join with the meta data table above
  genomics.df.long <- full_join(genomics.full_join,
                                       genomics.meta)


  ##########################################################################################################################
  # Return tibble
  ##########################################################################################################################
  return(genomics.df.long)
  }

