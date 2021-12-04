#' Create a data frame of clinical tumor characteristics from a structured electronic data collection instrument (e.g. the Lesion Information instrument) and maps them to a look up table that contains a coordinate system for the BodyMapR
#' @description
#' `bodymapr_df()` returns a data frame of lesions from a electronic data capture project that uses the Lesion Information Instrument. This is a intermediary step for the body_map.plot() function.
#' @param data is a data frame which contains the data for which you want to create a body map. This data is typically an exported csv from a REDCap project that has incorporated the Lesion Information Instrument.
#' @return Data Frame
#' @export
#'
#' @examples
#' # Test with embedded data set "mock_mcc_dataset"
#' BodyMapR::mock_mcc_dataset %>%
#'   bodymapr.df()
#'
bodymapr_df <- function(data){

  ##########################################################################################################################
  # Load Data
  ##########################################################################################################################
  dt <- data
  dt <- dt %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  ##########################################################################################################################
  # Load Look Up Tables
  ##########################################################################################################################
  # Head and Neck Look Up
  look_up_table_head_neck <- BodyMapR::look_up_table_head_neck
  ## Turn all columns to characters
  look_up_table_head_neck <- look_up_table_head_neck %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  # Trunk Look Up
  look_up_table_trunk <- BodyMapR::look_up_table_trunk
  ## Turn all columns to characters
  look_up_table_trunk <- look_up_table_trunk %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  # Upper Extremity Look Up
  look_up_table_UE <- BodyMapR::look_up_table_upper_extremity
  ## Turn all columns to characters
  look_up_table_UE <- look_up_table_UE %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  # Lower Extremity Look Up
  look_up_table_LE <- BodyMapR::look_up_table_lower_extremity
  ## Turn all columns to characters
  look_up_table_LE <- look_up_table_LE %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  # Lymphatics Look Up
  look_up_table_lymphatics <- BodyMapR::look_up_table_lymphatics
  ## Turn all columns to characters
  look_up_table_lymphatics <- look_up_table_lymphatics %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  # cns Look Up
  look_up_table_cns <- BodyMapR::look_up_table_cns
  ## Turn all columns to characters
  look_up_table_cns <- look_up_table_cns %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  # bones Look Up
  look_up_table_bones <- BodyMapR::look_up_table_bones
  ## Turn all columns to characters
  look_up_table_bones <- look_up_table_bones %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  # Viscera
  look_up_table_viscera <- BodyMapR::look_up_table_viscera
  ## Turn all columns to characters
  look_up_table_viscera <- look_up_table_viscera %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  # Mucosa
  look_up_table_mucosa <- BodyMapR::look_up_table_mucosa
  ## Turn all columns to characters
  look_up_table_mucosa <- look_up_table_mucosa %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  ##########################################################################################################################
  #Process Data
  ##########################################################################################################################

  lesion_loc_pre_process.1 <- dt %>%
    select(record_id,
           lesion_tag,
           tum_type,
           tum_dtctn_dt,
           contains("tum_loc")) %>%
    drop_na(lesion_tag)


  # Drop those columns with cns, lymph or mucosa
  lesion_loc_pre_process <- lesion_loc_pre_process.1 %>%
    select(-tum_loc_cns,
           -contains("tum_loc_lymph"),
           -tum_loc_muc,
           -tum_loc_lid,
           -tum_loc_lip,
           -contains("bone"))


  # drop those columns with all NA
  #lesion_loc_pre_process <- lesion_loc_pre_process[colSums(!is.na(lesion_loc_pre_process)) > 0]

  # drop those rows that have all NA but the record_id
  ind <- apply(lesion_loc_pre_process[2:length(lesion_loc_pre_process)],
               1,
               function(x) all(is.na(x)))

  lesion_loc_pre_process <- lesion_loc_pre_process[!ind,]


  # Eliminate any duplicates
  lesion_loc_pre_process <- lesion_loc_pre_process %>%
    group_by(record_id, lesion_tag) %>%
    arrange(tum_dtctn_dt) %>%
    slice_head() %>%
    ungroup()


  ##########################################################################################################################
  # Select those columns that are skin upper extremity
  ##########################################################################################################################

  ## Filter for those that are cutaneous
  cutaneous_lesion_loc_pre_process <- lesion_loc_pre_process %>%
    filter((!is.na(tum_loc_skin) &
             !is.na(tum_dtctn_dt) |
                      tum_type == 1 &
              !is.na(tum_dtctn_dt)
              ))

  skin_lesion_ue <- cutaneous_lesion_loc_pre_process %>%
    select(record_id,
           lesion_tag,
           tum_loc_laterality,
           tum_loc_ant_post,
           tum_loc_med_lat,
           tum_loc_ue) %>%
    drop_na(tum_loc_ue)

  # Now let’s link up skin_lesion_ue with the look up table to give the lesion x and y coordinates

  skin_lesion_ue_with_lookup <- left_join(skin_lesion_ue, look_up_table_UE,
                                          by = c("tum_loc_laterality",
                                                 "tum_loc_ant_post",
                                                 "tum_loc_med_lat",
                                                 "tum_loc_ue"))


  ##########################################################################################################################
  # Select those columns that are skin Lower Extremity
  ##########################################################################################################################


  skin_lesion_le <- cutaneous_lesion_loc_pre_process %>%
    select(record_id,
           lesion_tag,
           tum_loc_laterality,
           tum_loc_ant_post,
           tum_loc_med_lat,
           tum_loc_le) %>%
    drop_na(tum_loc_le)

  # Now let's link up skin_lesion_le with the look up table to give the lesion x and y coordinates
  skin_lesion_le_with_lookup <- left_join(skin_lesion_le,
                                          look_up_table_LE,
                                          by = c("tum_loc_laterality",
                                                 "tum_loc_ant_post",
                                                 "tum_loc_med_lat",
                                                 "tum_loc_le"))



  ##########################################################################################################################
  # Select those columns that are skin Head and neck
  ##########################################################################################################################

  skin_lesion_head <- cutaneous_lesion_loc_pre_process %>%
    select(record_id,
           lesion_tag,
           tum_loc_laterality,
           tum_loc_ant_post,
           tum_loc_med_lat,
           tum_loc_head) %>%
    drop_na(tum_loc_head)

  # Now let's link up skin_lesion_head with the look up table to give the lesion x and y coordinates
  skin_lesion_head_with_lookup <- left_join(skin_lesion_head,
                                            look_up_table_head_neck,
                                            by = c("tum_loc_laterality",
                                                   "tum_loc_ant_post",
                                                   "tum_loc_med_lat",
                                                   "tum_loc_head"))

  ##########################################################################################################################
  # Select those columns that are skin Trunk
  ##########################################################################################################################

  skin_lesion_trunk_preprocess <- cutaneous_lesion_loc_pre_process %>%
    select(record_id,
           lesion_tag,
           tum_loc_laterality,
           tum_loc_ant_post,
           tum_loc_med_lat,
           tum_loc_skin) %>%
    drop_na(tum_loc_skin)

  # Now let's filter those rows that are relevant to trunk
  skin_lesion_trunk <- skin_lesion_trunk_preprocess %>%
    filter(tum_loc_skin == 2 |
             tum_loc_skin == 3 |
             tum_loc_skin == 4 |
             tum_loc_skin == 6 |
             tum_loc_skin == 7)


  # Now let's link up skin_lesion_trunk with the look up table to give the lesion x and y coordinates
  skin_lesion_trunk_with_lookup <- left_join(skin_lesion_trunk,
                                             look_up_table_trunk,
                                             by = c("tum_loc_laterality",
                                                    "tum_loc_ant_post",
                                                    "tum_loc_med_lat",
                                                    "tum_loc_skin"))

  ##########################################################################################################################
  # Now let’s process the visceral lesions
  ##########################################################################################################################
  viscera.1 <- dt %>%
    select(record_id,
           lesion_tag,
           tum_type,
           tum_dtctn_dt,
           tum_loc_laterality,
           tum_loc_viscera) %>%
    drop_na(lesion_tag, tum_loc_viscera)

  viscera.2 <- left_join(viscera.1,
                    look_up_table_viscera,
                    by = c("tum_loc_laterality",
                           "tum_loc_viscera"))
  viscera.3 <- viscera.2 %>% drop_na(x)


  ##########################################################################################################################
  # Now let’s process the lymphatic lesions
  ##########################################################################################################################
  lymph.1 <- dt %>%
    select(record_id,
           lesion_tag,
           tum_type,
           tum_dtctn_dt,
           tum_lymph_laterality_1,
           tum_lymph_1) %>%
    drop_na(lesion_tag, tum_lymph_1)

  lymph.2 <- left_join(lymph.1,
                         look_up_table_lymphatics,
                         by = c("tum_lymph_laterality_1",
                                "tum_lymph_1"))
  lymph.3 <- lymph.2 %>% drop_na(x)


  ##########################################################################################################################
  # Now let’s process the Bone lesions
  ##########################################################################################################################
  bones.1 <- dt %>%
    select(record_id,
           lesion_tag,
           tum_type,
           tum_dtctn_dt,
           tum_bone_laterality_1,
           tum_location_bone_1) %>%
    drop_na(lesion_tag, tum_location_bone_1)

  bones.2 <- left_join(bones.1,
                       look_up_table_bones,
                       by = c("tum_bone_laterality_1",
                              "tum_location_bone_1"))

  bones.3 <- bones.2 %>% drop_na(x)


  ##########################################################################################################################
  # Now let’s process the CNS lesions
  ##########################################################################################################################
  cns.1 <- dt %>%
    select(record_id,
           lesion_tag,
           tum_type,
           tum_dtctn_dt,
           tum_loc_cns) %>%
    drop_na(lesion_tag, tum_loc_cns)

  cns.2 <- left_join(cns.1,
                     look_up_table_cns,
                     by = "tum_loc_cns")

  cns.3 <- cns.2 %>% drop_na(x)

  ##########################################################################################################################
  # Now let’s process the Mucosal lesions
  ##########################################################################################################################
  mucosa.1 <- dt %>%
    select(record_id,
           lesion_tag,
           tum_type,
           tum_dtctn_dt,
           tum_loc_muc) %>%
    drop_na(lesion_tag, tum_loc_muc)

  mucosa.2 <- left_join(mucosa.1,
                     look_up_table_mucosa,
                     by = "tum_loc_muc")

  mucosa.3 <- mucosa.2 %>% drop_na(x)

  ##########################################################################################################################
  # Now let’s bring all of these DFs together
  ##########################################################################################################################

  ## First thing to do is remove tum_loc_* because otherwise all of the columns are exactly the same
  skin_lesion_head_with_coordinates <- skin_lesion_head_with_lookup %>%
    select(record_id, lesion_tag, x, y)

  skin_lesion_trunk_with_coordinates <- skin_lesion_trunk_with_lookup %>%
    select(record_id, lesion_tag, x, y)

  skin_lesion_ue_with_coordinates <- skin_lesion_ue_with_lookup %>%
    select(record_id, lesion_tag, x, y)

  skin_lesion_le_with_coordinates <- skin_lesion_le_with_lookup %>%
    select(record_id, lesion_tag, x, y)

  viscera_with_coordinates <- viscera.3 %>%
    select(record_id, lesion_tag, x, y)

  lymph_with_coordinates <- lymph.3 %>%
    select(record_id, lesion_tag, x, y)

  bones_with_coordinates <- bones.3 %>%
    select(record_id, lesion_tag, x, y)

  cns_with_coordinates <- cns.3 %>%
    select(record_id, lesion_tag, x, y)

  mucosa_with_coordinates <- mucosa.3 %>%
    select(record_id, lesion_tag, x, y)



  body_map.pre <- bind_rows(skin_lesion_head_with_coordinates,
                            skin_lesion_trunk_with_coordinates,
                            skin_lesion_ue_with_coordinates,
                            skin_lesion_le_with_coordinates,
                            viscera_with_coordinates,
                            lymph_with_coordinates,
                            bones_with_coordinates,
                            cns_with_coordinates,
                            mucosa_with_coordinates)



  ##########################################################################################################################
  # Combine with lesion tag and tum_dtctn_dtc
  ##########################################################################################################################
  body_map.1 <- left_join(body_map.pre,
                        lesion_loc_pre_process)



  ##########################################################################################################################
  # Add isolate tmb
  ##########################################################################################################################
  dt.tmb <- dt %>% select(record_id, contains("lesion_tag"), tmb) %>% drop_na(tmb)

  # drop those columns with all NA to get rid of all those lesion_tag..... columns that are not desired
  dt.tmb.2 <- dt.tmb[colSums(!is.na(dt.tmb)) > 0]

  # rename lesion_tag_genomics with lesion_tag so we can combine this later
  names(dt.tmb.2)[2] <- "lesion_tag"

  dt.tmb.2$tmb <- as.double(dt.tmb.2$tmb)
  dt.tmb.2$tmb <- round(dt.tmb.2$tmb, digits = 1)

  body_map.2 <- left_join(body_map.1,
                          dt.tmb.2,
                          by = c("record_id", "lesion_tag"))

  ##########################################################################################################################
  # Add isolate tmb_abs
  ##########################################################################################################################
  dt.tmb.abs <- dt %>% select(record_id, contains("lesion_tag"), tmb_abs) %>% drop_na(tmb_abs)

  # drop those columns with all NA to get rid of all those lesion_tag..... columns that are not desired
  dt.tmb.abs.2 <- dt.tmb.abs[colSums(!is.na(dt.tmb.abs)) > 0]

  # rename lesion_tag_genomics with lesion_tag so we can combine this later
  names(dt.tmb.abs.2)[2] <- "lesion_tag"

  dt.tmb.abs.2$tmb_abs <- as.double(dt.tmb.abs.2$tmb_abs)
  dt.tmb.abs.2$tmb_abs <- round(dt.tmb.abs.2$tmb_abs, digits = 1)

  body_map.3 <- full_join(body_map.2,
                          dt.tmb.abs.2,
                          by = c("record_id", "lesion_tag"))

  body_map.3$tmb.test <- ifelse(
      test = !is.na(body_map.3$tmb),
      yes = body_map.3$tmb,
      no = ifelse(
        test = !is.na(body_map.3$tmb_abs),
        yes = body_map.3$tmb_abs,
        no = NA
      )
    )

  # Create a new column that brings the two different tmb together
  body_map.4 <- body_map.3 %>% select(-tmb, -tmb_abs)
  body_map.4 <- body_map.4 %>% rename(tmb = tmb.test)

  body_map.5 <- body_map.4

  body_map.5$tmb <- as.numeric(body_map.5$tmb)


  ##########################################################################################################################
  # Add Genomic Data
  ##########################################################################################################################
  genes.unite <- BodyMapR::genomics.df.unite(data = dt)

  #rename "lesion_tag_genomics" "lesion_tag" so we can left_join
  names(genes.unite)[2] <- "lesion_tag"

  body_map.6 <- left_join(body_map.5,
                        genes.unite,
                        by = c("record_id",
                               "lesion_tag")
                        )

  # Replace "NA" with "Not Assessed" for genomic alterations
  body_map.6$genomic_alterations[is.na(body_map.6$genomic_alterations)] <- "Not Assessed"


  ##########################################################################################################################
  # Prep for Hover Text
  ##########################################################################################################################

  # Create Age at Date of detection
  body_map.6$tum_dtctn_dt <- lubridate::as_date(body_map.6$tum_dtctn_dt, )
  body_map.6$tum_dtctn_dt <- lubridate::ymd(body_map.6$tum_dtctn_dt)

  # Convert Tumor Type to Strings
  body_map.6$tum_type[body_map.6$tum_type == 1] <- "Primary Cutaneous Lesion"
  body_map.6$tum_type[body_map.6$tum_type == 2] <- "Metastasis"
  body_map.6$tum_type[body_map.6$tum_type == 3] <- "Unknown Primary"
  body_map.6$tum_type[body_map.6$tum_type == 4] <- "Local Recurrence"
  body_map.6$tum_type[body_map.6$tum_type == 5] <- "Histologically not consistent with MCC"
  body_map.6$tum_type[body_map.6$tum_type == 6] <- "Unclear Etiology/Not Confirmed as MCC"
  body_map.6$tum_type[body_map.6$tum_type == 98] <- "Unknown"

  ##########################################################################################################################
  # Clean Data
  ##########################################################################################################################
  body_map.6 <- body_map.6 %>% drop_na(lesion_tag)


  ##########################################################################################################################
  # Convert Data to appropriate types
  ##########################################################################################################################
  body_map.6$x <- as.double(body_map.6$x)
  body_map.6$y <- as.double(body_map.6$y)
  body_map.6$tum_dtctn_dt <- lubridate::as_date(body_map.6$tum_dtctn_dt)


  ##########################################################################################################################
  # Drop Missing values and arrange in specific order
  ##########################################################################################################################
  body_map.7 <- body_map.6 %>% drop_na(tum_type)
  body_map.7 <- body_map.7 %>% arrange(factor(tum_type,
                                              levels = c("Primary Cutaneous Lesion",
                                                         "Metastasis",
                                                         "MCC of Unknown Primary",
                                                         "Local Recurrence",
                                                         "Histologically not consistent with MCC",
                                                         "Unclear Etiology/Not Confirmed as MCC",
                                                         "Unknown")))




  ##########################################################################################################################
  # Isolate the number of lesions in that instance
  ##########################################################################################################################
  df1 <- dt %>% select(record_id, lesion_tag, tum_type, tum_num, tum_num_cat, tum_num_detail) %>% drop_na(lesion_tag)
  df1$tum_type <- as.double(df1$tum_type)
  df1$tum_num <- as.double(df1$tum_num)
  df1$tum_num_cat <- as.double(df1$tum_num_cat)
  df1$tum_num_detail <- as.double(df1$tum_num_detail)
  df2 <- df1 %>% mutate(tum_number = ifelse(test = !is.na(tum_num_detail),
                                            yes = tum_num_detail,
                                            no = ifelse(test = tum_type == 1,
                                                        yes = 1,
                                                        no = ifelse(test = tum_type == 4,
                                                                    yes = 1,
                                                                    no =  ifelse(test = tum_type == 3,
                                                                                 yes = 1,
                                                                                 no = ifelse(test = tum_num_cat == 1,
                                                                                             yes = 5,
                                                                                             no = ifelse(test = tum_num_cat == 2,
                                                                                                         yes = 15,
                                                                                                         no = ifelse(test = tum_num_cat == 3,
                                                                                                                     yes = 25,
                                                                                                                     no = ifelse(test = tum_num == 1,
                                                                                                                                 yes = 1,
                                                                                                                                 no = NA)
                                                                                                                     ))))))))
  df3 <- df2
  df3$tum_num[is.na(df3$tum_num)] <- 0
  df3$tum_number <- ifelse(
    test = !is.na(df3$tum_number),
    yes = df3$tum_number,
    no = ifelse(test = df3$tum_num == 1,
                yes = 1,
                no = NA))

  df4 <- df3
  df4$tum_number <- ifelse(
    test = !is.na(df4$tum_number),
    yes = df4$tum_number,
    no = ifelse(test = stringr::str_detect(df4$lesion_tag, stringr::regex("lesion", ignore_case = TRUE)),
                yes = 1,
                no = ifelse(test = stringr::str_detect(df4$lesion_tag, stringr::regex("metastasis", ignore_case = TRUE)),
                            yes = 1,
                            no = ifelse(test = stringr::str_detect(df4$lesion_tag, stringr::regex("nodule", ignore_case = TRUE)),
                                        yes = 1,
                                        no = ifelse(test = stringr::str_detect(df4$lesion_tag, stringr::regex("nass", ignore_case = TRUE)),
                                                    yes = 1,
                                                    no = NA)))
                )
  )

  df5 <- df4 %>% select(record_id, lesion_tag, tum_number) %>% drop_na()


  ##########################################################################################################################
  # Repeat each row based on the number of lesions in that instance
  ##########################################################################################################################
  body_map.8 <- left_join(
    body_map.7,
    df5,
    by = c("record_id","lesion_tag")
  )

  body_map.8$tum_number <- as.double(body_map.8$tum_number)
  # replace NA in tum_number with 1, this will bias to the null for missing values
  body_map.9 <- body_map.8
  body_map.9$tum_number[is.na(body_map.9$tum_number)] <- 1
  body_map.10 <- as_tibble(lapply(body_map.9, rep, body_map.9$tum_number))

  ##########################################################################################################################
  # Expand Map for liver lesions >= 25 lesions
  ##########################################################################################################################
  look_up_table_liver_lesions <- BodyMapR::look_up_table_liver_lesions
  #look_up_table_liver_lesions <- look_up_table_liver_lesions %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  d <- body_map.10 %>%
    mutate(liver_ind =
             ifelse(test = stringr::str_detect(string = body_map.10$lesion_tag,
                                               pattern = stringr::regex("liver", ignore_case = TRUE)),
                    yes = 1,
                    no = 0),
           num_ind =
             ifelse(test = tum_number >= 25,
                    yes = 1,
                    no = 0),
           liver_num = liver_ind + num_ind
    ) %>%
    group_by(record_id, lesion_tag) %>%
    mutate(id = row_number())

  #d$id <- as.character(d$id)

  d1 <- left_join(
    d,
    look_up_table_liver_lesions,
    by = "id"
  )

  d1 <- d1 %>% rename(x_old = x,
                        y_old = y)

  d2 <- d1 %>% mutate(
    x = ifelse(test = liver_num == 2,
               yes = x1,
               no = x_old),
    y = ifelse(test = liver_num == 2,
               yes = y1,
               no = y_old)
  )


  ##########################################################################################################################
  # Format Size of Lesions
  ##########################################################################################################################
  size <- data %>%
    select(record_id, lesion_tag, contains("clin_dim_1"), contains("tum_dim_clin_unit")) %>%
    drop_na(lesion_tag) %>%
    select(-contains("date"), - contains("fu"))

  size$tum_dim_clin_unit <- as.numeric(size$tum_dim_clin_unit)
  size$clin_dim_1_1 <- as.numeric(size$clin_dim_1_1)
  size$clin_dim_1_2 <- as.numeric(size$clin_dim_1_2)
  size$clin_dim_1_3 <- as.numeric(size$clin_dim_1_3)

  size <- size %>%
    mutate(
      units = ifelse(
        test = tum_dim_clin_unit == 0,
        yes = 1,
        no = 10
      ))

  size$max_dim <- pmax(size$clin_dim_1_1, size$clin_dim_1_2, size$clin_dim_1_3, na.rm = TRUE)
  size$tum_size_mm <- size$units * size$max_dim

  size$record_id <- as.character(size$record_id)

body_map.11 <- left_join(
    d2,
    size,
    by = c("record_id","lesion_tag")
  )

  ##########################################################################################################################
  # Select appropriate fields
  ##########################################################################################################################

  a <- paste("<b>Record ID:</b>",
           body_map.11$record_id)
  b <- paste("<b>Lesion Tag:</b>",
           body_map.11$lesion_tag)
  c <- paste("<b>Genomic Alterations:</b>",
           body_map.11$genomic_alterations)
  d <- paste("<b>Tumor Max Dimension (mm):</b>",
           body_map.11$tum_size_mm)
  e <- paste("<b>Tumor Mutation Burden:</b>",
           body_map.11$tmb)

body_map.11$hover <- paste(a, b, c, d, e, sep = "<br>")


  # We need a size so if tum_size_mm is missing, we need to fill it in with something, we will use 10
body_map.11$tum_size_mm[is.na(body_map.11$tum_size_mm)] <- 10

  ##########################################################################################################################
  # Select appropriate fields
  ##########################################################################################################################
  body_map <- body_map.11 %>%
    select(record_id, lesion_tag, tum_type, tum_size_mm, genomic_alterations, tmb, hover, x, y) %>%
    dplyr::ungroup()

  ##########################################################################################################################
  # Return final data frame
  ##########################################################################################################################

  return(body_map)

}


