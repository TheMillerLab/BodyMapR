##' The Shiny App Server.
#' @param input input set by Shiny.
#' @param output output set by Shiny.
#' @param session session set by Shiny.
#' @export
shiny_server <- function(input, output, session) {


  ##########################################################################################################################
  # Create a, "if" statement so that the default dataset is used if no Data argument is used
  ##########################################################################################################################
  if(!exists('Data', envir = parent.env(environment()), inherits = FALSE)) {
    message('External data set was not entered as an argument, therefore, using default BodyMapR_mock_dataset')
    data(BodyMapR_mock_dataset, envir = environment())
    Data <- BodyMapR_mock_dataset
  }


  ##########################################################################################################################
  # Use the Server Side of the App to build the sidebar selector that will be used in the app's UI
  ##########################################################################################################################
  # Create a character vector of record_id to be used for the selectizeInput function in the UI
  record_id_chr <- Data %>%
    select(record_id) %>%
    unique() %>%
    arrange(record_id)

  # This selector will display the various record IDs found in the cohort
  updateSelectizeInput(
    session = session,
    inputId = "record_id_input",
    choices = c("Entire Cohort" = "",
                record_id_chr$record_id),
    server = TRUE
  )

  # This selector will display the gene names of all the genes in the BodyMapR genes dataset
  updateSelectizeInput(
    session = session,
    inputId = "gene",
    choices = c("No Genes Selected" = "",
                BodyMapR::genes$genes),
    server = TRUE)


  ##########################################################################################################################
  # Apply `bodymapr_df()` to Data to create a data frame that will be used in our graphing function
  ##########################################################################################################################

  dt <- Data %>% BodyMapR::bodymapr_df()




  ##########################################################################################################################
  # Filter the Data Frame `dt` using the inputs from the Sidebar
  ##########################################################################################################################

  ############################ Filtered By Record ID ################################
  # Build a reactive "Record ID selector"
  recordId.filter.reactive <- reactive({
    input$record_id_input
  })
  shiny::observe(print(c("Filtered on Record ID", input$record_id_input)))


  # Use the above reactive "Record ID selector" to filter the data frame processed
  ## `bodymapr_df()` to select for a given record
  dt.filter.record.reactive <- reactive({
    dt %>% dplyr::filter(record_id == recordId.filter.reactive())
  })
  shiny::observe(print(c("Below is the Data Frame following selection of Record ID:", input$record_id_input)))
  shiny::observe(print(dt.filter.record.reactive()))

  ############################ Filtered By Gene ################################
  # Build a reactive "Gene selector"
  regex_gene.reactive <- reactive({
    input$gene
  })
  shiny::observe(print(c("Filtered on Gene", input$gene)))

  # Use the above reactive "Gene selector" to filter the data frame processed
  ## `bodymapr_df()` to select for a given genomic alteration
  dt.filter.gene.reactive <- reactive({
    dt %>% filter(stringr::str_detect(string = dt$genomic_alterations,
                                      pattern = stringr::regex(regex_gene.reactive(), ignore_case = TRUE)))
  })
  shiny::observe(print(c("Below is the Data Frame following selection of Gene:", input$gene)))
  shiny::observe(print(dt.filter.gene.reactive()))
  message("Quality Control Step for Initial Selection of Record ID and Genes")
  shiny::observe(print(c("The Gene Selector is Blank (T/F):",(input$gene == ""))))


  ############################ Filtered By Both Record ID and Gene Selection ################################
  # Step One, create a reactive data frame that is a full join of both
  ## the reactive Record ID and Gene data frames
  print("Below is Full join data frame of Record ID and Gene")
  dt.record.gene.reactive <- reactive({
    dt.record.gene <- full_join(dt.filter.record.reactive(),
                                dt.filter.gene.reactive()
    )
    return(dt.record.gene)
  })
  # Print the reactive data frame dt.record.gene.reactive()
  shiny::observe(print(dt.record.gene.reactive()))


  # Step Two, build a reactive data frame that has only those records with the
  ## selected gene mutations
  dt.recordId.in.gene.reactive <- reactive({
    dt.filter.gene.reactive() %>%
      filter(dt.filter.gene.reactive()$record_id %in%
               dt.filter.record.reactive()$record_id)
  })
  print("Below is the data frame of those records with the selected gene mutation")
  shiny::observe(print(dt.recordId.in.gene.reactive()))


  # Step three, build a series of "else if" statements to select the appropriate
  ## data frame to visualize and store the output as its own reactive
  dt.gene.record.filter.reactive <- reactive({
    if (
      # if a gene is selected and ...
      input$gene != "" &
      ## there are no records with that gene alteration
      nrow(dt.filter.gene.reactive()) == 0
    ) {
      ### return that blank data frame
      dt.filter.gene.reactive()
    } else if (
      # if a record ID was chosen and ...
      nrow(dt.filter.record.reactive()) != 0 &
      ## the reactive data frame filtered on a gene is not empty
      nrow(dt.filter.gene.reactive()) != 0) {
      ### return the data frame that has only observations with that record ID and genomic alteration
      #### e.g. record ID 1 with a mutation in RB1
      dt.recordId.in.gene.reactive()
    } else if (
      # if a record ID was chosen and ...
      nrow(dt.filter.record.reactive()) != 0 &
      ## the reactive data frame filtered on gene is not empty
      nrow(dt.filter.gene.reactive()) == 0) {
      ### return the data frame filtered only by record ID
      dt.filter.record.reactive()
    } else if (
      # if a record was selected and ...
      nrow(dt.filter.record.reactive()) == 0 &
      ## the data frame with that gene is not empty, given that we have eliminated the first possibility
      nrow(dt.filter.gene.reactive()) != 0) {
      ### return the data frame that has the appropriately filtered both Record ID and Gene
      dt.filter.gene.reactive()
    } else {
      # Otherwise, if the above are not true, return the original data frame
      dt
    }
  }
  )

  message("This is the final data frame that is plotted on the Body Map")
  observe(print(dt.gene.record.filter.reactive()))
  message("dt.gene.record.filter.reactive() contains the following number of rows:")
  observe(print(nrow(dt.gene.record.filter.reactive())))


  ##########################################################################################################################
  # Use the graphing function of BodyMapR to Create the Interactive Anatomical Data Visualization
  ##########################################################################################################################

  ########################## Create a Reactive Using the BodyMapR `bodymapr_plot() function ################################
  bodymapr_plot.reactive <- reactive({
    BodyMapR::bodymapr_plot(data = dt.gene.record.filter.reactive())
  })

  ############################ Output of the BodyMapR ################################
  output$bodymapPlotFilterByGeneRecord <- renderPlotly(
    bodymapr_plot.reactive()
    ) %>%
    bindCache(input$record_id_input, input$gene)


  }
