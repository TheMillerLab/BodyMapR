##########################################################################################################################
# Load Packages
##########################################################################################################################
library(shiny)
library(BodyMap)
library(tidyverse)
library(shinydashboard)
library(plotly)
##########################################################################################################################
# Load Data and Wrangle
##########################################################################################################################

data <- BodyMapR::BodyMapR_mock_dataset
record_id_select <- data.frame(
  record_id = c(1:34, 250:260, 400:450, 550:560, 650:660, 760:770, 880:896, 990:1000, 1090:1110,
                1290:1300, 1390:1400, 1490:1500, 1590:1600, 1650:1691)
)
data <- data %>% filter(record_id %in% record_id_select$record_id)

# Create a character vector of record_id to be used for the selectInput function
record_id_chr <- data %>% select(record_id) %>% unique() %>% arrange(record_id)

# Apply bodymapr_df() to subset data
dt <- data %>% BodyMapR::bodymapr_df()

##########################################################################################################################
# Shiny User Interface
##########################################################################################################################

header <- shinydashboard::dashboardHeader(title = "BodyMapR")


sidebar <- dashboardSidebar(
  # Create a select list
  selectizeInput(inputId = "record_id_input",
              label = tagList(icon("atlas"), "Filter on Record ID"),
              choices = c("Select Subject's Record ID" = "",
                          record_id_chr$record_id)),
  selectizeInput(inputId = "gene",
               label = tagList(icon("dna"), "Filter on Gene Mutation"),
               choices = c("Select Gene" = "",
                           BodyMapR::genes$genes),
               multiple = F))



body <- dashboardBody(
      plotlyOutput("bodymapPlotFilterByGeneRecord", height = "1000px")
      )

ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body
                    )

    ##########################################################################################################################
    # Shiny Server Side Commands
    ##########################################################################################################################

    server <- function(input, output) {

      ############################ bodymap Filtered By Gene and Record and Viral Status################################
      # Build reactive for Record ID and select for Record ID
      recordId.filter.reactive <- reactive({
        input$record_id_input
      })
      observe(print(recordId.filter.reactive()))

      dt.filter.record.reactive <- reactive({
        dt %>% dplyr::filter(record_id == recordId.filter.reactive())
      })
      observe(print(dt.filter.record.reactive()))

      # Build reactive for Gene and select for Gene
      regex_gene.reactive <- reactive({
       input$gene
      })
      observe(print(regex_gene.reactive()))

      dt.filter.gene.reactive <- reactive({
        dt %>% filter(stringr::str_detect(string = dt$genomic_alterations,
                                          pattern = stringr::regex(regex_gene.reactive(), ignore_case = TRUE)))
      })
      observe(print(dt.filter.gene.reactive()))

      # Build a reactive data frame for
      print("Data frame of Record ID and Gene")
      dt.record.gene.reactive <- reactive({
        dt.record.gene <- full_join(dt.filter.record.reactive(),
                                    dt.filter.gene.reactive(),
                                    by = c("record_id", "lesion_tag", "tum_type", "tum_dtctn_dt", "genomic_alterations", "tmb", "hover", "x", "y")
        )
        return(dt.record.gene)
        })


    # Build a reactive data frame that has only those records with the selected genes and the selected recordID
    dt.recordId.in.gene.reactive <- reactive({
     dt.filter.gene.reactive() %>%
        filter(dt.filter.gene.reactive()$record_id %in% dt.filter.record.reactive()$record_id)
    })


    # Build a reactive data frame that has only those records with the selected virus status and the selected recordID
    dt.recordId.in.virus.reactive <- reactive({
      dt.filter.virus.reactive() %>% filter(dt.filter.virus.reactive()$record_id %in% dt.filter.record.reactive()$record_id)
    })

    # Build a reactive data frame that has only those records with the selected virus status and the selected recordID and selected gene
    virus.in.gene  <- reactive({
     dt.filter.virus.reactive() %>% filter(dt.filter.virus.reactive()$record_id %in% dt.filter.gene.reactive()$record_id)
    })

    dt.virus.in.gene.reactive <- reactive({
      virus.in.gene() %>%
      filter(
        !stringr::str_detect(
        string = virus.in.gene()$genomic_alterations,
        pattern = "Not Assessed"
      ))
    })

    # Build a reactive data frame that has only those records with the selected viral status and the selected recordID
    dt.recordId.in.viral_status.reactive <- reactive({
      dt.filter.virus.reactive() %>% filter(dt.filter.virus.reactive()$record_id %in% dt.filter.record.reactive()$record_id)
    })


    # Build a reactive to select a data frame if recordID, Gene and Viral Status are selected
    dt.record.in.gene.in.viral_status.reactive <- reactive({
      dt.recordId.in.gene.reactive() %>% filter(dt.recordId.in.gene.reactive()$record_id %in% dt.filter.virus.reactive()$record_id)
    })

    # Build ifelse statement to select the appropriate dataframe to visualize
      dt.gene.record.filter.reactive <- reactive({
        if(nrow(dt.filter.record.reactive()) == 0 &
           nrow(dt.filter.gene.reactive()) == 0) {
          dt
          } else if (nrow(dt.filter.record.reactive()) != 0 &
                     nrow(dt.filter.gene.reactive()) == 0) {
            dt.filter.record.reactive()
          } else if (nrow(dt.filter.record.reactive()) == 0 &
                     nrow(dt.filter.gene.reactive()) != 0) {
            dt.filter.gene.reactive()
          } else {
        dt.recordId.in.gene.reactive()
          }
        }
        )

      observe(print(dt.gene.record.filter.reactive()))

      ##########################################################################################################################
      # Import Body Map
      ##########################################################################################################################
      body_map.png <-  BodyMapR::BodyMapR_biorender.png

      ##########################################################################################################################
      # Now let’s look at this body map….
     ##########################################################################################################################

      ############################ Base BodyMap Image ################################
      bodymapr_plot.reactive <- reactive({
        BodyMapR::bodymapr_plot(data = dt.gene.record.filter.reactive())
      })

     ############################ Output of the BodyMap ################################
    output$bodymapPlotFilterByGeneRecord <- renderPlotly(bodymapr_plot.reactive()) %>%
      bindCache(input$record_id_input, input$gene)


    observe(print(nrow(dt.gene.record.filter.reactive())))
    observe(print((dt.gene.record.filter.reactive())))

  }




  ##########################################################################################################################
    # Run the application
    ##########################################################################################################################
    shinyApp(ui = ui, server = server)
