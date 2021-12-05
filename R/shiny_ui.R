#' The Shiny App UI.
#' @export
shiny_ui <- function() {

  ##################################################################################################
  # Create a if statement so that the default dataset is used if no Data argument is used
  ##################################################################################################
  if(!exists('Data', envir = parent.env(environment()), inherits = FALSE)) {
    data(BodyMapR_mock_dataset, envir = environment())
    Data <- BodyMapR_mock_dataset
  }



  ##################################################################################################
  # Header
  ##################################################################################################
  header <- shinydashboard::dashboardHeader(title = "BodyMapR")


  ##################################################################################################
  # Sidebar
  ##################################################################################################
  sidebar <- dashboardSidebar(
    # Create a select list
    selectizeInput(inputId = "record_id_input",
                   label = tagList(icon("atlas"), "Filter on Record ID"),
                   choices = NULL),
    selectizeInput(inputId = "gene",
                   label = tagList(icon("dna"), "Filter on Gene Mutation"),
                   choices = NULL))
  ##################################################################################################
  # Body
  ##################################################################################################
  body <- dashboardBody(
    plotlyOutput("bodymapPlotFilterByGeneRecord", height = "1000px")
  )

  ##################################################################################################
  # Aggregated UI
  ##################################################################################################
  ui <- dashboardPage(header = header,
                      sidebar = sidebar,
                      body = body
  )


}
