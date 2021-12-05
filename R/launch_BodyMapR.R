#' Lauch BodyMapR App
#' @export
launch_BodyMapR <- function(Data, ...) {
  shiny_env <- new.env()
  if(!missing(Data)) {
    print('Setting parameters')
    assign('Data', Data, shiny_env)
  }
  environment(shiny_ui) <- shiny_env

  environment(shiny_server) <- shiny_env

  app <- shiny::shinyApp(
    ui = shiny_ui,
    server = shiny_server
  )
  runApp(app, ...)
}
