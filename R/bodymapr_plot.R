#' Create an interactive data visualization of clinical tumor characteristics from a structured EDC lesion instrument for a Shiny App
#' @description
#' `bodymapr_plot()`returns a plotly visualization of cutaneous tumor characteristics plotted on a body map
#' @param data is a data frame which contains the data for which you want to create a body map after it has been processed for this application
#' @return Plotly Visualization
#' @export
#' @examples
#' # Test with embedded data set "mock_dataset"
#' BodyMapR::BodyMapR_mock_dataset %>%
#'   bodymapr_plot()
#'
bodymapr_plot <- function(data){

  ##########################################################################################################################
  # Load Data
  ##########################################################################################################################

  ##########################################################################################################################
  # Import Body Map
  ##########################################################################################################################
  body_map.png <-  BodyMapR::BodyMapR_biorender.png

  ##########################################################################################################################
  # Now let’s look at this body map….
  ##########################################################################################################################

bodymapr.plot <- ggplot(data = NULL) +
    xlim(0, 100) +
    ylim(0, 100) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_blank()) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank()) +
    annotation_raster(body_map.png,
                      ymin = 0,
                      xmin = 0,
                      xmax = 100,
                      ymax = 100) +
    geom_point(data = data,
               aes(x = x,
                   y = y,
                   size = tum_size_mm,
                   text = gsub('(.{1,90})(\\s|$)', '\\1\n', hover),
                   color = tum_type),
               position=position_jitter(width = 0.35,
                                        height = 1),
               shape = 19)

  plotly::ggplotly(bodymapr.plot,
                   tooltip = c("text")) %>%
    layout(legend = list(orientation = "h",
                         x = 0,
                         y = 0)) %>%
    layout(showlegend = TRUE,
           legend = list(font = list(size = 18))) %>%
    layout(hoverlabel = list(font=list(size=22))) %>%
    add_annotations(
      text = "Osseous Lesions",
      x = 0.09,
      xref = "paper",
      y = 0.01,
      yref = "paper",
      showarrow = F,
      font = list(size = 20)
    ) %>%
    add_annotations(
      text = "Cutaneous Lesions",
      x = 0.5,
      xref = "paper",
      y = 0.01,
      yref = "paper",
      showarrow = F,
      font = list(size = 20)
    ) %>%
    add_annotations(
      text = "Visceral and Lymphatic Lesions",
      x = 0.938,
      xref = "paper",
      y = 0.01,
      yref = "paper",
      showarrow = F,
      font = list(size = 20)
    ) %>%
    add_annotations(
      text = "<b>Cancer Lesion Body Map</b>",
      x = 0.5,
      xref = "paper",
      y = 1,
      yref = "paper",
      showarrow = F,
      font = list(size = 24)
    )
}
