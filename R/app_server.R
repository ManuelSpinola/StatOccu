#' Application Server
#'
#' @param input,output,session Internal parameters for Shiny.
#' @noRd
app_server <- function(input, output, session) {
  mod_ocupacion_server("ocupacion")
  mod_acerca_de_server("acerca_de")
}
