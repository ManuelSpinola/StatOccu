# ============================================================
# mod_acerca_de.R — Información sobre StatOccu
# StatOccu · StatSuite · Manuel Spínola · ICOMVIS · UNA
# ============================================================

mod_acerca_de_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "py-4 px-3",
      style = "max-width: 780px; margin: 0 auto;",
      
      h4(
        bs_icon("info-circle", class = "me-2"),
        "Acerca de StatOccu",
        style = paste0("color:", colores$primario, "; font-weight:700;")
      ),
      p(class = "text-muted mb-4",
        "StatOccu es la app de modelos de ocupaci\u00f3n de sitios de StatSuite, ",
        "desarrollada en el ICOMVIS de la Universidad Nacional de Costa Rica. ",
        "Inspirada en la filosof\u00eda de JASP y jamovi: accesible, did\u00e1ctica, ",
        "y con c\u00f3digo R reproducible para quienes quieran profundizar."
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          card_header(bs_icon("collection", class = "me-1"),
                      "StatSuite — Ecosistema completo"),
          card_body(
            tags$ul(
              class = "small",
              tags$li(strong("StatDesign"),  " \u2014 Dise\u00f1o de estudios y muestreo"),
              tags$li(strong("StatFlow"),    " \u2014 Primeros an\u00e1lisis y visualizaci\u00f3n"),
              tags$li(strong("StatGeo"),     " \u2014 An\u00e1lisis espacial y mapas"),
              tags$li(strong("StatMonitor"), " \u2014 Monitoreo poblacional"),
              tags$li(strong("StatModels"),  " \u2014 Modelos estad\u00edsticos"),
              tags$li(strong("StatOccu"),    " \u2014 Modelos de ocupaci\u00f3n \u2190 aqu\u00ed")
            )
          )
        ),
        
        card(
          card_header(bs_icon("box-seam", class = "me-1"),
                      "M\u00f3dulos de StatOccu"),
          card_body(
            tags$ul(
              class = "small",
              tags$li(
                bs_icon("check-circle-fill", class = "me-1",
                        style = paste0("color:", colores$exito)),
                strong("Ocupaci\u00f3n cl\u00e1sica"),
                " \u2014 MacKenzie et al. (2002), paquete ",
                code("unmarked")
              ),
              tags$li(
                bs_icon("hourglass-split", class = "me-1",
                        style = paste0("color:", colores$acento)),
                strong("Ocupaci\u00f3n espacial"),
                " \u2014 spOccupancy (pr\u00f3ximamente)"
              ),
              tags$li(
                bs_icon("hourglass-split", class = "me-1",
                        style = paste0("color:", colores$acento)),
                strong("Marco bayesiano"),
                " \u2014 nimble / Stan (pr\u00f3ximamente)"
              )
            )
          )
        )
      ),
      
      card(
        class = "mt-3",
        card_header(bs_icon("book", class = "me-1"),
                    "Referencias clave"),
        card_body(
          tags$ul(
            class = "small text-muted mb-0",
            tags$li(
              "MacKenzie, D.I., Nichols, J.D., Lachman, G.B., Droege, S., ",
              "Royle, J.A. & Langtimm, C.A. (2002). Estimating site occupancy ",
              "rates when detection probabilities are less than one. ",
              em("Ecology"), ", 83(8), 2248\u20132255."
            ),
            tags$li(
              class = "mt-1",
              "MacKenzie, D.I. & Bailey, L.L. (2004). Assessing the fit of ",
              "site-occupancy models. ",
              em("Journal of Agricultural, Biological and Environmental Statistics"),
              ", 9(3), 300\u2013318."
            ),
            tags$li(
              class = "mt-1",
              "Fiske, I. & Chandler, R. (2011). unmarked: An R package for fitting ",
              "hierarchical models of wildlife occurrence and abundance. ",
              em("Journal of Statistical Software"), ", 43(10), 1\u201323."
            )
          )
        )
      ),
      
      div(
        class = "alert alert-info small mt-3",
        bs_icon("envelope", class = "me-1"),
        "Contacto: ",
        tags$a(href = "mailto:manuel.spinola@una.ac.cr",
               "manuel.spinola@una.ac.cr")
      )
    )
  )
}

mod_acerca_de_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # sin lógica reactiva por ahora
  })
}