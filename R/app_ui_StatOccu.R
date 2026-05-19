#' Application UI
#'
#' @return A Shiny UI object.
#' @noRd
app_ui <- function() {

  golem::add_resource_path(
    "www",
    system.file("app/www", package = "StatOccu")
  )

  page_navbar(
    header = shinyjs::useShinyjs(),
    title  = div(
      style = "display: flex; align-items: center; gap: 10px; margin-top: 4px;",
      img(src = "www/hexsticker_StatOccu.png", height = "38px"),
      span("StatOccu", style = "font-weight: 600;")
    ),
    theme  = tema_app,
    lang   = "es",
    footer = div(
      class = "text-center text-muted small py-2",
      style = paste0("border-top: 1px solid ", colores$borde, ";"),
      "Manuel Sp\u00ednola \u00b7 ICOMVIS \u00b7 Universidad Nacional \u00b7 Costa Rica"
    ),

    # ── Módulo 1: unmarked (activo) ───────────────────────
    nav_panel(
      title = "Ocupaci\u00f3n frecuentista (unmarked)",
      icon  = bs_icon("camera"),
      mod_occu_unmarked_ui("occu_unmarked")
    ),

    # ── Módulo 2: ubms (próximamente) ─────────────────────
    nav_panel(
      title = "Ocupaci\u00f3n bayesiana (ubms \u00b7 Stan)",
      icon  = bs_icon("distribute-vertical"),
      proximamente_ui(
        icono     = "distribute-vertical",
        titulo    = "Ocupaci\u00f3n bayesiana (ubms \u00b7 Stan)",
        subtitulo = paste0(
          "Misma sintaxis que unmarked pero con inferencia bayesiana v\u00eda Stan. ",
          "Agrega efectos aleatorios por especie, modelos espaciales con ",
          "Restricted Spatial Regression (RSR) y ocupaci\u00f3n din\u00e1mica. ",
          "Ideal como transici\u00f3n natural desde el m\u00f3dulo frecuentista. ",
          "Paquete: ubms (Kellner et al.)."
        ),
        paquete  = "ubms \u2014 Kellner et al.",
        datasets = "Mismos datasets del m\u00f3dulo frecuentista \u00b7 comparaci\u00f3n directa frecuentista vs. bayesiano"
      )
    ),

    # ── Módulo 3: spOccupancy (próximamente) ──────────────
    nav_panel(
      title = "Ocupaci\u00f3n espacial avanzada (spOccupancy)",
      icon  = bs_icon("globe-americas"),
      proximamente_ui(
        icono     = "globe-americas",
        titulo    = "Ocupaci\u00f3n espacial avanzada (spOccupancy)",
        subtitulo = paste0(
          "Modelos de ocupaci\u00f3n con estructura espacial expl\u00edcita mediante ",
          "Nearest Neighbour Gaussian Processes (NNGP) y P\u00f3lya-Gamma MCMC. ",
          "Soporta modelos de una y m\u00faltiples especies con correlaci\u00f3n espacial ",
          "entre sitios. Dise\u00f1ado para datasets masivos. ",
          "Paquete: spOccupancy (Doser et al. 2022)."
        ),
        paquete  = "spOccupancy \u2014 Doser et al. (2022)",
        datasets = "Datos de aves de Norteam\u00e9rica (BBS) \u00b7 ejemplos simulados multi-especie"
      )
    ),

    nav_spacer(),

    nav_panel(
      title = "Acerca de",
      icon  = bs_icon("info-circle"),
      mod_acerca_de_ui("acerca_de")
    ),

    nav_item(
      tags$span(class = "text-white-50 small", "StatOccu v1.0")
    )
  )
}
