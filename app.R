# ============================================================
# app.R — Punto de entrada de StatOccu
#
# Este archivo SOLO:
#   1. Carga librerías y helpers compartidos
#   2. Carga los módulos
#   3. Define ui y server ensamblando los módulos
#
# La lógica de cada módulo vive en modules/mod_*.R
# Las funciones y estilos compartidos viven en R/helpers.R
#
# StatSuite:
#   StatDesign  — Diseño de estudios y muestreo
#   StatFlow    — Primeros análisis y visualización
#   StatGeo     — Análisis espacial y mapas
#   StatMonitor — Monitoreo poblacional
#   StatModels  — Modelos estadísticos
#   StatOccu    — Modelos de ocupación      ← esta app
# ============================================================

# ── 1. Librerías y helpers ─────────────────────────────────
source("R/helpers.R")

# ── 2. Módulos ─────────────────────────────────────────────
source("modules/mod_ocupacion.R")
# source("modules/mod_spocc.R")     # próximamente — spOccupancy
# source("modules/mod_bayes.R")     # próximamente — nimble / Stan
source("modules/mod_acerca_de.R")

# ── 3. UI ──────────────────────────────────────────────────
ui <- page_navbar(
  header = shinyjs::useShinyjs(),
  title  = div(
    style = "display: flex; align-items: center; gap: 10px; margin-top: 4px;",
    img(src = "hexsticker_StatOccu.png", height = "38px"),
    span("StatOccu", style = "font-weight: 600;")
  ),
  theme  = tema_app,
  lang   = "es",
  footer = div(
    class = "text-center text-muted small py-2",
    style = paste0("border-top: 1px solid ", colores$borde, ";"),
    "Manuel Sp\u00ednola \u00b7 ICOMVIS \u00b7 Universidad Nacional \u00b7 Costa Rica"
  ),

  # ── Módulo activo ─────────────────────────────────────────
  nav_panel(
    title = "Ocupaci\u00f3n cl\u00e1sica",
    icon  = bs_icon("camera"),
    mod_ocupacion_ui("ocupacion")
  ),

  # ── Próximamente ──────────────────────────────────────────
  nav_panel(
    title = "Ocupaci\u00f3n espacial (spOccupancy)",
    icon  = bs_icon("globe-americas"),
    proximamente_ui(
      icono     = "globe-americas",
      titulo    = "Ocupaci\u00f3n espacial y multi-especie",
      subtitulo = paste0(
        "Extiende el modelo cl\u00e1sico incorporando estructura espacial expl\u00edcita, ",
        "efectos aleatorios de sitio y modelos de m\u00faltiples especies. ",
        "Ideal cuando los sitios no son independientes o se trabaja con ",
        "comunidades. Paquete: spOccupancy (Doser et al. 2022)."
      ),
      paquete  = "spOccupancy \u2014 Doser et al. (2022)",
      datasets = "Datos de aves de Norteam\u00e9rica (BBS) \u00b7 ejemplos simulados multi-especie"
    )
  ),

  nav_panel(
    title = "Marco bayesiano",
    icon  = bs_icon("distribute-vertical"),
    proximamente_ui(
      icono     = "distribute-vertical",
      titulo    = "Modelos de ocupaci\u00f3n bayesianos",
      subtitulo = paste0(
        "Especificaci\u00f3n completa del modelo jer\u00e1rquico con distribuciones a priori, ",
        "muestreo MCMC y diagn\u00f3stico de cadenas. Permite incorporar informaci\u00f3n ",
        "previa, modelar par\u00e1metros derivados y propagar la incertidumbre de forma ",
        "coherente. Paquetes: nimble \u00b7 Stan \u00b7 JAGS."
      ),
      paquete  = "nimble \u00b7 rstan \u00b7 rjags",
      datasets = "Mismos datasets del m\u00f3dulo cl\u00e1sico \u00b7 comparaci\u00f3n frecuentista vs. bayesiano"
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

# ── 4. Server ──────────────────────────────────────────────
server <- function(input, output, session) {
  mod_ocupacion_server("ocupacion")
  mod_acerca_de_server("acerca_de")
}

# ── 5. Lanzar ──────────────────────────────────────────────
shinyApp(ui, server)
