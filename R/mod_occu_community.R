# ============================================================
# mod_occu_community.R — Modelos de ocupación de comunidades
# StatOccu · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Familia: modelos de ocupación de comunidades (occuComm)
# Datos: Ejemplo simulado de aves con elevación, bosque y masa corporal
# Ecosistema: unmarked + tidyverse + ggplot2
# ============================================================

# ── Datos simulados ───────────────────────────────────────
.cargar_datos_comunidad <- function() {
  path <- system.file("app/data/aves_comunidad.rda", package = "StatOccu")
  if (nchar(path) > 0) {
    e <- new.env()
    load(path, envir = e)
    return(e$aves_comunidad)
  }
  # Fallback: simular datos si el .rda no est\u00e1 disponible
  message("aves_comunidad.rda no encontrado, usando datos simulados")
  set.seed(42)
  nsite <- 100; nocc <- 3; nsp <- 20
  elev   <- rnorm(nsite, 1000, 400)
  bosque <- runif(nsite, 0, 100)
  sitecov <- data.frame(
    elevacion = as.numeric(scale(elev)),
    bosque    = as.numeric(scale(bosque))
  )
  masa <- exp(rnorm(nsp, log(50), 0.8))
  fecha_raw <- matrix(rnorm(nsite * nocc, 90, 20), nsite, nocc)
  fecha     <- matrix(as.numeric(scale(fecha_raw)), nsite, nocc)
  dur_raw   <- matrix(rnorm(nsite * nocc, 120, 30), nsite, nocc)
  duracion  <- matrix(as.numeric(scale(dur_raw)), nsite, nocc)
  colnames(fecha)    <- paste0("fecha.", 1:nocc)
  colnames(duracion) <- paste0("duracion.", 1:nocc)
  obscov <- list(fecha = as.data.frame(fecha), duracion = as.data.frame(duracion))
  beta0 <- rnorm(nsp,-0.5,1.2); beta1 <- rnorm(nsp,-0.8,0.5)
  alpha0 <- rnorm(nsp,0.5,0.4); alpha1 <- rnorm(nsp,0.3,0.2)
  ylist <- vector("list", nsp)
  nombres <- paste0("Especie_", sprintf("%02d", 1:nsp))
  for (s in 1:nsp) {
    psi <- plogis(beta0[s] + beta1[s] * sitecov$elevacion)
    z   <- rbinom(nsite, 1, psi)
    y   <- matrix(NA, nsite, nocc)
    for (i in 1:nsite) {
      p_ij <- plogis(alpha0[s] + alpha1[s] * fecha[i,])
      y[i,] <- rbinom(nocc, 1, p_ij * z[i])
    }
    ylist[[s]] <- y
  }
  names(ylist) <- nombres
  list(ylist=ylist, sitecov=sitecov, spcov=list(log_masa=log(masa)),
       obscov=obscov, nombres=nombres, nsite=nsite, nocc=nocc, nsp=nsp)
}

# ── UI ────────────────────────────────────────────────────
mod_occu_community_ui <- function(id) {
  ns <- NS(id)

  tagList(

    div(
      class = "py-3 px-2",
      h4(
        bs_icon("grid-3x3-gap", class = "me-2"),
        "Modelos de ocupaci\u00f3n de comunidades",
        style = paste0("color:", colores$primario, "; font-weight:700;")
      ),
      p(
        class = "text-muted mb-0",
        "Los modelos de comunidades estiman la ocupaci\u00f3n de m\u00faltiples especies ",
        "simult\u00e1neamente, incorporando efectos aleatorios por especie y rasgos ",
        "funcionales como covariables. Basado en ", tags$strong("occuComm"),
        " (unmarked)."
      )
    ),

    navset_card_tab(

      # ════════════════════════════════════════════════
      # PESTAÑA 1: ¿Qué es?
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("book", class = "me-1"), "\u00bfQu\u00e9 es?"),
        div(class = "p-3",
          layout_columns(
            col_widths = c(6, 6),

            card(
              card_header(bs_icon("diagram-3", class = "me-1"),
                          "Modelo jer\u00e1rquico de comunidades"),
              card_body(
                p(class = "small text-muted mb-3",
                  "El modelo de comunidades extiende el modelo de ocupaci\u00f3n de ",
                  "especie \u00fanica para analizar ", tags$strong("m\u00faltiples especies"),
                  " simult\u00e1neamente. Las especies comparten una distribuci\u00f3n ",
                  "com\u00fan de par\u00e1metros, lo que permite ", tags$strong("tomar prestada"),
                  " informaci\u00f3n entre especies."),
                div(
                  style = paste0("border-left:4px solid ", colores$primario,
                                 "; padding:8px 12px; margin-bottom:10px;",
                                 " background:#E8F4FB; border-radius:0 6px 6px 0;"),
                  tags$b(class = "small", style = paste0("color:", colores$primario),
                         bs_icon("geo-alt-fill", class = "me-1"),
                         "Proceso de estado por especie s"),
                  p(class = "small mb-1 mt-1",
                    style = "font-family:monospace;",
                    "z[i,s] ~ Bernoulli(\u03c8[i,s])"),
                  p(class = "small text-muted mb-0",
                    "logit(\u03c8[i,s]) = \u03b2\u2080[s] + \u03b2\u2081 \u00d7 x[i] + \u03b2\u2082 \u00d7 rasgo[s]")
                ),
                div(
                  style = paste0("border-left:4px solid ", colores$acento,
                                 "; padding:8px 12px;",
                                 " background:#FFF3E0; border-radius:0 6px 6px 0;"),
                  tags$b(class = "small", style = paste0("color:", colores$acento),
                         bs_icon("binoculars-fill", class = "me-1"),
                         "Proceso de detecci\u00f3n por especie s"),
                  p(class = "small mb-1 mt-1",
                    style = "font-family:monospace;",
                    "y[i,j,s] | z[i,s]=1 ~ Bernoulli(p[i,j,s])"),
                  p(class = "small text-muted mb-0",
                    "Los efectos aleatorios por especie se configuran autom\u00e1ticamente.")
                )
              )
            ),

            card(
              card_header(bs_icon("stars", class = "me-1"),
                          "Ventajas del modelo de comunidades"),
              card_body(
                tags$ul(
                  class = "small text-muted mb-0",
                  tags$li(
                    tags$strong("Informaci\u00f3n compartida:"),
                    " especies con pocos datos se benefician de la informaci\u00f3n de ",
                    "especies mejor muestreadas."
                  ),
                  tags$li(
                    tags$strong("Rasgos funcionales:"),
                    " la masa corporal, el gremio o el tama\u00f1o del \u00e1rea de acci\u00f3n ",
                    "pueden explicar diferencias entre especies."
                  ),
                  tags$li(
                    tags$strong("Efectos aleatorios:"),
                    " cada especie tiene su propio intercepto y pendiente, ",
                    "estimados como desviaciones de la media de la comunidad."
                  ),
                  tags$li(
                    tags$strong("Riqueza de especies:"),
                    " se puede estimar la riqueza total incluyendo especies no detectadas."
                  )
                ),
                tags$hr(),
                div(
                  class = "alert alert-info small py-2 px-3 mb-0",
                  bs_icon("info-circle", class = "me-1"),
                  tags$strong("Formato requerido:"),
                  " una matriz de detecci\u00f3n (sitios \u00d7 ocasiones) por especie, ",
                  "covariables de sitio y opcionalmente rasgos funcionales por especie."
                )
              )
            )
          ),

          layout_columns(
            col_widths = c(12),
            class = "mt-3",
            card(
              card_header(bs_icon("table", class = "me-1"),
                          "Estructura de los datos"),
              card_body(
                layout_columns(
                  col_widths = c(4, 4, 4),
                  div(
                    tags$b(class = "small", style = paste0("color:", colores$primario),
                           "Matriz de detecci\u00f3n (por especie)"),
                    p(class = "small text-muted",
                      "Una matriz por especie: sitios (filas) \u00d7 ocasiones (columnas). ",
                      "Valores: 0 = no detectada, 1 = detectada, NA = no visitado."),
                    tags$code(class = "small d-block p-2",
                              style = "background:#f8f9fa; border-radius:4px;",
                              "y[[especie]][sitio, ocasi\u00f3n]")
                  ),
                  div(
                    tags$b(class = "small", style = paste0("color:", colores$primario),
                           "Covariables de sitio"),
                    p(class = "small text-muted",
                      "Un data.frame con una fila por sitio. Pueden incluir ",
                      "elevaci\u00f3n, cobertura forestal, temperatura, etc."),
                    tags$code(class = "small d-block p-2",
                              style = "background:#f8f9fa; border-radius:4px;",
                              "siteCovs[sitio, covaribale]")
                  ),
                  div(
                    tags$b(class = "small", style = paste0("color:", colores$primario),
                           "Rasgos funcionales (por especie)"),
                    p(class = "small text-muted",
                      "Lista con un elemento por rasgo. Cada elemento es un vector ",
                      "de longitud S (n\u00famero de especies)."),
                    tags$code(class = "small d-block p-2",
                              style = "background:#f8f9fa; border-radius:4px;",
                              "speciesCovs$masa[especie]")
                  )
                )
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 2: Los datos
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("database", class = "me-1"), "Los datos"),
        div(class = "p-3",
          layout_columns(
            col_widths = c(4, 8),

            div(
              card(
                class = "mb-3",
                card_header(bs_icon("sliders", class = "me-1"), "Fuente de datos"),
                card_body(
                  radioButtons(
                    ns("fuente_datos"),
                    label = NULL,
                    choices = c(
                      "Dataset simulado (aves)" = "simulado",
                      "Cargar mis datos"        = "propio"
                    ),
                    selected = "simulado"
                  ),
                  conditionalPanel(
                    condition = paste0("input['", ns("fuente_datos"), "'] == 'propio'"),
                    div(
                      class = "alert alert-info small py-2 px-3 mb-2",
                      bs_icon("info-circle", class = "me-1"),
                      "Sube un archivo RDS con una lista que contenga:",
                      tags$ul(
                        class = "mb-0 mt-1",
                        tags$li(code("ylist"), " — lista de matrices por especie"),
                        tags$li(code("sitecov"), " — data.frame de covariables de sitio"),
                        tags$li(code("spcov"), " — lista de rasgos funcionales (opcional)")
                      )
                    ),
                    fileInput(ns("archivo_rds"), label = NULL,
                              accept = ".rds",
                              buttonLabel = "Buscar\u2026",
                              placeholder = "Archivo .rds")
                  )
                )
              ),
              card(
                class = "mb-0",
                card_header(bs_icon("info-circle", class = "me-1"), "Resumen"),
                card_body(uiOutput(ns("resumen_datos")))
              )
            ),

            div(
              card(
                class = "mb-3",
                card_header(bs_icon("table", class = "me-1"),
                            "Matriz de detecci\u00f3n — Especie 1"),
                card_body(
                  DT::DTOutput(ns("tabla_y1"))
                )
              ),
              card(
                card_header(bs_icon("bar-chart", class = "me-1"),
                            "Tasa de detecci\u00f3n por especie"),
                card_body(
                  plotOutput(ns("plot_deteccion"), height = "250px")
                )
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 3: Preparar datos
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("gear", class = "me-1"), "Preparar datos"),
        div(class = "p-3",
          layout_columns(
            col_widths = c(4, 8),

            div(
              card(
                class = "mb-3",
                card_header(bs_icon("sliders", class = "me-1"),
                            "Covariables de sitio"),
                card_body(
                  uiOutput(ns("sel_cov_sitio"))
                )
              ),
              card(
                class = "mb-3",
                card_header(bs_icon("star", class = "me-1"),
                            "Rasgos funcionales (covariables de especie)"),
                card_body(
                  uiOutput(ns("sel_cov_especie")),
                  p(class = "small text-muted mb-0",
                    "Si no hay rasgos disponibles, el modelo usa solo ",
                    "efectos aleatorios por especie.")
                )
              ),
              card(
                class = "mb-3",
                card_header(bs_icon("eye", class = "me-1"),
                            "Covariables de observaci\u00f3n"),
                card_body(
                  uiOutput(ns("sel_cov_obs")),
                  p(class = "small text-muted mb-0",
                    "Covariables que var\u00edan entre sitios y ocasiones. ",
                    "Pueden usarse en la f\u00f3rmula de detecci\u00f3n.")
                )
              ),
              card(
                class = "mb-0",
                card_header(bs_icon("check2-square", class = "me-1"),
                            "Estandarizar covariables"),
                card_body(
                  checkboxInput(ns("estandarizar"), "Estandarizar (media=0, sd=1)",
                                value = TRUE),
                  actionButton(ns("preparar"), "Preparar datos",
                               class = "btn-primary w-100 mt-2",
                               icon  = icon("play")),
                  uiOutput(ns("estado_umf"))
                )
              )
            ),

            card(
              card_header(bs_icon("table", class = "me-1"),
                          "Resumen del unmarkedFrame"),
              card_body(
                uiOutput(ns("summary_umf"))
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 4: Ajustar modelo
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("cpu", class = "me-1"), "Ajustar modelo"),
        div(class = "p-3",
          layout_columns(
            col_widths = c(4, 8),

            div(
              card(
                class = "mb-3",
                card_header(bs_icon("braces", class = "me-1"),
                            "F\u00f3rmulas del modelo"),
                card_body(
                  p(class = "small text-muted mb-2",
                    "Notaci\u00f3n: ", code("occuComm(~detecci\u00f3n ~ocupaci\u00f3n)")),
                  uiOutput(ns("sel_formula_det")),
                  uiOutput(ns("sel_formula_ocu")),
                  tags$hr(),
                  p(class = "small text-muted mb-1",
                    "Los rasgos funcionales seleccionados se incluyen autom\u00e1ticamente ",
                    "como covariables de especie en ocupaci\u00f3n."),
                  div(
                    class = "alert alert-warning small py-2 px-3 mb-0",
                    bs_icon("clock", class = "me-1"),
                    "Este modelo puede tardar varios minutos con muchas especies."
                  )
                )
              ),
              input_task_button(ns("ajustar"), "Ajustar modelo",
                                label_busy = "Ajustando...",
                                class = "btn-primary w-100 mb-2"),
              uiOutput(ns("estado_ajuste"))
            ),

            card(
              card_header(bs_icon("terminal", class = "me-1"),
                          "Salida del modelo"),
              card_body(
                layout_columns(
                  col_widths = c(7, 5),
                  verbatimTextOutput(ns("output_modelo")),
                  div(
                    h6(class = "fw-bold mb-2",
                       style = paste0("color:", colores$primario),
                       "C\u00f3mo interpretar"),
                    div(class = "alert alert-primary small py-2 px-3 mb-2",
                        tags$b("Efectos aleatorios (Random effects):"),
                        tags$br(),
                        "Varianza y SD del intercepto y pendientes entre especies. ",
                        "Una SD alta indica que las especies difieren mucho entre s\u00ed. ",
                        "Los efectos aleatorios permiten que cada especie tenga su propio ",
                        "intercepto y pendiente."),
                    div(class = "alert alert-success small py-2 px-3 mb-2",
                        tags$b("Efectos fijos (Fixed effects):"),
                        tags$br(),
                        "Estimaciones medias de la comunidad. ",
                        "El intercepto (\u03b2\u2080) es la ocupaci\u00f3n media cuando todas las covariables son 0. ",
                        "Las pendientes indican el efecto promedio sobre todas las especies."),
                    div(class = "alert alert-warning small py-2 px-3 mb-0",
                        tags$b("AIC:"),
                        " Criterio de informaci\u00f3n de Akaike. Menor es mejor para comparar modelos. ",
                        tags$b("Number of sites removed:"),
                        " sitios excluidos por datos faltantes.")
                  )
                )
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # ════════════════════════════════════════════════
      # PESTAÑA 5: Parámetros
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("list-ol", class = "me-1"), "Par\u00e1metros"),
        div(
          class = "p-3",
          p(class = "small text-muted mb-3",
            "Estimaciones en escala logit. Los efectos fijos representan la media ",
            "de la comunidad; cada especie tiene adem\u00e1s un intercepto aleatorio propio."),
          uiOutput(ns("vbox_aic")),
          div(class = "mt-3",
            h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
               "Tabla de coeficientes con IC 95%"),
            uiOutput(ns("tabla_params"))
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 6: Gráficos
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("graph-up-arrow", class = "me-1"), "Gr\u00e1ficos"),
        div(
          class = "p-3",
          layout_columns(
            col_widths = c(6, 6),
            card(
              card_header(bs_icon("bar-chart-steps", class = "me-1"),
                          "Coeficientes del modelo"),
              card_body(
                p(class = "small text-muted mb-2",
                  "Estimaciones en escala logit con IC 95%. ",
                  "Valores positivos aumentan \u03c8 o p; negativos la reducen."),
                plotOutput(ns("plot_coefs"), height = "280px")
              )
            ),
            card(
              card_header(bs_icon("list-ul", class = "me-1"),
                          "Distribuci\u00f3n de \u03c8 por especie"),
              card_body(
                p(class = "small text-muted mb-2",
                  "\u03c8 predicha en cada sitio para cada especie (boxplot). ",
                  "Muestra c\u00f3mo var\u00eda la ocupaci\u00f3n entre sitios, ",
                  "reflejando el efecto de las covariables de sitio."),
                plotOutput(ns("plot_caterpillar"), height = "280px")
              )
            )
          ),
          div(class = "mt-3",
            card(
              card_header(bs_icon("bezier2", class = "me-1"),
                          "Efectos marginales"),
              card_body(
                layout_columns(
                  col_widths = c(3, 9),
                  div(uiOutput(ns("sel_cov_marginal"))),
                  plotOutput(ns("plot_marginal"), height = "420px")
                )
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 7: Diagnóstico
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("clipboard-check", class = "me-1"), "Diagn\u00f3stico"),
        div(class = "p-3",
          layout_columns(
            col_widths = c(6, 6),
            card(
              card_header(bs_icon("graph-up", class = "me-1"),
                          "Distribuci\u00f3n de efectos aleatorios por especie"),
              card_body(
                p(class = "small text-muted mb-2",
                  "Si los efectos aleatorios siguen una distribuci\u00f3n normal centrada en 0, ",
                  "el supuesto del modelo se cumple."),
                plotOutput(ns("plot_random_effects"), height = "350px")
              )
            ),
            card(
              card_header(bs_icon("exclamation-triangle", class = "me-1"),
                          "Bondad de ajuste (puede ser lento)"),
              card_body(
                layout_columns(
                  col_widths = c(6, 6),
                  numericInput(ns("nsim_gof"), "Simulaciones:", value = 100,
                               min = 50, max = 1000, step = 50),
                  input_task_button(ns("correr_gof"), "Correr GoF",
                                    label_busy = "Corriendo...",
                                    class = "btn-outline-primary btn-sm w-100 mt-4")
                ),
                uiOutput(ns("resultado_gof"))
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 8: Predicciones
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("pin-map", class = "me-1"), "Predicciones"),
        div(class = "p-3",
          layout_columns(
            col_widths = c(4, 8),
            div(
              card(
                class = "mb-3",
                card_header(bs_icon("sliders", class = "me-1"), "Opciones"),
                card_body(
                  uiOutput(ns("sel_especie_pred")),
                  selectInput(ns("tipo_pred"), "Tipo de predicci\u00f3n:",
                              choices = c("Ocupaci\u00f3n (\u03c8)" = "state",
                                          "Detecci\u00f3n (p)"    = "det"),
                              selected = "state"),
                  actionButton(ns("predecir"), "Calcular predicciones",
                               class = "btn-primary w-100",
                               icon  = icon("calculator"))
                )
              ),
              card(
                card_header(bs_icon("info-circle", class = "me-1"),
                            "Riqueza local esperada"),
                card_body(uiOutput(ns("riqueza_estimada")))
              )
            ),
            div(
              card(
                class = "mb-3",
                card_header(bs_icon("bar-chart", class = "me-1"),
                            "Ocupaci\u00f3n predicha por especie"),
                card_body(plotOutput(ns("plot_pred_especies"), height = "350px"))
              ),
              card(
                card_header(bs_icon("table", class = "me-1"),
                            "Tabla de predicciones"),
                card_body(DT::DTOutput(ns("tabla_predicciones")))
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 9: Código R
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("code-slash", class = "me-1"), "C\u00f3digo R"),
        div(class = "p-3",
          layout_columns(
            col_widths = c(2, 10),
            div(downloadButton(ns("descargar_codigo"), "Descargar .R",
                               class = "btn-outline-primary btn-sm w-100")),
            verbatimTextOutput(ns("codigo_r"))
          )
        )
      )

    ) # /navset_card_tab
  ) # /tagList
} # /mod_occu_community_ui

# ── SERVER ────────────────────────────────────────────────
mod_occu_community_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Datos ────────────────────────────────────────────
    datos_activos <- reactiveVal(.cargar_datos_comunidad())
    umf_obj       <- reactiveVal(NULL)
    modelo_fit    <- reactiveVal(NULL)
    scale_info    <- reactiveVal(list())  # guarda center/scale por covariable

    # Cargar datos propios
    observeEvent(input$archivo_rds, {
      tryCatch({
        d <- readRDS(input$archivo_rds$datapath)
        req(is.list(d), !is.null(d$ylist), !is.null(d$sitecov))
        datos_activos(d)
        showNotification("Datos cargados correctamente.", type = "message")
      }, error = function(e) {
        showNotification(paste("Error:", conditionMessage(e)), type = "error")
      })
    })

    # Cambio de fuente
    observeEvent(input$fuente_datos, {
      if (input$fuente_datos == "simulado") {
        datos_activos(.cargar_datos_comunidad())
      }
    })

    # Resumen de datos
    output$resumen_datos <- renderUI({
      d <- datos_activos(); req(d)
      div(
        tags$ul(class = "small mb-0",
          tags$li(strong("Especies: "), length(d$ylist)),
          tags$li(strong("Sitios: "), nrow(d$sitecov)),
          tags$li(strong("Ocasiones: "), ncol(d$ylist[[1]])),
          tags$li(strong("Cov. sitio: "),
                  paste(names(d$sitecov), collapse = ", ")),
          if (!is.null(d$spcov))
            tags$li(strong("Rasgos: "),
                    paste(names(d$spcov), collapse = ", ")),
          if (!is.null(d$obscov))
            tags$li(strong("Cov. observaci\u00f3n: "),
                    paste(names(d$obscov), collapse = ", "))
        )
      )
    })

    # Tabla detección especie 1
    output$tabla_y1 <- DT::renderDT({
      d <- datos_activos(); req(d)
      df <- as.data.frame(d$ylist[[1]])
      names(df) <- paste0("Ocasi\u00f3n ", seq_len(ncol(df)))
      rownames(df) <- paste0("Sitio ", seq_len(nrow(df)))
      DT::datatable(df, options = list(pageLength = 8, scrollX = TRUE),
                    class = "table-sm table-striped")
    })

    # Plot tasa de detección por especie
    output$plot_deteccion <- renderPlot({
      d <- datos_activos(); req(d)
      tasas <- sapply(d$ylist, function(y) mean(y, na.rm = TRUE))
      df <- data.frame(
        especie = factor(names(tasas), levels = names(tasas)),
        tasa    = tasas
      )
      ggplot2::ggplot(df, ggplot2::aes(x = especie, y = tasa)) +
        ggplot2::geom_col(fill = colores$primario, alpha = 0.8) +
        ggplot2::labs(x = NULL, y = "Tasa de detecci\u00f3n bruta") +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          panel.grid.minor = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8)
        )
    })

    # ── Preparar datos ───────────────────────────────────

    # Selector covariables de sitio
    output$sel_cov_sitio <- renderUI({
      d <- datos_activos(); req(d)
      checkboxGroupInput(
        ns("cov_sitio"),
        label   = "Selecciona covariables de sitio:",
        choices = names(d$sitecov),
        selected = names(d$sitecov)
      )
    })

    # Selector rasgos funcionales
    output$sel_cov_especie <- renderUI({
      d <- datos_activos(); req(d)
      if (is.null(d$spcov)) {
        p(class = "small text-muted", "No hay rasgos funcionales disponibles.")
      } else {
        checkboxGroupInput(
          ns("cov_especie"),
          label   = "Selecciona rasgos funcionales:",
          choices = names(d$spcov),
          selected = names(d$spcov)
        )
      }
    })

    # Selector covariables de observación
    output$sel_cov_obs <- renderUI({
      d <- datos_activos(); req(d)
      if (is.null(d$obscov) || length(d$obscov) == 0) {
        p(class = "small text-muted", "No hay covariables de observaci\u00f3n disponibles.")
      } else {
        checkboxGroupInput(
          ns("cov_obs"),
          label    = "Selecciona covariables de observaci\u00f3n:",
          choices  = names(d$obscov),
          selected = names(d$obscov)
        )
      }
    })

    # Preparar unmarkedFrame
    observeEvent(input$preparar, {
      d <- datos_activos(); req(d)
      tryCatch({
        sitecov <- d$sitecov
        if (!is.null(input$cov_sitio) && length(input$cov_sitio) > 0) {
          sitecov <- sitecov[, input$cov_sitio, drop = FALSE]
        }
        if (isTRUE(input$estandarizar)) {
          sitecov_scaled <- scale(sitecov)
          si <- list()
          for (v in names(sitecov)) {
            si[[v]] <- list(
              center = attr(sitecov_scaled, "scaled:center")[v],
              scale  = attr(sitecov_scaled, "scaled:scale")[v],
              raw    = sitecov[[v]]
            )
          }
          scale_info(si)
          sitecov <- as.data.frame(sitecov_scaled)
        } else {
          scale_info(list())
        }

        spcov <- NULL
        if (!is.null(d$spcov) && !is.null(input$cov_especie) &&
            length(input$cov_especie) > 0) {
          spcov <- d$spcov[input$cov_especie]
        }

        obscov_umf <- NULL
        if (!is.null(d$obscov) && !is.null(input$cov_obs) &&
            length(input$cov_obs) > 0) {
          obscov_umf <- d$obscov[input$cov_obs]
        }
        umf <- unmarked::unmarkedFrameOccuComm(
          y           = d$ylist,
          siteCovs    = sitecov,
          obsCovs     = obscov_umf,
          speciesCovs = spcov
        )
        umf_obj(umf)
        showNotification("unmarkedFrame preparado.", type = "message", duration = 3)
      }, error = function(e) {
        showNotification(paste("Error:", conditionMessage(e)), type = "error")
      })
    })

    output$estado_umf <- renderUI({
      if (is.null(umf_obj())) return(NULL)
      div(class = "alert alert-success small py-2 px-3 mt-2 mb-0",
          bs_icon("check-circle-fill", class = "me-1"),
          "unmarkedFrame listo.")
    })

    output$summary_umf <- renderUI({
      umf <- umf_obj()
      if (is.null(umf)) return(p(class = "small text-muted", "Prepara los datos primero."))
      
      d <- datos_activos()
      nsp    <- length(umf@ylist)
      nsite  <- nrow(umf@siteCovs)
      nocc   <- ncol(umf@ylist[[1]])
      
      # Conteo de detecciones
      y_all <- do.call(rbind, lapply(umf@ylist, function(y) as.vector(y)))
      n0   <- sum(y_all == 0, na.rm = TRUE)
      n1   <- sum(y_all == 1, na.rm = TRUE)
      nna  <- sum(is.na(y_all))
      
      div(
        tags$ul(class = "small mb-2",
          tags$li(strong("Especies: "), nsp),
          tags$li(strong("Sitios: "), nsite),
          tags$li(strong("Ocasiones por sitio: "), nocc),
          tags$li(strong("Detecciones (y=1): "), n1),
          tags$li(strong("No detecciones (y=0): "), n0),
          tags$li(strong("Datos faltantes (NA): "), nna)
        ),
        tags$hr(),
        p(class = "small fw-bold mb-1", "Covariables de sitio:"),
        tags$ul(class = "small mb-0",
          lapply(names(umf@siteCovs), tags$li)
        ),
        if (!is.null(umf@speciesCovs)) {
          tagList(
            tags$hr(),
            p(class = "small fw-bold mb-1", "Rasgos funcionales:"),
            tags$ul(class = "small mb-0",
              lapply(names(umf@speciesCovs), tags$li)
            )
          )
        }
      )
    })

    # ── Fórmulas ─────────────────────────────────────────

    output$sel_formula_det <- renderUI({
      d <- datos_activos(); req(d)
      cov_obs <- if (!is.null(input$cov_obs) && length(input$cov_obs) > 0)
        input$cov_obs
      else if (!is.null(d$obscov)) names(d$obscov)
      else character(0)
      choices_det <- c("Solo intercepto" = "1",
                       setNames(cov_obs, cov_obs))
      tagList(
        p(class = "small fw-bold mb-1",
          "Detecci\u00f3n (p):"),
        checkboxGroupInput(
          ns("formula_det"),
          label    = NULL,
          choices  = choices_det,
          selected = character(0)
        )
      )
    })

    output$sel_formula_ocu <- renderUI({
      d <- datos_activos(); req(d)
      cov_sit <- if (!is.null(input$cov_sitio)) input$cov_sitio else character(0)
      cov_sp  <- if (!is.null(input$cov_especie)) input$cov_especie else character(0)
      todas   <- c(cov_sit, cov_sp)
      choices_ocu <- c("Solo intercepto" = "1",
                       setNames(todas, todas))
      tagList(
        p(class = "small fw-bold mb-1",
          "Ocupaci\u00f3n (\u03c8):"),
        checkboxGroupInput(
          ns("formula_ocu"),
          label    = NULL,
          choices  = choices_ocu,
          selected = character(0)
        )
      )
    })

    # ── Ajustar modelo ───────────────────────────────────

    observeEvent(input$ajustar, {
      umf <- umf_obj()
      if (is.null(umf)) {
        showNotification("Prepara los datos primero.", type = "warning"); return()
      }

      # Excluir "1" si hay otras covariables seleccionadas
      ocu_sel  <- input$formula_ocu[input$formula_ocu != "1"]
      ocu_form <- if (is.null(ocu_sel) || length(ocu_sel) == 0) "1" else paste(ocu_sel, collapse = " + ")
      det_sel  <- input$formula_det[input$formula_det != "1"]
      det_form <- if (is.null(det_sel) || length(det_sel) == 0) "1" else paste(det_sel, collapse = " + ")

      # Avisar si ninguna covariable seleccionada
      if (ocu_form == "1" && det_form == "1") {
        showNotification(
          "Ninguna covariable seleccionada \u2014 se ajustar\u00e1 un modelo nulo (~1 ~1).",
          type = "message", duration = 5)
      }

      formula_str <- paste0("~", det_form, " ~", ocu_form)

      withProgress(message = "Ajustando occuComm\u2026 (puede tardar varios minutos)", {
        tryCatch({
          fm <- unmarked::occuComm(
            formula = as.formula(formula_str),
            data    = umf
          )
          modelo_fit(fm)
          showNotification("Modelo ajustado.", type = "message", duration = 4)
        }, error = function(e) {
          showNotification(paste("Error:", conditionMessage(e)),
                           type = "error", duration = 10)
        })
      })
    })

    output$estado_ajuste <- renderUI({
      if (is.null(modelo_fit())) return(NULL)
      div(class = "alert alert-success small py-2 px-3 mt-2 mb-0",
          bs_icon("check-circle-fill", class = "me-1"),
          "Modelo ajustado correctamente.")
    })

    # Value box AIC
    output$vbox_aic <- renderUI({
      fm <- modelo_fit(); req(fm)
      aic_val <- tryCatch(fm@AIC, error = function(e) NA)
      if (is.na(aic_val)) return(NULL)
      div(class = "alert alert-info small py-2 px-3 mb-0",
          bs_icon("info-circle", class = "me-1"),
          strong("AIC: "), round(aic_val, 2))
    })

    output$output_modelo <- renderPrint({
      fm <- modelo_fit()
      if (is.null(fm)) cat("Ajusta el modelo primero.")
      else fm
    })

    # ── Resultados ───────────────────────────────────────

    output$tabla_params <- renderUI({
      fm <- modelo_fit(); req(fm)
      tryCatch({
        cf_s <- coef(fm["state"])
        cf_d <- coef(fm["det"])
        fijos <- c(cf_s, cf_d)
        df <- data.frame(
          Parametro = names(fijos),
          Estimado  = round(as.numeric(fijos), 4)
        )
        ci_s <- tryCatch(confint(fm, type="state", method="normal"), error=function(e) NULL)
        ci_d <- tryCatch(confint(fm, type="det",   method="normal"), error=function(e) NULL)
        if (!is.null(ci_s) && !is.null(ci_d)) {
          ci_all <- rbind(ci_s, ci_d)
          ci_sub <- ci_all[rownames(ci_all) %in% names(fijos), , drop=FALSE]
          if (nrow(ci_sub) == nrow(df)) {
            df$IC_inf <- round(ci_sub[,1], 4)
            df$IC_sup <- round(ci_sub[,2], 4)
          }
        }
        tags$table(
          class = "table table-sm small mb-0",
          tags$thead(style = paste0("background:", colores$primario, "; color:#fff;"),
                     tags$tr(lapply(names(df), tags$th))),
          tags$tbody(lapply(seq_len(nrow(df)), function(i)
            tags$tr(lapply(df[i,], tags$td))))
        )
      }, error = function(e)
        p(class="small text-danger",
          bs_icon("exclamation-circle", class="me-1"), conditionMessage(e)))
    })

    output$plot_coefs <- renderPlot({
      fm <- modelo_fit(); req(fm)
      tryCatch({
        cf_s <- coef(fm["state"])
        cf_d <- coef(fm["det"])
        fijos <- c(cf_s, cf_d)
        df <- data.frame(param = names(fijos), valor = as.numeric(fijos))
        ci_s <- tryCatch(confint(fm, type="state", method="normal"), error=function(e) NULL)
        ci_d <- tryCatch(confint(fm, type="det",   method="normal"), error=function(e) NULL)
        if (!is.null(ci_s) && !is.null(ci_d)) {
          ci_all <- rbind(ci_s, ci_d)
          ci_sub <- ci_all[rownames(ci_all) %in% names(fijos), , drop=FALSE]
          if (nrow(ci_sub) == nrow(df)) { df$lo <- ci_sub[,1]; df$hi <- ci_sub[,2] }
        }
        p <- ggplot2::ggplot(df, ggplot2::aes(x=valor, y=reorder(param,valor))) +
          ggplot2::geom_vline(xintercept=0, linetype="dashed", color=colores$texto, alpha=0.5) +
          ggplot2::geom_point(color=colores$primario, size=3) +
          ggplot2::labs(x="Estimado (escala logit)", y=NULL) +
          ggplot2::theme_minimal(base_size=11)
        if (!is.null(df$lo))
          p <- p + ggplot2::geom_errorbarh(ggplot2::aes(xmin=lo, xmax=hi),
                                            height=0.3, color=colores$secundario)
        p
      }, error=function(e) NULL)
    })

    # Selector covariables de sitio
    output$sel_cov_sitio <- renderUI({
      d <- datos_activos(); req(d)
      checkboxGroupInput(
        ns("cov_sitio"),
        label   = "Selecciona covariables de sitio:",
        choices = names(d$sitecov),
        selected = names(d$sitecov)
      )
    })

    # Selector rasgos funcionales
    output$sel_cov_especie <- renderUI({
      d <- datos_activos(); req(d)
      if (is.null(d$spcov)) {
        p(class = "small text-muted", "No hay rasgos funcionales disponibles.")
      } else {
        checkboxGroupInput(
          ns("cov_especie"),
          label   = "Selecciona rasgos funcionales:",
          choices = names(d$spcov),
          selected = names(d$spcov)
        )
      }
    })

    # Selector covariables de observación
    output$sel_cov_obs <- renderUI({
      d <- datos_activos(); req(d)
      if (is.null(d$obscov) || length(d$obscov) == 0) {
        p(class = "small text-muted", "No hay covariables de observaci\u00f3n disponibles.")
      } else {
        checkboxGroupInput(
          ns("cov_obs"),
          label    = "Selecciona covariables de observaci\u00f3n:",
          choices  = names(d$obscov),
          selected = names(d$obscov)
        )
      }
    })


    output$estado_umf <- renderUI({
      if (is.null(umf_obj())) return(NULL)
      div(class = "alert alert-success small py-2 px-3 mt-2 mb-0",
          bs_icon("check-circle-fill", class = "me-1"),
          "unmarkedFrame listo.")
    })

    output$summary_umf <- renderUI({
      umf <- umf_obj()
      if (is.null(umf)) return(p(class = "small text-muted", "Prepara los datos primero."))
      
      d <- datos_activos()
      nsp    <- length(umf@ylist)
      nsite  <- nrow(umf@siteCovs)
      nocc   <- ncol(umf@ylist[[1]])
      
      # Conteo de detecciones
      y_all <- do.call(rbind, lapply(umf@ylist, function(y) as.vector(y)))
      n0   <- sum(y_all == 0, na.rm = TRUE)
      n1   <- sum(y_all == 1, na.rm = TRUE)
      nna  <- sum(is.na(y_all))
      
      div(
        tags$ul(class = "small mb-2",
          tags$li(strong("Especies: "), nsp),
          tags$li(strong("Sitios: "), nsite),
          tags$li(strong("Ocasiones por sitio: "), nocc),
          tags$li(strong("Detecciones (y=1): "), n1),
          tags$li(strong("No detecciones (y=0): "), n0),
          tags$li(strong("Datos faltantes (NA): "), nna)
        ),
        tags$hr(),
        p(class = "small fw-bold mb-1", "Covariables de sitio:"),
        tags$ul(class = "small mb-0",
          lapply(names(umf@siteCovs), tags$li)
        ),
        if (!is.null(umf@speciesCovs)) {
          tagList(
            tags$hr(),
            p(class = "small fw-bold mb-1", "Rasgos funcionales:"),
            tags$ul(class = "small mb-0",
              lapply(names(umf@speciesCovs), tags$li)
            )
          )
        }
      )
    })

    # ── Fórmulas ─────────────────────────────────────────

    output$sel_formula_det <- renderUI({
      d <- datos_activos(); req(d)
      cov_obs <- if (!is.null(input$cov_obs) && length(input$cov_obs) > 0)
        input$cov_obs
      else if (!is.null(d$obscov)) names(d$obscov)
      else character(0)
      choices_det <- c("Solo intercepto" = "1",
                       setNames(cov_obs, cov_obs))
      tagList(
        p(class = "small fw-bold mb-1",
          "Detecci\u00f3n (p):"),
        checkboxGroupInput(
          ns("formula_det"),
          label    = NULL,
          choices  = choices_det,
          selected = character(0)
        )
      )
    })

    output$sel_formula_ocu <- renderUI({
      d <- datos_activos(); req(d)
      cov_sit <- if (!is.null(input$cov_sitio)) input$cov_sitio else character(0)
      cov_sp  <- if (!is.null(input$cov_especie)) input$cov_especie else character(0)
      todas   <- c(cov_sit, cov_sp)
      choices_ocu <- c("Solo intercepto" = "1",
                       setNames(todas, todas))
      tagList(
        p(class = "small fw-bold mb-1",
          "Ocupaci\u00f3n (\u03c8):"),
        checkboxGroupInput(
          ns("formula_ocu"),
          label    = NULL,
          choices  = choices_ocu,
          selected = character(0)
        )
      )
    })

    # ── Ajustar modelo ───────────────────────────────────

    observeEvent(input$ajustar, {
      umf <- umf_obj()
      if (is.null(umf)) {
        showNotification("Prepara los datos primero.", type = "warning"); return()
      }

      # Excluir "1" si hay otras covariables seleccionadas
      ocu_sel  <- input$formula_ocu[input$formula_ocu != "1"]
      ocu_form <- if (is.null(ocu_sel) || length(ocu_sel) == 0) "1" else paste(ocu_sel, collapse = " + ")
      det_sel  <- input$formula_det[input$formula_det != "1"]
      det_form <- if (is.null(det_sel) || length(det_sel) == 0) "1" else paste(det_sel, collapse = " + ")

      # Avisar si ninguna covariable seleccionada
      if (ocu_form == "1" && det_form == "1") {
        showNotification(
          "Ninguna covariable seleccionada \u2014 se ajustar\u00e1 un modelo nulo (~1 ~1).",
          type = "message", duration = 5)
      }

      formula_str <- paste0("~", det_form, " ~", ocu_form)

      withProgress(message = "Ajustando occuComm\u2026 (puede tardar varios minutos)", {
        tryCatch({
          fm <- unmarked::occuComm(
            formula = as.formula(formula_str),
            data    = umf
          )
          modelo_fit(fm)
          showNotification("Modelo ajustado.", type = "message", duration = 4)
        }, error = function(e) {
          showNotification(paste("Error:", conditionMessage(e)),
                           type = "error", duration = 10)
        })
      })
    })

    output$estado_ajuste <- renderUI({
      if (is.null(modelo_fit())) return(NULL)
      div(class = "alert alert-success small py-2 px-3 mt-2 mb-0",
          bs_icon("check-circle-fill", class = "me-1"),
          "Modelo ajustado correctamente.")
    })

    # Value box AIC
    output$vbox_aic <- renderUI({
      fm <- modelo_fit(); req(fm)
      aic_val <- tryCatch(fm@AIC, error = function(e) NA)
      if (is.na(aic_val)) return(NULL)
      div(class = "alert alert-info small py-2 px-3 mb-0",
          bs_icon("info-circle", class = "me-1"),
          strong("AIC: "), round(aic_val, 2))
    })

    output$output_modelo <- renderPrint({
      fm <- modelo_fit()
      if (is.null(fm)) cat("Ajusta el modelo primero.")
      else fm
    })

    # ── Resultados ───────────────────────────────────────

    output$tabla_params <- renderUI({
      fm <- modelo_fit(); req(fm)
      tryCatch({
        cf_s <- coef(fm["state"])
        cf_d <- coef(fm["det"])
        fijos <- c(cf_s, cf_d)
        df <- data.frame(
          Parametro = names(fijos),
          Estimado  = round(as.numeric(fijos), 4)
        )
        ci_s <- tryCatch(confint(fm, type="state", method="normal"), error=function(e) NULL)
        ci_d <- tryCatch(confint(fm, type="det",   method="normal"), error=function(e) NULL)
        if (!is.null(ci_s) && !is.null(ci_d)) {
          ci_all <- rbind(ci_s, ci_d)
          ci_sub <- ci_all[rownames(ci_all) %in% names(fijos), , drop=FALSE]
          if (nrow(ci_sub) == nrow(df)) {
            df$IC_inf <- round(ci_sub[,1], 4)
            df$IC_sup <- round(ci_sub[,2], 4)
          }
        }
        tags$table(
          class = "table table-sm small mb-0",
          tags$thead(style = paste0("background:", colores$primario, "; color:#fff;"),
                     tags$tr(lapply(names(df), tags$th))),
          tags$tbody(lapply(seq_len(nrow(df)), function(i)
            tags$tr(lapply(df[i,], tags$td))))
        )
      }, error = function(e)
        p(class="small text-danger",
          bs_icon("exclamation-circle", class="me-1"), conditionMessage(e)))
    })

    output$plot_coefs <- renderPlot({
      fm <- modelo_fit(); req(fm)
      cf_state <- tryCatch(coef(fm["state"]), error = function(e) NULL)
      cf_det   <- tryCatch(coef(fm["det"]),   error = function(e) NULL)
      req(!is.null(cf_state))
      fijos <- c(cf_state, if (!is.null(cf_det)) cf_det else numeric(0))
      req(length(fijos) > 0)
      df <- data.frame(param = names(fijos), valor = as.numeric(fijos))
      ci_state <- tryCatch(confint(fm, type = "state", method = "normal"), error = function(e) NULL)
      ci_det   <- tryCatch(confint(fm, type = "det", method = "normal"),   error = function(e) NULL)
      if (!is.null(ci_state) && !is.null(ci_det)) {
        ci_all <- rbind(ci_state, ci_det)
        ci_sub <- ci_all[rownames(ci_all) %in% names(fijos), , drop = FALSE]
        if (nrow(ci_sub) == nrow(df)) {
          df$lo <- ci_sub[, 1]; df$hi <- ci_sub[, 2]
        }
      }
      p <- ggplot2::ggplot(df, ggplot2::aes(x = valor, y = reorder(param, valor))) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                            color = colores$texto, alpha = 0.5) +
        ggplot2::geom_point(color = colores$primario, size = 3) +
        ggplot2::labs(x = "Estimado (escala logit)", y = NULL) +
        ggplot2::theme_minimal(base_size = 11)
      if (!is.null(df$lo)) {
        p <- p + ggplot2::geom_errorbarh(
          ggplot2::aes(xmin = lo, xmax = hi),
          height = 0.3, color = colores$secundario)
      }
      p
    })

    # ── Efectos marginales ────────────────────────────────

    output$sel_cov_marginal <- renderUI({
      fm  <- modelo_fit(); req(fm)
      umf <- umf_obj(); req(umf)
      submod <- input$submod_marginal %||% "state"
      # Mostrar solo covariables que están EN el modelo
      if (submod == "state") {
        cf      <- coef(fm["state"])
        en_modelo <- names(umf@siteCovs)[sapply(names(umf@siteCovs), function(v)
          any(grepl(v, names(cf))))]
        cov_choices <- if (length(en_modelo) > 0) en_modelo else names(umf@siteCovs)
      } else {
        cf_det    <- coef(fm["det"])
        obs_names <- if (!is.null(umf@obsCovs)) names(umf@obsCovs) else character(0)
        en_modelo <- obs_names[sapply(obs_names, function(v)
          any(grepl(v, names(cf_det))))]
        cov_choices <- if (length(en_modelo) > 0) en_modelo else obs_names
      }
      sp_names <- names(umf@ylist)
      tagList(
        selectInput(ns("submod_marginal"), "Submodelo:",
                    choices  = c("Ocupaci\u00f3n (\u03c8)" = "state",
                                 "Detecci\u00f3n (p)"      = "det"),
                    selected = submod),
        if (length(cov_choices) > 0)
          selectInput(ns("cov_marginal"), "Covariable:",
                      choices = cov_choices, selected = cov_choices[1])
        else
          p(class = "small text-muted",
            "No hay covariables para este submodelo en el modelo ajustado."),
        selectInput(ns("especie_marginal"), "Especie:",
                    choices = sp_names, selected = sp_names[1])
      )
    })

    output$plot_marginal <- renderPlot({
      fm       <- modelo_fit(); req(fm)
      umf      <- umf_obj(); req(umf)
      d        <- datos_activos(); req(d)
      submod   <- input$submod_marginal %||% "state"
      especie  <- input$especie_marginal
      sp_names <- names(umf@ylist)
      if (is.null(especie) || !especie %in% sp_names) especie <- sp_names[1]
      tryCatch({
        # Determinar sc según submodelo
        if (submod == "state") {
          sc   <- umf@siteCovs
          # Usar valores originales para eje x si están disponibles
          si   <- scale_info()
          type <- "state"
          ylab <- "Ocupaci\u00f3n (\u03c8)"
        } else {
          if (is.null(umf@obsCovs) || nrow(umf@obsCovs) == 0) {
            return(ggplot2::ggplot() +
              ggplot2::annotate("text", x=0.5, y=0.5,
                label="No hay covariables de observaci\u00f3n en el modelo.",
                size=5, color="gray50") +
              ggplot2::theme_void())
          }
          nsite   <- nrow(umf@siteCovs)
          sc      <- umf@obsCovs[1:nsite, , drop = FALSE]
          si      <- list()  # obs covs no tienen scale_info por ahora
          type    <- "det"
          ylab    <- "Detecci\u00f3n (p)"
        }
        cov_name <- input$cov_marginal
        if (is.null(cov_name) || !cov_name %in% names(sc)) cov_name <- names(sc)[1]
        req(cov_name)
        cov_seq <- seq(min(sc[[cov_name]], na.rm = TRUE),
                       max(sc[[cov_name]], na.rm = TRUE), length.out = 50)
        # Eje x: valores originales si hay scale_info, si no los estandarizados
        if (!is.null(si[[cov_name]])) {
          cov_seq_raw <- si[[cov_name]]$center + si[[cov_name]]$scale * cov_seq
          xlab <- cov_name
        } else {
          cov_seq_raw <- cov_seq
          xlab <- paste0(cov_name, " (estandarizado)")
        }
        nd <- as.data.frame(lapply(names(sc), function(v)
          rep(mean(sc[[v]], na.rm = TRUE), 50)))
        names(nd) <- names(sc)
        nd[[cov_name]] <- cov_seq
        nd$species     <- especie
        pr <- unmarked::predict(fm, type = type, newdata = nd)
        req(is.data.frame(pr))
        df_plot <- data.frame(cov = cov_seq_raw, est = pr$Predicted,
                              lo = pr$lower, hi = pr$upper)
        ggplot2::ggplot(df_plot, ggplot2::aes(x = cov, y = est)) +
          ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi),
                               fill = colores$secundario, alpha = 0.25) +
          ggplot2::geom_line(color = colores$primario, linewidth = 1.2) +
          ggplot2::scale_y_continuous(limits = c(0, 1)) +
          ggplot2::labs(x = xlab, y = ylab,
                        title = especie,
                        caption = "IC 95%") +
          ggplot2::theme_minimal(base_size = 12) +
          ggplot2::theme(
            panel.grid.minor = ggplot2::element_blank(),
            plot.title = ggplot2::element_text(color = colores$primario, face = "bold")
          )
      }, error = function(e) NULL)
    })

    # Caterpillar plot de ocupación por especie
    output$plot_caterpillar <- renderPlot({
      fm <- modelo_fit(); req(fm)
      d  <- datos_activos(); req(d)
      tryCatch({
        preds <- unmarked::predict(fm, type = "state")
        # ψ por sitio para cada especie → boxplot
        df <- do.call(rbind, lapply(names(preds), function(sp) {
          data.frame(especie = sp,
                     psi     = preds[[sp]]$Predicted)
        }))
        # Ordenar especies por mediana de ψ
        ord <- tapply(df$psi, df$especie, median)
        df$especie <- factor(df$especie,
                             levels = names(sort(ord)))
        ggplot2::ggplot(df, ggplot2::aes(x = psi, y = especie)) +
          ggplot2::geom_boxplot(fill = colores$secundario,
                                color = colores$primario,
                                alpha = 0.7, outlier.size = 0.8) +
          ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed",
                              color = colores$texto, alpha = 0.4) +
          ggplot2::scale_x_continuous(limits = c(0, 1)) +
          ggplot2::labs(
            x = "Distribuci\u00f3n de \u03c8 entre sitios",
            y = NULL,
            caption = "Cada caja muestra la variaci\u00f3n de \u03c8 entre sitios para cada especie"
          ) +
          ggplot2::theme_minimal(base_size = 10) +
          ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
      }, error = function(e) NULL)
    })

    # Distribución de efectos aleatorios
    output$plot_random_effects <- renderPlot({
      fm <- modelo_fit(); req(fm)
      tryCatch({
        cf_state <- tryCatch(coef(fm["state"]), error = function(e) numeric(0))
        cf_det   <- tryCatch(coef(fm["det"]),   error = function(e) numeric(0))
        cf_all   <- c(cf_state, cf_det)
        re_idx   <- grepl("\\[", names(cf_all))
        re_vals  <- cf_all[re_idx]
        if (length(re_vals) == 0) return(NULL)
        df <- data.frame(valor = as.numeric(re_vals))
        ggplot2::ggplot(df, ggplot2::aes(x = valor)) +
          ggplot2::geom_histogram(fill = colores$primario, color = "white",
                                  bins = 15, alpha = 0.8) +
          ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                              color = colores$peligro) +
          ggplot2::labs(x = "Efecto aleatorio (escala logit)",
                        y = "Frecuencia",
                        title = "Distribuci\u00f3n de efectos aleatorios por especie") +
          ggplot2::theme_minimal(base_size = 11) +
          ggplot2::theme(plot.title = ggplot2::element_text(
            color = colores$primario, face = "bold", size = 12))
      }, error = function(e) NULL)
    })

    # GoF
    gof_resultado <- reactiveVal(NULL)
    observeEvent(input$correr_gof, {
      fm <- modelo_fit()
      if (is.null(fm)) {
        showNotification("Ajusta el modelo primero.", type = "warning"); return()
      }
      withProgress(message = "Corriendo GoF\u2026 (puede ser lento)", {
        tryCatch({
          SSE_comm <- function(fm) {
            obs_mat  <- unmarked::getY(fm@data)   # matrix
            fit_list <- unmarked::fitted(fm)        # list
            # fit_list tiene una matriz por especie - sumar sobre todas
            fit_mat  <- do.call(cbind, lapply(fit_list, as.vector))
            obs_vec  <- as.vector(obs_mat)
            fit_vec  <- as.vector(fit_mat)
            idx      <- !is.na(obs_vec) & !is.na(fit_vec)
            sum((obs_vec[idx] - fit_vec[idx])^2)
          }
          res <- unmarked::parboot(fm, statistic = SSE_comm,
                                   nsim = input$nsim_gof, parallel = FALSE)
          gof_resultado(res)
        }, error = function(e) {
          showNotification(paste("Error GoF:", conditionMessage(e)),
                           type = "error", duration = 8)
        })
      })
    })

    output$resultado_gof <- renderUI({
      res <- gof_resultado()
      if (is.null(res)) return(
        p(class = "small text-muted", "Haz clic en Correr GoF.")
      )
      # Freeman-Tukey (index 2) es más robusto para datos binarios
      tobs <- res@t0[1]
      tsim <- res@t.star[, 1]
      pval <- mean(tsim >= tobs)
      chat <- round(tobs / mean(tsim), 3)
      col  <- if (chat > 3) colores$peligro else
              if (chat > 2) colores$acento  else colores$exito
      div(
        div(class = "text-center py-2",
            h3(style = paste0("color:", col, "; font-weight:700;"), chat),
            p(class = "text-muted mb-0", strong("\u0109 (c-hat)")),
            p(class = "small text-muted",
              if (chat <= 1.5) "\u2714 Buen ajuste"
              else if (chat <= 2) "\u26a0 Sobredispersi\u00f3n leve"
              else if (chat <= 3) "\u26a0 Sobredispersi\u00f3n moderada"
              else "\u274c Sobredispersi\u00f3n severa")
        ),
        div(class = "small text-muted",
            p(strong("p-valor: "), round(pval, 3)),
            p(strong("\u03c7\u00b2 obs: "), round(tobs, 2)),
            p(strong("Simulaciones: "), length(tsim)))
      )
    })

    # ── Predicciones ─────────────────────────────────────

    output$sel_especie_pred <- renderUI({
      d <- datos_activos(); req(d)
      selectInput(ns("especie_pred"), "Especie:",
                  choices  = c("Todas" = "todas", names(d$ylist)),
                  selected = "todas")
    })

    observeEvent(input$predecir, {
      fm <- modelo_fit()
      if (is.null(fm)) {
        showNotification("Ajusta el modelo primero.", type = "warning"); return()
      }
      showNotification(paste0("Calculando predicciones (", input$tipo_pred, ")\u2026"),
                       duration = 2)
    })

    output$plot_pred_especies <- renderPlot({
      fm <- modelo_fit(); req(fm)
      input$predecir
      tryCatch({
        preds  <- unmarked::predict(fm, type = input$tipo_pred)
        psi_sp <- sapply(preds, function(x) mean(x$Predicted, na.rm = TRUE))
        se_sp  <- sapply(preds, function(x) mean(x$SE, na.rm = TRUE))
        d  <- datos_activos()
        df <- data.frame(
          especie = names(d$ylist),
          psi     = psi_sp,
          lo      = pmax(0, psi_sp - 1.96 * se_sp),
          hi      = pmin(1, psi_sp + 1.96 * se_sp)
        )
        df <- df[order(df$psi), ]
        df$especie <- factor(df$especie, levels = df$especie)
        ylab <- if (input$tipo_pred == "state")
          "Ocupaci\u00f3n media estimada (\u03c8) \u00b1 IC 95%"
        else
          "Detecci\u00f3n media estimada (p) \u00b1 IC 95%"
        ggplot2::ggplot(df, ggplot2::aes(x = psi, y = especie)) +
          ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed",
                              color = colores$texto, alpha = 0.4) +
          ggplot2::geom_errorbarh(
            ggplot2::aes(xmin = lo, xmax = hi),
            height = 0.3, color = colores$secundario) +
          ggplot2::geom_point(color = colores$primario, size = 2.5) +
          ggplot2::scale_x_continuous(limits = c(0, 1)) +
          ggplot2::labs(x = ylab, y = NULL) +
          ggplot2::theme_minimal(base_size = 10) +
          ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
      }, error = function(e) NULL)
    })

    output$tabla_predicciones <- DT::renderDT({
      fm <- modelo_fit(); req(fm)
      input$predecir
      tryCatch({
        preds <- unmarked::predict(fm, type = input$tipo_pred)
        d     <- datos_activos()
        df_list <- lapply(seq_along(preds), function(i) {
          p <- preds[[i]]
          data.frame(
            Especie   = names(d$ylist)[i],
            Media_psi = round(mean(p$Predicted, na.rm = TRUE), 3),
            SE        = round(mean(p$SE, na.rm = TRUE), 3),
            IC_inf    = round(mean(p$lower, na.rm = TRUE), 3),
            IC_sup    = round(mean(p$upper, na.rm = TRUE), 3)
          )
        })
        df <- do.call(rbind, df_list)
        df <- df[order(df$Media_psi, decreasing = TRUE), ]
        DT::datatable(df, rownames = FALSE,
                      options = list(pageLength = 10, scrollX = TRUE),
                      class = "table-sm table-striped")
      }, error = function(e) NULL)
    })

    output$riqueza_estimada <- renderUI({
      fm <- modelo_fit(); req(fm)
      tryCatch({
        post    <- unmarked::richness(fm, posterior = TRUE)
        samples <- drop(post@samples)  # matrix: sitios x simulaciones
        riq_obs <- sum(sapply(datos_activos()$ylist,
                              function(y) any(y == 1, na.rm = TRUE)))
        # Media y IC 95% por sitio, luego promedio entre sitios
        est_por_sitio <- apply(samples, 1, mean)
        lo_por_sitio  <- apply(samples, 1, quantile, 0.025)
        hi_por_sitio  <- apply(samples, 1, quantile, 0.975)
        riq_est <- round(mean(est_por_sitio), 1)
        riq_lo  <- round(mean(lo_por_sitio),  1)
        riq_hi  <- round(mean(hi_por_sitio),  1)
        div(
          div(class = "alert alert-success small py-2 px-3 mb-2",
              bs_icon("check-circle-fill", class = "me-1"),
              strong("Especies detectadas: "), riq_obs),
          div(class = "alert alert-info small py-2 px-3 mb-2",
              bs_icon("grid-3x3-gap", class = "me-1"),
              strong("Riqueza local media: "), riq_est,
              tags$br(),
              "IC 95%: ", riq_lo, " \u2013 ", riq_hi,
              tags$br(),
              "Rango entre sitios: ",
              round(min(est_por_sitio), 1), " \u2013 ",
              round(max(est_por_sitio), 1)),
          div(class = "alert alert-warning small py-2 px-3 mb-0",
              bs_icon("info-circle", class = "me-1"),
              "N\u00famero esperado de especies ocupando un sitio t\u00edpico. ",
              "No es la riqueza total del pool regional.")
        )
      }, error = function(e) NULL)
    })

    # ── Código R ─────────────────────────────────────────

    codigo_generado <- reactive({
      req(modelo_fit())
      # Excluir "1" si hay otras covariables seleccionadas
      ocu_sel  <- input$formula_ocu[input$formula_ocu != "1"]
      ocu_form <- if (is.null(ocu_sel) || length(ocu_sel) == 0) "1" else paste(ocu_sel, collapse = " + ")
      det_sel  <- input$formula_det[input$formula_det != "1"]
      det_form <- if (is.null(det_sel) || length(det_sel) == 0) "1" else paste(det_sel, collapse = " + ")

      paste0(
        "# ============================================\n",
        "# StatOccu \u00b7 StatSuite\n",
        "# Modelo de ocupaci\u00f3n de comunidades (occuComm)\n",
        "# ============================================\n\n",
        "library(unmarked)\n",
        "library(tidyverse)\n\n",
        "# 1. Organizar datos\n",
        "# ylist: lista de matrices de detecci\u00f3n (sitios \u00d7 ocasiones) por especie\n",
        "# sitecov: data.frame con covariables de sitio\n",
        "# spcov: lista con rasgos funcionales por especie\n\n",
        "umf <- unmarkedFrameOccuComm(\n",
        "  y           = ylist,\n",
        "  siteCovs    = sitecov,\n",
        "  speciesCovs = spcov\n",
        ")\n",
        "summary(umf)\n\n",
        "# 2. Ajustar modelo\n",
        "fm <- occuComm(\n",
        "  formula = ~", det_form, " ~", ocu_form, ",\n",
        "  data    = umf\n",
        ")\n",
        "summary(fm)\n\n",
        "# 3. Predicciones por especie\n",
        "preds <- predict(fm, type = 'state')\n",
        "psi_sp <- sapply(preds, function(x) mean(x$Predicted, na.rm = TRUE))\n",
        "print(round(sort(psi_sp, decreasing = TRUE), 3))\n\n",
        "# 4. Riqueza estimada\n",
        "riqueza_estimada <- sum(psi_sp)\n",
        "cat('Riqueza estimada:', round(riqueza_estimada, 1), '\n')\n\n",
        "# 5. Caterpillar plot\n",
        "se_sp <- sapply(preds, function(x) mean(x$SE, na.rm = TRUE))\n",
        "df <- data.frame(especie=names(psi_sp), psi=psi_sp, se=se_sp)\n",
        "df <- df[order(df$psi), ]\n",
        "df$especie <- factor(df$especie, levels = df$especie)\n",
        "ggplot(df, aes(x=psi, y=especie)) +\n",
        "  geom_errorbarh(aes(xmin=psi-1.96*se, xmax=psi+1.96*se), height=0.3) +\n",
        "  geom_point(size=2.5) +\n",
        "  scale_x_continuous(limits=c(0,1)) +\n",
        "  labs(x='Ocupaci\u00f3n estimada', y=NULL) +\n",
        "  theme_minimal()\n"
      )
    })

    output$codigo_r <- renderText({ codigo_generado() })

    output$descargar_codigo <- downloadHandler(
      filename = function() paste0("occu_community_", format(Sys.Date(), "%Y%m%d"), ".R"),
      content  = function(file) writeLines(codigo_generado(), file)
    )

  }) # /moduleServer
} # /mod_occu_community_server
