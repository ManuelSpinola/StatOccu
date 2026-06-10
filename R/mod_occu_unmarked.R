# ============================================================
# mod_ocupacion.R — Modelos de ocupación de sitios
# StatOccu · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Familia: modelos de ocupación de una sola especie (MacKenzie et al. 2002)
# Datos: Ejemplo simulado de mamíferos con cámaras trampa, o CSV/XLSX propio
# Ecosistema: unmarked + tidyverse + ggplot2
# ============================================================

# ── UI ────────────────────────────────────────────────────
mod_occu_unmarked_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    div(
      class = "py-3 px-2",
      h4(
        bs_icon("camera", class = "me-2"),
        "Modelos de ocupación de sitios",
        style = paste0("color:", colores$primario, "; font-weight:700;")
      ),
      p(
        class = "text-muted mb-0",
        "Este tipo de modelos jerárquicos estiman la probabilidad de que una especie ",
        tags$strong("ocupe"), " un sitio (ψ) y la probabilidad de ",
        tags$strong("detectarla"), " dado que está presente (p), ",
        "corrigiendo el sesgo causado por la detectabilidad imperfecta ",
        "(MacKenzie et al. 2002)."
      )
    ),
    
    navset_card_tab(
      
      # ════════════════════════════════════════════════
      # PESTAÑA 1: ¿Qué es?
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("book", class = "me-1"), "¿Qué es?"),
        card_body(
          
          # ── Fila 1 ────────────────────────────────────
          layout_columns(
            col_widths = c(6, 6),
            
            # Card 1: El problema
            card(
              card_header(bs_icon("question-circle", class = "me-1"),
                          "El problema de la detectabilidad imperfecta"),
              card_body(
                p(class = "small text-muted mb-3",
                  "Cuando monitoreamos una especie, la ",
                  tags$strong("ausencia observada"), " no implica ausencia real: ",
                  "el animal puede estar presente pero no haber sido detectado. ",
                  "Ignorar esto ", tags$strong("subestima la ocupación real"),
                  " y sesga todas las inferencias."
                ),
                # Esquema visual: lógica del modelo de ocupación
                div(
                  # Fila 1: sitio ocupado con dos ramas
                  div(
                    style = "display:flex; gap:8px; align-items:center; margin-bottom:6px;",
                    div(
                      style = "background:#E8F4FB; border-radius:8px; padding:8px 12px;
                               text-align:center; min-width:110px;",
                      bs_icon("geo-alt-fill", style = "color:#1170AA; font-size:1.2rem;"),
                      tags$br(),
                      tags$b(class = "small", style = "color:#1170AA", "Sitio ocupado"),
                      p(class = "small text-muted mb-0", "z = 1")
                    ),
                    div(style = "color:#A3ACB9; font-size:1.1rem;", "→"),
                    div(
                      style = "display:flex; flex-direction:column; gap:6px; flex:1;",
                      div(
                        style = "background:#FFF3E0; border-radius:8px; padding:6px 12px;
                                 display:flex; align-items:center; gap:8px;",
                        bs_icon("binoculars-fill", style = "color:#FC7D0B; font-size:1.1rem;"),
                        div(
                          tags$b(class = "small", style = "color:#FC7D0B", "Detectado"),
                          p(class = "small text-muted mb-0", "y = 1  (p > 0)")
                        )
                      ),
                      div(
                        style = "background:#EEF3FA; border-radius:8px; padding:6px 12px;
                                 display:flex; align-items:center; gap:8px;
                                 border: 1px solid #C8D9EC;",
                        bs_icon("eye-slash", style = "color:#5FA2CE; font-size:1.1rem;"),
                        div(
                          tags$b(class = "small", style = "color:#5FA2CE", "No detectado"),
                          p(class = "small text-muted mb-0", "y = 0  (1 − p)")
                        )
                      )
                    )
                  ),
                  # Fila 2: sitio no ocupado
                  div(
                    style = "display:flex; gap:8px; align-items:center;",
                    div(
                      style = "background:#F4F7FB; border-radius:8px; padding:8px 12px;
                               text-align:center; min-width:110px;
                               border: 1px dashed #C8D9EC;",
                      bs_icon("geo-alt", style = "color:#A3ACB9; font-size:1.2rem;"),
                      tags$br(),
                      tags$b(class = "small", style = "color:#A3ACB9", "Sitio vacío"),
                      p(class = "small text-muted mb-0", "z = 0")
                    ),
                    div(style = "color:#A3ACB9; font-size:1.1rem;", "→"),
                    div(
                      style = "background:#F4F7FB; border-radius:8px; padding:6px 12px;
                               display:flex; align-items:center; gap:8px; flex:1;
                               border: 1px dashed #C8D9EC;",
                      bs_icon("x-circle", style = "color:#A3ACB9; font-size:1.1rem;"),
                      div(
                        tags$b(class = "small", style = "color:#A3ACB9",
                               "Siempre no detectado"),
                        p(class = "small text-muted mb-0", "y = 0  (imposible y = 1)")
                      )
                    )
                  )
                )
              )
            ),
            
            # Card 2: Estructura jerárquica
            card(
              card_header(bs_icon("diagram-3", class = "me-1"),
                          "Estructura jerárquica del modelo"),
              card_body(
                p(class = "small text-muted mb-2",
                  "El modelo tiene dos niveles que se estiman simultáneamente:"
                ),
                div(
                  style = paste0("border-left: 4px solid ", "#1170AA",
                                 "; padding: 8px 12px; margin-bottom:10px;",
                                 " background:#E8F4FB; border-radius:0 6px 6px 0;"),
                  tags$b(class = "small", style = paste0("color:", "#1170AA"),
                         bs_icon("geo-alt-fill", class = "me-1"),
                         "Nivel 1 — Proceso de estado (Ocupación)"),
                  p(class = "small mb-1 mt-1",
                    style = "font-family: monospace;", "z[i] ~ Bernoulli(ψ[i])"),
                  p(class = "small text-muted mb-0",
                    "¿Está la especie presente en el sitio i? ψ puede depender de ",
                    "covariables del sitio (elevación, cobertura, hábitat).")
                ),
                div(
                  style = paste0("border-left: 4px solid ", "#FC7D0B",
                                 "; padding: 8px 12px;",
                                 " background:#FFF3E0; border-radius:0 6px 6px 0;"),
                  tags$b(class = "small", style = paste0("color:", "#FC7D0B"),
                         bs_icon("binoculars-fill", class = "me-1"),
                         "Nivel 2 — Proceso de observación (Detección)"),
                  p(class = "small mb-1 mt-1",
                    style = "font-family: monospace;",
                    "y[i,j] | z[i]=1 ~ Bernoulli(p[i,j])"),
                  p(class = "small text-muted mb-0",
                    "Dado que el sitio está ocupado, ¿fue detectada en la ocasión j? ",
                    "p puede depender de covariables de la ocasión (esfuerzo, clima).")
                )
              )
            )
          ),
          
          # ── Fila 2 ────────────────────────────────────
          layout_columns(
            col_widths = c(6, 6),
            class = "mt-3",
            
            # Card 3: Supuestos
            card(
              card_header(bs_icon("shield-check", class = "me-1"), "Supuestos clave"),
              card_body(
                tags$ul(
                  class = "small text-muted mb-0",
                  tags$li(
                    tags$strong("Sitios cerrados:"),
                    " la especie no coloniza ni abandona el sitio entre ocasiones ",
                    "de muestreo. Si hay movimiento entre ocasiones, el supuesto se viola."
                  ),
                  tags$li(
                    tags$strong("Independencia condicional:"),
                    " las detecciones en cada ocasión son independientes dado el estado del sitio."
                  ),
                  tags$li(
                    tags$strong("Sin falsos positivos:"),
                    " si se registra una detección, la especie realmente estaba presente."
                  ),
                  tags$li(
                    tags$strong("≥ 2 ocasiones de muestreo"),
                    " por sitio para poder estimar p."
                  )
                )
              )
            ),
            
            # Card 4: ¿Cuándo usar?
            card(
              card_header(bs_icon("camera", class = "me-1"),
                          "¿Cuándo usar modelos de ocupación?"),
              card_body(class = "p-0",
                        tags$table(
                          class = "table table-sm table-bordered small mb-0",
                          style = "background: #ffffff;",
                          tags$thead(
                            style = paste0("background:", "#1170AA", "; color: #ffffff;"),
                            tags$tr(
                              tags$th("Tipo de dato"),
                              tags$th("Organismo"),
                              tags$th("Covariables ψ"),
                              tags$th("Covariables p")
                            )
                          ),
                          tags$tbody(
                            tags$tr(
                              tags$td("Cámaras trampa"),
                              tags$td("Mamíferos medianos y grandes"),
                              tags$td("Elevación, cobertura, perturbación"),
                              tags$td("Días-trampa, lluvias")
                            ),
                            tags$tr(style = paste0("background:", "#F4F7FB"),
                                    tags$td("Puntos de conteo"),
                                    tags$td("Aves, anfibios"),
                                    tags$td("Estructura vegetal, fragmentación"),
                                    tags$td("Viento, nubosidad, observador")
                            ),
                            tags$tr(
                              tags$td("Grabadoras acústicas"),
                              tags$td("Murciélagos, aves"),
                              tags$td("Altitud, cobertura"),
                              tags$td("Ruido de fondo, temperatura")
                            )
                          )
                        )
              )
            )
          )
        )
      ), # /PESTAÑA 1
      
      # ════════════════════════════════════════════════
      # PESTAÑA 2: Fundamentos
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("lightbulb", class = "me-1"), "Fundamentos"),
        card_body(
          
          # ── Fila 1 ────────────────────────────────────
          layout_columns(
            col_widths = c(6, 6),
            
            # Card 1: Verosimilitud del historial
            card(
              card_header(bs_icon("calculator", class = "me-1"),
                          "Verosimilitud del historial de detecciones"),
              card_body(
                p(class = "small text-muted mb-3",
                  "El modelo calcula la probabilidad del historial de detecciones ",
                  "considerando que no sabemos con certeza si el sitio estaba ocupado o no:"
                ),
                div(
                  style = paste0("border-left: 4px solid ", "#1170AA",
                                 "; padding: 8px 12px; margin-bottom:10px;",
                                 " background:#E8F4FB; border-radius:0 6px 6px 0;"),
                  tags$b(class = "small", style = paste0("color:", "#1170AA"),
                         bs_icon("check-circle", class = "me-1"),
                         "Historia con detecciones: 1,0,1,0,0"),
                  p(class = "small mb-0 mt-1",
                    style = "font-family: monospace;",
                    "L = ψ · p²·(1−p)³")
                ),
                div(
                  style = paste0("border-left: 4px solid ", "#5FA2CE",
                                 "; padding: 8px 12px;",
                                 " background:#F4F7FB; border-radius:0 6px 6px 0;"),
                  tags$b(class = "small", style = paste0("color:", "#5FA2CE"),
                         bs_icon("x-circle", class = "me-1"),
                         "Historia sin detecciones: 0,0,0,0,0"),
                  p(class = "small mb-0 mt-1",
                    style = "font-family: monospace;",
                    "L = ψ·(1−p)⁵ + (1−ψ)"),
                  p(class = "small text-muted mb-0 mt-1",
                    "La especie pudo estar presente pero no detectada, ",
                    "o simplemente ausente.")
                )
              )
            ),
            
            # Card 2: Sesgo naïve
            card(
              card_header(bs_icon("exclamation-triangle-fill", class = "me-1"),
                          "El sesgo de la ocupación naïve"),
              card_body(
                p(class = "small text-muted mb-3",
                  "Si ignoramos la detectabilidad y calculamos simplemente la ",
                  "proporción de sitios con al menos una detección, obtenemos ",
                  "una estimación ", tags$strong("siempre sesgada hacia abajo"), "."
                ),
                div(
                  style = "display:flex; gap:12px; align-items:center; margin-bottom:12px;",
                  div(
                    style = paste0("flex:1; background:#fff0f2; border-radius:8px;",
                                   " padding:10px; text-align:center;"),
                    tags$b(class = "small", style = "color:#C85200", "Ocupación naïve"),
                    p(class = "small mb-0",
                      style = "font-family:monospace; font-size:1.1rem;",
                      "sitios det. / total"),
                    p(class = "small text-muted mb-0", "Subestima ψ real")
                  ),
                  div(style = "font-size:1.4rem; color:#A3ACB9;", "<"),
                  div(
                    style = paste0("flex:1; background:#E8F4FB; border-radius:8px;",
                                   " padding:10px; text-align:center;"),
                    tags$b(class = "small", style = "color:#1170AA", "ψ estimada"),
                    p(class = "small mb-0",
                      style = "font-family:monospace; font-size:1.1rem;",
                      "modelo de ocupación"),
                    p(class = "small text-muted mb-0", "Corrige por p")
                  )
                ),
                div(
                  class = "alert mb-0",
                  style = paste0("background:#FFF3E0; border-left: 3px solid #FC7D0B;",
                                 " font-size:0.82rem; padding: 0.5rem 0.8rem;"),
                  bs_icon("exclamation-triangle-fill", class = "me-1",
                          style = "color:#FC7D0B;"),
                  tags$strong("Regla de oro:"), " cuanto menor es p, mayor es el sesgo naïve. ",
                  "Con p = 0.3 y ψ real = 0.8, la estimación naïve puede ser tan baja como 0.4."
                )
              )
            )
          ),
          
          # ── Fila 2 ────────────────────────────────────
          layout_columns(
            col_widths = c(6, 6),
            class = "mt-3",
            
            # Card 3: Covariables con logit
            card(
              card_header(bs_icon("graph-up", class = "me-1"),
                          "Incorporar covariables con la función logit"),
              card_body(
                p(class = "small text-muted mb-2",
                  "Para mantener ψ y p entre 0 y 1 se usa la transformación logit, ",
                  "igual que en la regresión logística:"
                ),
                div(
                  style = paste0("border-left: 4px solid ", "#1170AA",
                                 "; padding: 8px 12px; margin-bottom:8px;",
                                 " background:#E8F4FB; border-radius:0 6px 6px 0;"),
                  tags$b(class = "small", style = "color:#1170AA",
                         bs_icon("geo-alt-fill", class = "me-1"), "Ocupación (ψ)"),
                  p(class = "small mb-0 mt-1", style = "font-family: monospace;",
                    "logit(ψᵢ) = β₀ + β₁·bosqueᵢ"),
                  p(class = "small text-muted mb-0",
                    "β₁ > 0 → más bosque = mayor probabilidad de ocupación")
                ),
                div(
                  style = paste0("border-left: 4px solid ", "#FC7D0B",
                                 "; padding: 8px 12px;",
                                 " background:#FFF3E0; border-radius:0 6px 6px 0;"),
                  tags$b(class = "small", style = "color:#FC7D0B",
                         bs_icon("binoculars-fill", class = "me-1"), "Detección (p)"),
                  p(class = "small mb-0 mt-1", style = "font-family: monospace;",
                    "logit(pᵢⱼ) = α₀ + α₁·esfuerzoᵢⱼ"),
                  p(class = "small text-muted mb-0",
                    "α₁ > 0 → más días-trampa = mayor probabilidad de detección")
                )
              )
            ),
            
            # Card 4: Selección de modelos
            card(
              card_header(bs_icon("bar-chart-steps", class = "me-1"),
                          "Selección de modelos (AIC)"),
              card_body(
                p(class = "small text-muted mb-3",
                  "Los parámetros se estiman por ", tags$strong("máxima verosimilitud"),
                  ". Para comparar modelos candidatos con distintas combinaciones ",
                  "de covariables se usa el ", tags$strong("AIC"),
                  " (Criterio de Información de Akaike):"
                ),
                div(
                  style = paste0("border-left: 4px solid #1170AA;",
                                 " padding: 8px 12px; background:#E8F4FB;",
                                 " border-radius:0 6px 6px 0; margin-bottom:8px;"),
                  tags$b(class = "small", style = "color:#1170AA",
                         "Menor AIC = mejor modelo"),
                  p(class = "small text-muted mb-0",
                    "El AIC penaliza la complejidad: un modelo con más parámetros ",
                    "solo es mejor si mejora el ajuste suficientemente.")
                ),
                div(
                  style = paste0("border-left: 4px solid #FC7D0B;",
                                 " padding: 8px 12px; background:#FFF3E0;",
                                 " border-radius:0 6px 6px 0; margin-bottom:8px;"),
                  tags$b(class = "small", style = "color:#FC7D0B", "ΔAIC < 2"),
                  p(class = "small text-muted mb-0",
                    "Modelos con ΔAIC < 2 tienen apoyo similar al mejor modelo ",
                    "ajustado a los datos; entre 2 y 7 el apoyo es considerablemente ",
                    "menor; mayor de 10 indica muy poco respaldo empírico dado los datos.")
                ),
                div(
                  style = paste0("border-left: 4px solid #5FA2CE;",
                                 " padding: 8px 12px; background:#EEF3FA;",
                                 " border-radius:0 6px 6px 0;"),
                  tags$b(class = "small", style = "color:#5FA2CE",
                         "Modelos candidatos típicos"),
                  tags$ul(
                    class = "small text-muted mb-0 mt-1",
                    style = "padding-left: 1.2rem;",
                    tags$li(tags$code("ψ(.) p(.)"),
                            " — ocupación y detección constantes (modelo nulo)"),
                    tags$li(tags$code("ψ(covariable) p(.)"),
                            " — ocupación varía por sitio, detección constante"),
                    tags$li(tags$code("ψ(.) p(covariable)"),
                            " — ocupación constante, detección varía por ocasión"),
                    tags$li(tags$code("ψ(covariable) p(covariable)"),
                            " — ambos procesos dependen de covariables")
                  )
                )
              )
            )
          )
        )
      ), # /PESTAÑA 2
      
      # ════════════════════════════════════════════════
      # PESTAÑA 3: Los datos
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("table", class = "me-1"), "Los datos"),
        card_body(
          navset_tab(
            nav_panel(
              title = tagList(bs_icon("upload", class = "me-1"), "Cargar datos"),
              div(class = "mt-3",
                  
                  uiOutput(ns("desc_dataset_ui")),
                  
                  p(class = "small text-muted",
                    "Selecciona un dataset de ejemplo o carga tu propio archivo CSV/XLSX. ",
                    "Columnas de detección: ", tags$code("y.1, y.2, … y.J"), "."
                  ),
                  radioButtons(
                    ns("fuente_datos"),
                    label = tagList(bs_icon("database", class = "me-1"), "Fuente de datos:"),
                    choices = c(
                      "Mamíferos — cámaras trampa (simulado)"  = "camaras",
                      "Ranas — transectos (simulado)" = "ranas",
                      "Cargar mis propios datos (CSV / XLSX)"  = "propio"
                    ),
                    selected = "camaras"
                  ),
                  conditionalPanel(
                    condition = paste0("input['", ns("fuente_datos"), "'] == 'propio'"),
                    fileInput(
                      ns("archivo_usuario"),
                      label    = tagList(bs_icon("file-earmark-arrow-up", class = "me-1"),
                                         "Seleccionar archivo:"),
                      accept   = c(".csv", ".xlsx"),
                      buttonLabel = "Examinar…",
                      placeholder = "Sin archivo seleccionado"
                    ),
                    div(
                      class = "alert alert-warning small py-2 px-3 mb-3",
                      bs_icon("exclamation-triangle", class = "me-1"),
                      tags$strong("Formato requerido:"),
                      tags$ul(
                        class = "mb-0 mt-1",
                        tags$li("Filas = sitios; columnas de detección: ",
                                tags$code("y.1, y.2, … y.J"), " (valores 0/1/NA)"),
                        tags$li("Covariables de sitio: columnas adicionales con nombre libre ",
                                "(ej. ", tags$code("bosque"), ", ", tags$code("elev"), ")"),
                        tags$li("Covariables de observación: una columna por ocasión, ",
                                "prefijo libre más sufijo numérico — ej. ",
                                tags$code("esfuerzo.1, esfuerzo.2, … esfuerzo.J"),
                                ". El nombre base (", tags$code("esfuerzo"),
                                ") se usa en la fórmula de detección.")
                      )
                    )
                  ),
                  uiOutput(ns("sel_cov_obs"))
              )
            ),
            nav_panel(
              title = tagList(bs_icon("eye", class = "me-1"), "Vista previa"),
              div(class = "mt-3",
                  uiOutput(ns("resumen_datos_ui")),
                  DTOutput(ns("tabla_preview"))
              )
            ),
            nav_panel(
              title = tagList(bs_icon("grid", class = "me-1"), "Historial"),
              div(class = "mt-3",
                  p(class = "small text-muted",
                    "1 = detectada, 0 = no detectada, NA = sin esfuerzo."),
                  DTOutput(ns("tabla_historial")),
                  div(class = "mt-3",
                      layout_columns(
                        col_widths = c(4, 4, 4),
                        uiOutput(ns("vbox_sitios")),
                        uiOutput(ns("vbox_ocasiones")),
                        uiOutput(ns("vbox_ocu_naive"))
                      )
                  )
              )
            )
          )
        )
      ), # /PESTAÑA 3
      
      # ════════════════════════════════════════════════
      
      # ════════════════════════════════════════════════
      # PESTAÑA 4: Explorar
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("zoom-in", class = "me-1"), "Explorar"),
        card_body(
          p(class = "small text-muted mb-3",
            "Visualiza las relaciones entre las covariables y las ",
            "detecciones antes de ajustar el modelo. Ayuda a identificar ",
            "predictores relevantes para los submodelos de ocupación y detección."
          ),
          layout_columns(
            col_widths = c(4, 8),
            fill = FALSE,
            card(
              card_header(bs_icon("sliders", class = "me-1"), "Controles"),
              card_body(
                style = "overflow: visible; height: auto;",
                uiOutput(ns("expl_sel_cov")),
                selectInput(
                  ns("expl_submodelo"),
                  label = "Submodelo:",
                  choices = c(
                    "Ocupación (ψ) — covariables de sitio" = "state",
                    "Detección (p) — covariables de ocasión" = "det"
                  )
                ),
                tags$hr(),
                uiOutput(ns("expl_cards_resumen"))
              )
            ),
            div(
              card(
                class = "mb-3",
                card_header(
                  bs_icon("graph-up-arrow", class = "me-1"),
                  "Detección naive vs. covariable",
                  span(class = "text-muted small ms-2",
                       "— proporción de sitios con ≥1 detección")
                ),
                card_body(
                  plotOutput(ns("expl_plot_cov"), height = "280px")
                )
              ),
              card(
                class = "mb-0",
                card_header(
                  bs_icon("grid-3x3", class = "me-1"),
                  "Correlación entre covariables",
                  span(class = "text-muted small ms-2",
                       "— detectar multicolinealidad")
                ),
                card_body(
                  plotOutput(ns("expl_plot_corr"), height = "240px")
                )
              )
            )
          )
        )
      ), # /PESTAÑA 4
      
      # PESTAÑA 5: Ajustar modelo
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("sliders", class = "me-1"), "Ajustar modelo"),
        card_body(
          p(class = "small text-muted mb-3",
            "Define los submodelos de ocupación y detección. ",
            "Deja vacío para usar solo el intercepto (~1)."
          ),
          layout_columns(
            col_widths = c(6, 6),
            card(
              style = paste0("border-left: 4px solid ", colores$primario, ";"),
              card_header(
                bs_icon("geo-alt-fill", class = "me-1"), "Submodelo de ocupación (ψ)"
              ),
              card_body(
                p(class = "small text-muted mb-2",
                  "Covariables de sitio que explican si la especie ocupa cada sitio."),
                uiOutput(ns("cov_ocu_ui")),
                div(class = "mt-2",
                    tags$b(class = "small", "Fórmula:"),
                    verbatimTextOutput(ns("formula_ocu_preview"))
                )
              )
            ),
            card(
              style = paste0("border-left: 4px solid ", colores$acento, ";"),
              card_header(
                bs_icon("binoculars-fill", class = "me-1"), "Submodelo de detección (p)"
              ),
              card_body(
                p(class = "small text-muted mb-2",
                  "Covariables que influyen en la probabilidad de detección."),
                uiOutput(ns("cov_det_ui")),
                div(class = "mt-2",
                    tags$b(class = "small", "Fórmula:"),
                    verbatimTextOutput(ns("formula_det_preview"))
                )
              )
            )
          ),
          div(class = "mt-3",
              actionButton(
                ns("ajustar_modelo"),
                label = tagList(bs_icon("play-fill", class = "me-1"), "Ajustar modelo"),
                class = "btn btn-primary me-2"
              )
          ),
          div(class = "alert alert-info small py-2 px-3 mt-2",
              bs_icon("info-circle", class = "me-1"),
              "Al ajustar el modelo, las covariables numéricas se estandarizan ",
              "automáticamente en segundo plano para mejorar la convergencia y ",
              "permitir comparar el peso relativo de cada variable. ",
              strong("Los gráficos, predicciones y ψ por sitio se muestran siempre en la escala original.")
          ),
          tags$hr(),
          p(class = "small fw-bold text-muted mb-1",
            bs_icon("floppy", class = "me-1"),
            "Guardar para comparar"),
          p(class = "small text-muted mb-2",
            "Dale un nombre al modelo ajustado y guárdalo. ",
            "Cambia las covariables, reajusta y guarda otro ",
            "para comparar en la pestaña ",
            strong("Comparar modelos"), "."),
          textInput(
            ns("nombre_modelo_guardar"),
            label       = NULL,
            placeholder = "Ej: nulo, habitat, habitat+distancia…"
          ),
          actionButton(
            ns("guardar_modelo"),
            label = tagList(bs_icon("bookmark-plus", class = "me-1"),
                            "Guardar modelo"),
            class = "btn btn-outline-primary btn-sm w-100"
          ),
          div(class = "mt-3", uiOutput(ns("estado_ajuste_ui")))
        )
      ), # /PESTAÑA 4
      
      # ════════════════════════════════════════════════
      # PESTAÑA 6: Parámetros
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("list-ol", class = "me-1"), "Parámetros"),
        div(
          class = "p-3",
          layout_columns(
            col_widths = c(6, 6),
            uiOutput(ns("vbox_psi")),
            uiOutput(ns("vbox_p"))
          ),
          div(class = "mt-3",
              checkboxInput(ns("mostrar_prob"),
                            "Mostrar interceptos en escala probabilidad", value = FALSE)
          ),
          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "Tabla de coeficientes"),
          DTOutput(ns("tabla_coef")),
          h5(style = paste0("color:", colores$primario, "; font-weight:700; margin-top:1rem;"),
             "Intervalos de confianza (escala logit)"),
          plotOutput(ns("plot_forest"), height = "300px"),
          tags$hr(),
          layout_columns(
            col_widths = c(6, 6),
            fill = FALSE,
            card(
              card_header(
                bs_icon("exclamation-triangle", class = "me-1"),
                "Inflación de varianza (VIF)",
                span(class = "text-muted small ms-2", "— multicolinealidad")
              ),
              card_body(
                style = "overflow: visible; height: auto;",
                p(class = "small text-muted mb-2",
                  "VIF > 5 indica multicolinealidad moderada; ",
                  "VIF > 10 es problemático."),
                uiOutput(ns("tabla_vif"))
              )
            ),
            card(
              card_header(
                bs_icon("bar-chart-steps", class = "me-1"),
                "Importancia de covariables",
                span(class = "text-muted small ms-2",
                     "— coeficientes estandarizados")
              ),
              card_body(
                style = "height: auto;",
                p(class = "small text-muted mb-2",
                  strong("Azul"), " = efecto positivo sobre ψ o p · ",
                  strong("rojo"), " = efecto negativo. ",
                  "Barras transparentes = no significativo (p ≥ 0.05)."
                ),
                plotOutput(ns("plot_importancia"), height = "280px")
              )
            )
          )
        )
      ), # /PESTAÑA 6
      
      # ════════════════════════════════════════════════
      # PESTAÑA 7: Gráficos
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("graph-up-arrow", class = "me-1"), "Gráficos"),
        card_body(
          p(class = "small text-muted mb-3",
            "Curva de respuesta de ψ o p sobre el rango de la covariable seleccionada, ",
            "manteniendo las demás en su valor promedio."
          ),
          layout_columns(
            col_widths = c(4, 4, 4),
            selectInput(ns("efecto_submodelo"), "Submodelo:",
                        choices = c("Ocupación (ψ)" = "state", "Detección (p)" = "det")),
            uiOutput(ns("sel_efecto_cov")),
            numericInput(ns("efecto_ic"), "Nivel IC (%):", 95, 80, 99, 1)
          ),
          plotOutput(ns("plot_efecto"), height = "380px"),
          div(
            class = "alert alert-info small py-2 px-3 mt-3",
            bs_icon("info-circle-fill", class = "me-1"),
            "Bandas con método delta. Eje Y en escala de probabilidad (0–1)."
          )
        )
      ), # /PESTAÑA 6
      
      # ════════════════════════════════════════════════
      # PESTAÑA 8: ψ por sitio
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("pin-map", class = "me-1"), "\u03c8 por sitio"),
        div(
          class = "p-3",
          navset_tab(
            # ── Sub-tab 1: ψ por covariables ──────────────
            nav_panel(
              title = tagList(bs_icon("sliders", class = "me-1"),
                              "\u03c8 por covariables"),
              br(),
              layout_columns(
                col_widths = c(4, 8),
                card(
                  class = "border-0",
                  style = paste0("background:", colores$fondo, ";"),
                  card_body(
                    sliderInput(ns("umbral_ocu"), "Umbral de ocupaci\u00f3n:",
                                min = 0, max = 1, value = 0.5, step = 0.05),
                    p(class = "small text-muted",
                      "Sitios con \u03c8 estimado \u2265 umbral = 'probablemente ocupados'."),
                    uiOutput(ns("vbox_sitios_ocu")),
                    tags$br(),
                    downloadButton(ns("descarga_psi_sitio"), "Descargar tabla",
                                   class = "btn btn-outline-secondary btn-sm w-100")
                  )
                ),
                plotOutput(ns("plot_psi_sitio"), height = "420px")
              ),
              div(class = "mt-3", DTOutput(ns("tabla_psi_sitio")))
            ),
            
            # ── Sub-tab 2: ψ posterior (ranef + bup) ──────
            nav_panel(
              title = tagList(bs_icon("binoculars-fill", class = "me-1"),
                              "\u03c8 posterior (ranef + bup)"),
              br(),
              p(class = "small text-muted mb-3",
                strong("predict()"), " estima \u03c8 bas\u00e1ndose solo en las covariables del sitio. ",
                strong("bup(ranef())"), " actualiza esa estimaci\u00f3n usando las ",
                strong("detecciones reales"), " observadas. La diferencia es m\u00e1s ",
                "pronunciada en sitios con historial ", code("0000"),
                " (muchas visitas sin detecci\u00f3n) cuando la detectabilidad p es alta."
              ),
              layout_columns(
                col_widths = c(6, 6),
                fill = FALSE,
                card(
                  card_header(
                    bs_icon("graph-up-arrow", class = "me-1"),
                    "\u03c8 covariables vs. \u03c8 posterior",
                    span(class = "text-muted small ms-2",
                         "— puntos bajo la diagonal = evidencia reduce la estimaci\u00f3n")
                  ),
                  card_body(
                    style = "height: auto;",
                    plotOutput(ns("plot_ranef_scatter"), height = "360px")
                  )
                ),
                card(
                  card_header(
                    bs_icon("bar-chart-fill", class = "me-1"),
                    "Diferencia \u03c8post \u2212 \u03c8cov por sitio"
                  ),
                  card_body(
                    style = "height: auto;",
                    plotOutput(ns("plot_ranef_diff"), height = "360px")
                  )
                )
              ),
              div(class = "mt-3",
                  card(
                    card_header(bs_icon("table", class = "me-1"),
                                "Tabla comparativa"),
                    card_body(
                      style = "overflow: visible; height: auto;",
                      DTOutput(ns("tabla_ranef"))
                    )
                  )
              )
            )
          )
        )
      ), # /PESTAÑA 8
      
      # ════════════════════════════════════════════════
      # PESTAÑA 9: Performance
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("speedometer2", class = "me-1"), "Performance"),
        div(
          class = "p-3",
          p(class = "small text-muted mb-3",
            "Evaluaci\u00f3n del poder predictivo del modelo mediante ",
            strong("validaci\u00f3n cruzada"), " (", code("crossVal()"), " de unmarked) y ",
            "estimaci\u00f3n del factor de sobredispersi\u00f3n ",
            strong("\u0109 (c-hat)"), " a partir del bootstrap de bondad de ajuste."
          ),
          layout_columns(
            col_widths = c(6, 6),
            fill = FALSE,
            
            card(
              card_header(
                bs_icon("calculator", class = "me-1"),
                "Factor de sobredispersi\u00f3n (\u0109 / c-hat)",
                span(class = "text-muted small ms-2",
                     "— requiere haber corrido GoF en Diagn\u00f3stico")
              ),
              card_body(
                style = "overflow: visible; height: auto;",
                uiOutput(ns("ui_chat")),
                tags$hr(),
                div(class = "alert alert-info small py-2 px-3 mb-0",
                    bs_icon("info-circle", class = "me-1"),
                    strong("\u0109 = \u03c7\u00b2obs / media(\u03c7\u00b2sim)"),
                    br(),
                    "\u0109 \u2248 1 = buen ajuste; \u0109 > 2 = sobredispersi\u00f3n moderada; ",
                    "\u0109 > 3 = sobredispersi\u00f3n severa \u2014 considerar QAICc."
                )
              )
            ),
            
            card(
              card_header(
                bs_icon("arrow-repeat", class = "me-1"),
                "Validaci\u00f3n cruzada",
                span(class = "text-muted small ms-2",
                     "— crossVal() \u00b7 unmarked")
              ),
              card_body(
                style = "overflow: visible; height: auto;",
                p(class = "small text-muted mb-2",
                  "\u00bfQu\u00e9 tan bien predice el modelo datos ",
                  strong("no vistos"), "?"),
                layout_columns(
                  col_widths = c(6, 6),
                  numericInput(ns("cv_folds"), "Folds (k):",
                               value = 5, min = 2, max = 10),
                  div(class = "pt-4",
                      actionButton(ns("correr_cv"), 
                                   label = tagList(bs_icon("arrow-repeat", class = "me-1"), "Correr CV"),
                                   class = "btn-primary w-100"))
                ),
                tags$hr(),
                uiOutput(ns("resultado_cv"))
              )
            )
          )
        )
      ), # /PESTAÑA 9
      
      # ════════════════════════════════════════════════
      # PESTAÑA 10: Comparar modelos
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("bar-chart-steps", class = "me-1"), "Comparar modelos"),
        card_body(
          p(class = "small text-muted mb-3",
            "Guarda modelos desde 'Ajustar modelo' y comp\u00e1ralos por AIC."
          ),
          actionButton(ns("limpiar_modelos"), 
                       label = tagList(bs_icon("trash", class = "me-1"), "Limpiar lista"),
                       class = "btn btn-outline-danger btn-sm mb-2"),
          uiOutput(ns("ui_sin_modelos")),
          DTOutput(ns("tabla_aic")),
          plotOutput(ns("plot_aic"), height = "320px"),
          div(
            class = "alert alert-info small py-2 px-3 mt-3",
            bs_icon("info-circle-fill", class = "me-1"),
            tags$strong("ΔAIC:"),
            " diferencia de AIC con respecto al mejor modelo ajustado a los datos. ",
            "Modelos con ΔAIC < 2 tienen apoyo similar; entre 2 y 7 el apoyo es ",
            "considerablemente menor; mayor de 10 indica muy poco respaldo empírico ",
            "dado los datos.",
            tags$br(), tags$br(),
            bs_icon("lightbulb", class = "me-1"),
            tags$strong("Importante:"),
            " el AIC es siempre relativo a los datos observados y a los modelos ",
            "candidatos considerados. El modelo con menor AIC es el que mejor se ajusta ",
            tags$em("a tus datos"), ", no necesariamente el modelo ",
            "'verdadero'. Si cambias los datos o incluyes nuevos candidatos, ",
            "el modelo ganador puede cambiar."
          )
        )
      ), # /PESTAÑA 8
      
      # ════════════════════════════════════════════════
      # PESTAÑA 11: Diagnóstico
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("clipboard2-pulse", class = "me-1"), "Diagnóstico"),
        card_body(
          
          # ── Fila 1: explicación + controles + resultado ──
          layout_columns(
            col_widths = c(8, 4),
            
            # Explicación didáctica
            div(
              class = "alert mb-0",
              style = paste0("background:", colores$fondo,
                             "; border-left: 4px solid ", colores$secundario, ";"),
              bs_icon("mortarboard-fill", class = "me-2",
                      style = paste0("color:", colores$secundario)),
              tags$strong("¿Cómo leer este diagnóstico?"),
              tags$br(),
              "El modelo predice cuántos sitios deberían tener cada patrón de detección ",
              "(ej. nunca detectada, una vez, siempre). El gráfico de barras ",
              tags$strong("compara esa predicción"), " (naranja) con lo ",
              tags$strong("observado"), " (azul). Si son similares, el modelo ajusta bien.",
              tags$br(),
              "El histograma simula esa diferencia cientos de veces bajo el modelo — ",
              "la línea roja es tu diferencia real. Si cae en la cola derecha (p < 0.05), ",
              "considera agregar covariables o revisar el supuesto de cierre."
            ),
            
            # Controles y resultado
            card(
              class = "border-0 h-100",
              style = paste0("background:", colores$fondo, ";"),
              card_body(
                numericInput(ns("nsim_gof"), "Simulaciones:",
                             500, 99, 2000, 100),
                actionButton(ns("correr_gof"),
                             label = tagList(bs_icon("play-fill", class = "me-1"),
                                             "Ejecutar GoF"),
                             class = "btn btn-primary w-100 mt-1"),
                div(class = "mt-2", uiOutput(ns("res_gof_ui")))
              )
            )
          ),
          
          # ── Fila 2: los dos gráficos lado a lado ─────────
          layout_columns(
            col_widths = c(6, 6),
            class = "mt-3",
            
            div(
              p(class = "small text-muted mb-1",
                bs_icon("bar-chart", class = "me-1"),
                tags$strong("Distribución bootstrap del χ²")),
              plotOutput(ns("plot_gof"), height = "280px"),
              p(class = "small text-muted mt-1",
                "Barras grises = χ² esperado por azar. ",
                "Línea roja = χ² de tus datos.")
            ),
            
            div(
              p(class = "small text-muted mb-1",
                bs_icon("bar-chart-steps", class = "me-1"),
                tags$strong("Frecuencias de historiales de detección")),
              plotOutput(ns("plot_histfreq"), height = "280px"),
              p(class = "small text-muted mt-1",
                tags$strong("Azul"), " = observado  |  ",
                tags$strong("Naranja"), " = esperado por el modelo.")
            )
          )
        )
      ), # /PESTAÑA 9
      
      # ════════════════════════════════════════════════
      # PESTAÑA 12: Código R
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("code-slash", class = "me-1"), "Código R"),
        card_body(
          div(
            class = "d-flex align-items-center gap-2 mb-3",
            downloadButton(
              ns("descarga_codigo"),
              label = tagList(bs_icon("download", class = "me-1"), "Descargar script .R"),
              class = "btn btn-outline-secondary btn-sm"
            ),
            p(class = "small text-muted mb-0",
              "El código refleja el último modelo ajustado y es completamente reproducible.")
          ),
          div(class = "codigo-bloque", verbatimTextOutput(ns("codigo_r")))
        )
      ) # /PESTAÑA 10
      
    ) # /navset_card_tab
  ) # /tagList
}

mod_occu_unmarked_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ────────────────────────────────────────────────────
    # DATOS INTERNOS (datasets de ejemplo)
    # ────────────────────────────────────────────────────
    
    # Genera datos simulados de mamíferos con cámaras trampa
    generar_datos_camaras <- function() {
      set.seed(42)
      n_sitios   <- 50
      n_ocasiones <- 5
      
      bosque <- runif(n_sitios, 10, 90)   # cobertura forestal (%)
      elev   <- runif(n_sitios, 200, 1800) # elevación (m)
      
      # Ocupación real
      lp_ocu <- -1.5 + 0.04 * bosque
      psi    <- plogis(lp_ocu)
      z      <- rbinom(n_sitios, 1, psi)
      
      # Esfuerzo de detección (días-trampa por ocasión) — covariable de observación
      esfuerzo <- matrix(
        round(runif(n_sitios * n_ocasiones, 3, 14)),
        nrow = n_sitios
      )
      
      # Detección
      lp_det <- -2.0 + 0.12 * esfuerzo
      p_mat  <- plogis(lp_det)
      y      <- matrix(
        rbinom(n_sitios * n_ocasiones, 1,
               ifelse(z == 1, p_mat, 0)),
        nrow = n_sitios
      )
      colnames(y) <- paste0("y.", 1:n_ocasiones)
      
      # obsCovs: lista nombrada — cada elemento es un data.frame
      # de n_sitios filas x n_ocasiones columnas
      obs_covs <- list(
        esfuerzo = as.data.frame(esfuerzo)
      )
      
      list(
        y         = as.data.frame(y),
        cov_sitio = data.frame(bosque = bosque, elev = elev),
        cov_obs   = obs_covs,
        n_sitios  = n_sitios,
        n_ocas    = n_ocasiones
      )
    }
    
    # Genera datos simulados de rana en transectos
    generar_datos_ranas <- function() {
      set.seed(7)
      n_sitios   <- 40
      n_ocasiones <- 4
      
      humedad <- runif(n_sitios, 20, 100)
      dist_ag <- runif(n_sitios, 0, 500)
      
      lp_ocu <- 0.5 + 0.03 * humedad - 0.005 * dist_ag
      psi    <- plogis(lp_ocu)
      z      <- rbinom(n_sitios, 1, psi)
      
      lluvia <- matrix(
        runif(n_sitios * n_ocasiones, 0, 30),
        nrow = n_sitios
      )
      
      lp_det <- -1.2 + 0.05 * lluvia
      p_mat  <- plogis(lp_det)
      y      <- matrix(
        rbinom(n_sitios * n_ocasiones, 1,
               ifelse(z == 1, p_mat, 0)),
        nrow = n_sitios
      )
      colnames(y) <- paste0("y.", 1:n_ocasiones)
      
      obs_covs <- list(
        lluvia = as.data.frame(lluvia)
      )
      
      list(
        y         = as.data.frame(y),
        cov_sitio = data.frame(humedad = humedad, dist_ag = dist_ag),
        cov_obs   = obs_covs,
        n_sitios  = n_sitios,
        n_ocas    = n_ocasiones
      )
    }
    
    # ────────────────────────────────────────────────────
    # REACTIVOS: datos activos
    # ────────────────────────────────────────────────────
    
    datos_crudos <- reactive({
      req(input$fuente_datos)
      switch(input$fuente_datos,
             camaras = generar_datos_camaras(),
             ranas   = generar_datos_ranas(),
             propio  = {
               req(input$archivo_usuario)
               ext <- tools::file_ext(input$archivo_usuario$name)
               df  <- if (ext == "xlsx")
                 readxl::read_excel(input$archivo_usuario$datapath)
               else
                 readr::read_csv(input$archivo_usuario$datapath, show_col_types = FALSE)
               
               # Detectar columnas y.*
               cols_y <- grep("^y\\.", names(df), value = TRUE)
               validate(
                 need(length(cols_y) >= 2,
                      "El archivo debe tener al menos 2 columnas nombradas y.1, y.2, ...")
               )
               cov_cols <- setdiff(names(df), cols_y)
               list(
                 y         = as.data.frame(df[, cols_y]),
                 cov_sitio = if (length(cov_cols) > 0)
                   as.data.frame(df[, cov_cols]) else NULL,
                 cov_obs   = NULL,
                 n_sitios  = nrow(df),
                 n_ocas    = length(cols_y)
               )
             }
      )
    })
    
    # Objeto unmarkedFrame (umf)
    umf <- reactive({
      req(datos_crudos())
      d <- datos_crudos()
      unmarked::unmarkedFrameOccu(
        y        = d$y,
        siteCovs = d$cov_sitio,
        obsCovs  = d$cov_obs   # lista: list(esfuerzo = df, ...) o NULL
      )
    })
    
    # ────────────────────────────────────────────────────
    # UI DINÁMICA: selectores de covariables
    # ────────────────────────────────────────────────────
    
    vars_sitio <- reactive({
      req(datos_crudos())
      names(datos_crudos()$cov_sitio)
    })
    
    vars_obs <- reactive({
      req(datos_crudos())
      d <- datos_crudos()
      if (is.null(d$cov_obs)) character(0)
      else names(d$cov_obs)   # lista nombrada: c("esfuerzo"), c("lluvia"), etc.
    })
    
    output$cov_ocu_ui <- renderUI({
      checkboxGroupInput(
        ns("cov_ocu"),
        label = "Covariables de sitio:",
        choices  = vars_sitio(),
        selected = NULL
      )
    })
    
    output$cov_det_ui <- renderUI({
      opts <- c(vars_obs(), vars_sitio())
      if (length(opts) == 0) opts <- character(0)
      checkboxGroupInput(
        ns("cov_det"),
        label = "Covariables de detección:",
        choices  = opts,
        selected = NULL
      )
    })
    
    output$sel_efecto_cov <- renderUI({
      sub <- input$efecto_submodelo
      opts <- if (sub == "state") vars_sitio() else vars_obs()
      if (length(opts) == 0) opts <- c("(ninguna)" = "")
      selectInput(ns("efecto_cov"), "Covariable:", choices = opts)
    })
    
    # ────────────────────────────────────────────────────
    # PREVIEWS DE FÓRMULAS
    # ────────────────────────────────────────────────────
    
    # Construir fórmulas de un lado (~cov) con env = baseenv() para
    # evitar que Shiny inyecte "y" u otras variables reactivas en el entorno
    formula_ocu <- reactive({
      covs <- input$cov_ocu
      rhs  <- if (is.null(covs) || length(covs) == 0) "1"
      else paste(covs, collapse = " + ")
      as.formula(paste("~", rhs), env = baseenv())
    })
    
    formula_det <- reactive({
      covs <- input$cov_det
      rhs  <- if (is.null(covs) || length(covs) == 0) "1"
      else paste(covs, collapse = " + ")
      as.formula(paste("~", rhs), env = baseenv())
    })
    
    output$formula_ocu_preview <- renderText({
      paste("ψ:", deparse(formula_ocu()))
    })
    
    output$formula_det_preview <- renderText({
      paste("p:", deparse(formula_det()))
    })
    
    # ────────────────────────────────────────────────────
    # AJUSTAR MODELO
    # ────────────────────────────────────────────────────
    
    modelo_actual <- reactiveVal(NULL)
    nombre_modelo_actual <- reactiveVal(NULL)
    
    # ── Modelo estandarizado (para importancia de variables) ──
    modelo_actual_std <- eventReactive(input$ajustar_modelo, {
      req(umf())
      site_covs <- unmarked::siteCovs(umf())
      obs_covs  <- unmarked::obsCovs(umf())
      
      tryCatch({
        # Estandarizar covariables numéricas con scale()
        estandarizar_df <- function(df) {
          if (is.null(df)) return(NULL)
          nums <- sapply(df, is.numeric)
          if (any(nums)) df[, nums] <- scale(df[, nums, drop = FALSE])
          df
        }
        
        site_std <- estandarizar_df(site_covs)
        obs_std  <- estandarizar_df(obs_covs)
        
        umf_std <- unmarked::unmarkedFrameOccu(
          y        = unmarked::getY(umf()),
          siteCovs = site_std,
          obsCovs  = obs_std
        )
        
        det_str <- if (is.null(input$cov_det) || length(input$cov_det) == 0)
          "1" else paste(input$cov_det, collapse = " + ")
        ocu_str <- if (is.null(input$cov_ocu) || length(input$cov_ocu) == 0)
          "1" else paste(input$cov_ocu, collapse = " + ")
        fm_formula <- eval(
          parse(text = paste("~", det_str, "~", ocu_str))[[1]],
          envir = new.env(parent = baseenv())
        )
        unmarked::occu(formula = fm_formula, data = umf_std)
      }, error = function(e) {
        message("Error en modelo_actual_std: ", conditionMessage(e))
        NULL
      })
    }, ignoreNULL = TRUE)
    
    observeEvent(input$ajustar_modelo, {
      req(umf())
      withProgress(message = "Ajustando modelo…", value = 0.5, {
        tryCatch({
          # Construir fórmula ~ det ~ ocu como objeto de 3 elementos
          # usando call() para que unmarked::getDesign pueda hacer
          # formula[[2]] y formula[[3]] sin buscar "y" en ningún entorno
          det_str <- if (is.null(input$cov_det) || length(input$cov_det) == 0)
            "1" else paste(input$cov_det, collapse = " + ")
          ocu_str <- if (is.null(input$cov_ocu) || length(input$cov_ocu) == 0)
            "1" else paste(input$cov_ocu, collapse = " + ")
          # Construir la fórmula ~ det ~ ocu sin as.formula:
          # eval(parse()) en un entorno vacío (sin "y" de Shiny)
          fm_formula <- eval(
            parse(text = paste("~", det_str, "~", ocu_str))[[1]],
            envir = new.env(parent = baseenv())
          )
          fm <- unmarked::occu(
            formula = fm_formula,
            data    = umf()
          )
          modelo_actual(fm)
          nombre <- paste0(
            "ψ(",
            ifelse(length(input$cov_ocu) == 0, ".",
                   paste(input$cov_ocu, collapse = "+")), ") ",
            "p(",
            ifelse(length(input$cov_det) == 0, ".",
                   paste(input$cov_det, collapse = "+")), ")"
          )
          nombre_modelo_actual(nombre)
          incProgress(0.5)
        }, error = function(e) {
          showNotification(
            paste("Error al ajustar el modelo:", conditionMessage(e)),
            type = "error"
          )
        })
      })
    })
    
    output$estado_ajuste_ui <- renderUI({
      fm <- modelo_actual()
      if (is.null(fm)) return(NULL)
      div(
        class = "alert alert-success small py-2 px-3",
        bs_icon("check-circle-fill", class = "me-1"),
        strong("Modelo ajustado: "), nombre_modelo_actual(),
        " — log-verosimilitud: ",
        round(as.numeric(logLik(fm)), 2)
      )
    })
    
    # ────────────────────────────────────────────────────
    # VALOR BOXES: datos
    # ────────────────────────────────────────────────────
    
    output$vbox_sitios <- renderUI({
      req(umf())
      n <- unmarked::numSites(umf())
      div(class="card h-100",
          style=paste0("background:var(--color-background-secondary); border-radius:var(--border-radius-md); padding:1rem; text-align:center;"),
          div(style=paste0("font-size:13px; color:", colores$texto, "; margin-bottom:4px;"),
              bs_icon("geo-alt", class="me-1"), "Sitios"),
          div(style="font-size:24px; font-weight:500;", n)
      )
    })
    
    output$vbox_ocasiones <- renderUI({
      req(umf())
      j <- unmarked::obsNum(umf())
      div(class="card h-100",
          style=paste0("background:var(--color-background-secondary); border-radius:var(--border-radius-md); padding:1rem; text-align:center;"),
          div(style=paste0("font-size:13px; color:", colores$texto, "; margin-bottom:4px;"),
              bs_icon("calendar", class="me-1"), "Ocasiones"),
          div(style="font-size:24px; font-weight:500;", j)
      )
    })
    
    output$vbox_ocu_naive <- renderUI({
      req(datos_crudos())
      y     <- datos_crudos()$y
      naïve <- round(mean(rowSums(y, na.rm = TRUE) > 0) * 100, 1)
      div(class="card h-100",
          style=paste0("background:var(--color-background-secondary); border-radius:var(--border-radius-md); padding:1rem; text-align:center;"),
          div(style=paste0("font-size:13px; color:", colores$texto, "; margin-bottom:4px;"),
              bs_icon("eye", class="me-1"), "Ocupación naïve"),
          div(style="font-size:24px; font-weight:500;", paste0(naïve, "%"))
      )
    })
    
    # ────────────────────────────────────────────────────
    # TABLAS: vista previa e historial
    # ────────────────────────────────────────────────────
    
    # Descripción del dataset activo — aparece en sub-pestaña "Cargar datos"
    output$desc_dataset_ui <- renderUI({
      req(input$fuente_datos)
      desc <- switch(input$fuente_datos,
                     camaras = tagList(
                       tags$strong("Mamíferos — cámaras trampa (simulado)."),
                       " Detecciones/no detecciones de una especie focal en ",
                       tags$strong("50 sitios"), " durante ",
                       tags$strong("5 ocasiones de muestreo"), ". ",
                       "Covariables de sitio: cobertura forestal (%) y elevación (m). ",
                       "Covariable de detección: esfuerzo de muestreo (días-trampa)."
                     ),
                     ranas = tagList(
                       tags$strong("Ranas — transectos (simulado)."),
                       " Detecciones/no detecciones en ",
                       tags$strong("40 sitios"), " durante ",
                       tags$strong("4 ocasiones de muestreo"), ". ",
                       "Covariables de sitio: humedad (%) y distancia a cuerpos de agua (m). ",
                       "Covariable de detección: lluvia acumulada (mm)."
                     ),
                     propio = NULL
      )
      if (!is.null(desc)) {
        div(
          class = "alert alert-info small py-2 px-3 mb-3",
          bs_icon("info-circle-fill", class = "me-1"),
          desc
        )
      }
    })
    
    output$resumen_datos_ui <- renderUI({
      req(datos_crudos())
      d <- datos_crudos()
      div(
        class = "alert alert-info small py-2 px-3 mb-2",
        bs_icon("info-circle-fill", class = "me-1"),
        strong(d$n_sitios), " sitios × ",
        strong(d$n_ocas), " ocasiones. ",
        strong(length(vars_sitio())),
        " covariable(s) de sitio. ",
        if (length(vars_obs()) > 0)
          paste0(length(vars_obs()), " covariable(s) de observación.")
        else
          "Sin covariables de observación."
      )
    })
    
    output$tabla_preview <- renderDT({
      req(datos_crudos())
      d  <- datos_crudos()
      df <- cbind(d$cov_sitio, d$y)
      datatable(
        df,
        options  = list(pageLength = 10, scrollX = TRUE,
                        dom = "tp", language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json")),
        rownames = FALSE,
        class    = "table-sm table-striped"
      )
    })
    
    output$tabla_historial <- renderDT({
      req(datos_crudos())
      datatable(
        datos_crudos()$y,
        options  = list(pageLength = 10, scrollX = TRUE,
                        dom = "tp", language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json")),
        rownames = paste("Sitio", seq_len(datos_crudos()$n_sitios)),
        class    = "table-sm table-condensed"
      ) |>
        DT::formatStyle(
          columns    = names(datos_crudos()$y),
          background = DT::styleEqual(c(0, 1), c("#fff0f2", "#f0f9f5"))
        )
    })
    
    # ────────────────────────────────────────────────────
    # PARÁMETROS
    # ────────────────────────────────────────────────────
    
    output$vbox_psi <- renderUI({
      req(modelo_actual())
      # plogis del intercepto de ocupación = ψ estimada con covariables en 0
      psi_val <- round(plogis(coef(modelo_actual(), type = "state")[1]), 3)
      div(class="card h-100",
          style=paste0("background:var(--color-background-secondary); border-radius:var(--border-radius-md); padding:1rem; text-align:center;"),
          div(style=paste0("font-size:13px; color:", colores$texto, "; margin-bottom:4px;"),
              bs_icon("geo-alt-fill", class="me-1"), "ψ estimada (intercepción)"),
          div(style=paste0("font-size:24px; font-weight:500; color:", colores$primario, ";"),
              psi_val)
      )
    })
    
    output$vbox_p <- renderUI({
      req(modelo_actual())
      p_val <- round(plogis(coef(modelo_actual(), type = "det")[1]), 3)
      div(class="card h-100",
          style=paste0("background:var(--color-background-secondary); border-radius:var(--border-radius-md); padding:1rem; text-align:center;"),
          div(style=paste0("font-size:13px; color:", colores$texto, "; margin-bottom:4px;"),
              bs_icon("binoculars-fill", class="me-1"), "p estimada (intercepción)"),
          div(style=paste0("font-size:24px; font-weight:500; color:", colores$acento, ";"),
              p_val)
      )
    })
    
    output$tabla_coef <- renderDT({
      req(modelo_actual())
      fm <- modelo_actual()
      
      # Extraer coeficientes usando coef() y SE() — compatible con unmarked 1.5.x
      hacer_df_sub <- function(tipo, label) {
        est <- coef(fm, type = tipo)
        se  <- unmarked::SE(fm, type = tipo)
        z   <- est / se
        pv  <- 2 * pnorm(-abs(z))
        data.frame(
          Submodelo = label,
          Parámetro = names(est),
          Estimado  = est,
          EE        = se,
          z         = z,
          `p-valor` = pv,
          check.names = FALSE,
          row.names   = NULL
        )
      }
      
      df <- dplyr::bind_rows(
        hacer_df_sub("state", "Ocupación (ψ)"),
        hacer_df_sub("det",   "Detección (p)")
      )
      
      if (input$mostrar_prob) {
        int_rows <- grepl("^\\(Int", df$Parámetro)
        df$Estimado[int_rows] <- plogis(df$Estimado[int_rows])
        df$Parámetro[int_rows] <- paste0(df$Parámetro[int_rows], " [prob]")
      }
      
      datatable(
        df,
        options  = list(dom = "t", pageLength = 20,
                        language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json")),
        rownames = FALSE,
        class    = "table-sm table-striped"
      ) |>
        DT::formatRound(c("Estimado", "EE", "z", "p-valor"), 4) |>
        DT::formatStyle(
          "p-valor",
          color = DT::styleInterval(c(0.05, 0.1),
                                    c("#C85200", "#B85A0D", colores$texto))
        )
    })
    
    output$plot_forest <- renderPlot({
      req(modelo_actual())
      fm <- modelo_actual()
      
      hacer_df_coef <- function(tipo, label) {
        est <- coef(fm, type = tipo)
        se  <- unmarked::SE(fm, type = tipo)
        data.frame(
          Estimate = est,
          SE       = se,
          par      = names(est),
          tipo     = label,
          lower    = est - 1.96 * se,
          upper    = est + 1.96 * se,
          row.names = NULL
        )
      }
      
      df <- dplyr::bind_rows(
        hacer_df_coef("state", "Ocupación (ψ)"),
        hacer_df_coef("det",   "Detección (p)")
      )
      
      ggplot2::ggplot(df,
                      ggplot2::aes(x = Estimate, y = reorder(par, Estimate),
                                   color = tipo, xmin = lower, xmax = upper)) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                            color = "#A3ACB9") +
        ggplot2::geom_errorbarh(height = 0.25, linewidth = 0.8) +
        ggplot2::geom_point(size = 3) +
        ggplot2::scale_color_manual(
          values = c("Ocupación (ψ)" = colores$primario,
                     "Detección (p)" = colores$acento)
        ) +
        ggplot2::facet_wrap(~tipo, scales = "free_y") +
        ggplot2::labs(x = "Estimado (logit)", y = NULL, color = NULL) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          legend.position = "none",
          panel.grid.minor = ggplot2::element_blank()
        )
    })
    
    # ────────────────────────────────────────────────────
    # EFECTOS DE COVARIABLES
    # ────────────────────────────────────────────────────
    
    output$plot_efecto <- renderPlot({
      req(modelo_actual(), input$efecto_cov, nchar(input$efecto_cov) > 0)
      fm   <- modelo_actual()
      cov  <- input$efecto_cov
      tipo <- input$efecto_submodelo
      nivel_ic <- input$efecto_ic / 100
      z_ic <- qnorm((1 + nivel_ic) / 2)
      
      d <- datos_crudos()
      # Para covariables de observación, colapsar columnas repetidas
      # (e.g. esfuerzo.1…J) al nombre base que usa unmarked
      if (tipo == "state") {
        cov_data <- d$cov_sitio
      } else {
        # cov_obs es lista nombrada: list(esfuerzo = df_50x5, ...)
        # Para predict(), necesitamos una columna por covariable (promedio entre ocasiones)
        cov_data <- as.data.frame(
          lapply(d$cov_obs, function(df) rowMeans(as.data.frame(df), na.rm = TRUE))
        )
      }
      
      validate(need(cov %in% names(cov_data),
                    paste("Covariable", cov, "no encontrada en los datos.")))
      
      rango <- seq(min(cov_data[[cov]], na.rm = TRUE),
                   max(cov_data[[cov]], na.rm = TRUE),
                   length.out = 100)
      
      # Nuevos datos (resto de covariables en su media)
      medias <- lapply(cov_data, function(x) rep(mean(x, na.rm = TRUE), 100))
      medias[[cov]] <- rango
      nd <- as.data.frame(medias)
      
      preds <- unmarked::predict(fm, type = tipo, newdata = nd,
                                 appendData = FALSE)
      
      df_plot <- data.frame(
        x      = rango,
        pred   = preds$Predicted,
        lower  = preds$lower,
        upper  = preds$upper
      )
      
      etiqueta_y <- if (tipo == "state") "ψ (probabilidad de ocupación)"
      else                  "p (probabilidad de detección)"
      color_use  <- if (tipo == "state") colores$primario else colores$acento
      
      ggplot2::ggplot(df_plot,
                      ggplot2::aes(x = x, y = pred, ymin = lower, ymax = upper)) +
        ggplot2::geom_ribbon(fill = color_use, alpha = 0.15) +
        ggplot2::geom_line(color = color_use, linewidth = 1.2) +
        ggplot2::ylim(0, 1) +
        ggplot2::labs(x = cov, y = etiqueta_y) +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
    })
    
    # ────────────────────────────────────────────────────
    # ψ POR SITIO
    # ────────────────────────────────────────────────────
    
    psi_sitios <- reactive({
      req(modelo_actual())
      preds <- unmarked::predict(modelo_actual(), type = "state")
      data.frame(
        sitio  = paste("Sitio", seq_len(nrow(preds))),
        psi    = preds$Predicted,
        lower  = preds$lower,
        upper  = preds$upper
      ) |>
        dplyr::arrange(dplyr::desc(psi))
    })
    
    output$vbox_sitios_ocu <- renderUI({
      req(psi_sitios(), input$umbral_ocu)
      n <- sum(psi_sitios()$psi >= input$umbral_ocu)
      div(class="card h-100",
          style=paste0("background:var(--color-background-secondary); border-radius:var(--border-radius-md); padding:1rem; text-align:center;"),
          div(style=paste0("font-size:13px; color:", colores$texto, "; margin-bottom:4px;"),
              bs_icon("geo-alt-fill", class="me-1"),
              paste0("Sitios con ψ ≥ ", input$umbral_ocu)),
          div(style=paste0("font-size:24px; font-weight:500; color:", colores$exito, ";"), n)
      )
    })
    
    output$plot_psi_sitio <- renderPlot({
      req(psi_sitios())
      df <- psi_sitios()
      df$sitio <- factor(df$sitio, levels = df$sitio)
      
      ggplot2::ggplot(df,
                      ggplot2::aes(x = psi, y = sitio, xmin = lower, xmax = upper,
                                   color = psi >= input$umbral_ocu)) +
        ggplot2::geom_errorbarh(height = 0.4, linewidth = 0.6, alpha = 0.6) +
        ggplot2::geom_point(size = 2.2) +
        ggplot2::geom_vline(xintercept = input$umbral_ocu,
                            linetype = "dashed", color = "#C85200") +
        ggplot2::scale_color_manual(
          values = c("FALSE" = "#A3ACB9", "TRUE" = colores$primario),
          guide  = "none"
        ) +
        ggplot2::xlim(0, 1) +
        ggplot2::labs(x = "ψ estimada", y = NULL) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          axis.text.y   = ggplot2::element_text(size = 7),
          panel.grid.minor = ggplot2::element_blank()
        )
    })
    
    output$tabla_psi_sitio <- renderDT({
      req(psi_sitios())
      datatable(
        psi_sitios() |> dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4))),
        options  = list(pageLength = 10, scrollX = TRUE,
                        dom = "tp", language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json")),
        rownames = FALSE,
        class    = "table-sm table-striped"
      )
    })
    
    output$descarga_psi_sitio <- downloadHandler(
      filename = function() "psi_por_sitio.csv",
      content  = function(file) {
        readr::write_csv(psi_sitios(), file)
      }
    )
    
    # ────────────────────────────────────────────────────
    # COMPARAR MODELOS (AIC)
    # ────────────────────────────────────────────────────
    
    modelos_guardados <- reactiveVal(list())
    
    observeEvent(input$guardar_modelo, {
      fm     <- modelo_actual()
      nombre <- trimws(input$nombre_modelo_guardar)
      if (is.null(fm)) {
        showNotification("Ajusta un modelo primero.",
                         type = "warning", duration = 3)
        return()
      }
      if (nchar(nombre) == 0) {
        showNotification("Escribe un nombre para el modelo.",
                         type = "warning", duration = 3)
        return()
      }
      nueva_lista <- modelos_guardados()
      nueva_lista[[nombre]] <- list(
        fit     = fm,
        formula = nombre_modelo_actual(),
        cov_ocu = input$cov_ocu,
        cov_det = input$cov_det
      )
      modelos_guardados(nueva_lista)
      updateTextInput(session, "nombre_modelo_guardar", value = "")
      showNotification(
        paste0("Modelo '", nombre, "' guardado."),
        type = "message", duration = 3
      )
    })
    
    observeEvent(input$limpiar_modelos, {
      modelos_guardados(list())
    })
    
    output$ui_sin_modelos <- renderUI({
      if (length(modelos_guardados()) == 0) {
        div(
          class = "alert alert-warning small py-2 px-3",
          bs_icon("exclamation-triangle", class = "me-1"),
          "Aún no has guardado ningún modelo. Ajusta modelos en la pestaña ",
          "'Ajustar modelo' y usa el botón 'Guardar para comparar'."
        )
      } else NULL
    })
    
    tabla_aic_df <- reactive({
      req(length(modelos_guardados()) >= 1)
      fl  <- do.call(unmarked::fitList, modelos_guardados())
      sel <- unmarked::modSel(fl)
      
      # Extraer AIC, nPars y logLik directamente de cada modelo
      nms  <- names(modelos_guardados())
      mods <- modelos_guardados()
      df   <- data.frame(
        Modelo        = nms,
        `Núm. params.` = sapply(nms, function(n) length(coef(mods[[n]]))),
        AIC           = sapply(nms, function(n) mods[[n]]@AIC),
        `log-lik`     = sapply(nms, function(n) -mods[[n]]@negLogLike),
        check.names   = FALSE,
        stringsAsFactors = FALSE
      ) |>
        dplyr::arrange(AIC) |>
        dplyr::mutate(
          `ΔAIC` = AIC - min(AIC),
          `w_i`  = exp(-0.5 * `ΔAIC`),
          `w_i`  = `w_i` / sum(`w_i`)
        ) |>
        dplyr::select(Modelo, `Núm. params.`, AIC, `ΔAIC`, `w_i`, `log-lik`)
      df
    })
    
    output$tabla_aic <- renderDT({
      req(tabla_aic_df())
      datatable(
        tabla_aic_df() |> dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3))),
        options  = list(dom = "t", pageLength = 20,
                        language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json")),
        rownames = FALSE,
        class    = "table-sm table-striped"
      )
    })
    
    output$plot_aic <- renderPlot({
      req(tabla_aic_df())
      df <- tabla_aic_df()
      df$Modelo <- factor(df$Modelo, levels = rev(df$Modelo))
      
      ggplot2::ggplot(df, ggplot2::aes(x = `w_i`, y = Modelo)) +
        ggplot2::geom_col(
          fill = colores$primario, alpha = 0.8, width = 0.6
        ) +
        ggplot2::geom_text(
          ggplot2::aes(label = paste0("ΔAIC = ", round(`ΔAIC`, 1))),
          hjust = -0.1, size = 3.5, color = colores$texto
        ) +
        ggplot2::xlim(0, max(df$`w_i`) * 1.25) +
        ggplot2::labs(x = "Peso de Akaike (w_i)", y = NULL) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
    })
    
    # ────────────────────────────────────────────────────
    # DIAGNÓSTICO (Bondad de ajuste — MacKenzie & Bailey 2004)
    # ────────────────────────────────────────────────────
    
    resultado_gof <- reactiveVal(NULL)
    
    # ── Estadística χ² sobre frecuencias de historiales ──
    # Misma función usada en parboot Y en el gráfico de frecuencias,
    # para que el p-valor y el gráfico sean coherentes.
    chi2_historiales <- function(fm) {
      y_mat <- unmarked::getY(fm@data)
      p_mat <- unmarked::fitted(fm)
      # ψ por sitio — usar predict() para respetar covariables
      psi_v <- unmarked::predict(fm, type = "state")$Predicted
      
      historiales <- apply(y_mat, 1, function(x)
        paste(ifelse(is.na(x), "NA", as.character(x)), collapse = ""))
      obs_freq <- as.numeric(table(historiales))
      unico    <- names(table(historiales))
      
      esp_freq <- sapply(unico, function(h) {
        chars <- strsplit(h, "")[[1]]
        sum(sapply(seq_len(nrow(p_mat)), function(i) {
          p_i   <- p_mat[i, ]
          psi_i <- psi_v[i]
          prob_ocu <- prod(ifelse(chars == "1", p_i,
                                  ifelse(chars == "0", 1 - p_i, 1)), na.rm = TRUE)
          if (all(chars[!is.na(chars)] == "0"))
            psi_i * prob_ocu + (1 - psi_i)
          else
            psi_i * prob_ocu
        }))
      })
      
      sum((obs_freq - esp_freq)^2 / (esp_freq + 0.5))
    }
    
    observeEvent(input$correr_gof, {
      req(modelo_actual())
      nsim <- input$nsim_gof
      withProgress(message = "Ejecutando bootstrap…", value = 0, {
        tryCatch({
          res <- unmarked::parboot(
            modelo_actual(),
            statistic = chi2_historiales,
            nsim      = nsim,
            parallel  = FALSE
          )
          incProgress(1)
          resultado_gof(res)
        }, error = function(e) {
          showNotification(
            paste("Error en bootstrap:", conditionMessage(e)),
            type = "error"
          )
        })
      })
    })
    
    output$res_gof_ui <- renderUI({
      req(resultado_gof())
      res  <- resultado_gof()
      tobs <- res@t0
      tsim <- res@t.star[, 1]
      pval <- mean(tsim >= tobs)
      clase <- if (pval >= 0.10) "sem-ok"
      else if (pval >= 0.05) "sem-warn"
      else "sem-bad"
      
      div(
        class = paste("p-3 rounded mb-2", clase),
        strong("χ² observado: "), round(tobs, 2), tags$br(),
        strong("p-valor bootstrap: "), round(pval, 3), tags$br(),
        if (pval >= 0.05)
          span(style = paste0("color:", colores$exito),
               bs_icon("check-circle"), " Buen ajuste del modelo")
        else
          span(style = paste0("color:", colores$peligro),
               bs_icon("x-circle"), " Posible falta de ajuste — revisa covariables o supuesto de cierre")
      )
    })
    
    output$plot_gof <- renderPlot({
      req(resultado_gof())
      res  <- resultado_gof()
      tobs <- res@t0
      tsim <- res@t.star[, 1]
      
      df <- data.frame(chi2 = tsim)
      ggplot2::ggplot(df, ggplot2::aes(x = chi2)) +
        ggplot2::geom_histogram(
          fill = "#A3ACB9", color = "#ffffff", bins = 30
        ) +
        ggplot2::geom_vline(
          xintercept = tobs,
          color = colores$peligro, linewidth = 1.2, linetype = "solid"
        ) +
        ggplot2::annotate(
          "text", x = tobs, y = Inf, vjust = 2, hjust = -0.1,
          label = paste0("χ²obs = ", round(tobs, 1)),
          color = colores$peligro, size = 4
        ) +
        ggplot2::labs(
          x = "χ² simulado", y = "Frecuencia",
          title = "Distribución nula (bootstrap)"
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
    })
    
    output$plot_histfreq <- renderPlot({
      req(modelo_actual())
      fm    <- modelo_actual()
      y_mat <- unmarked::getY(fm@data)
      p_mat <- unmarked::fitted(fm)
      # ψ por sitio — usar predict() para respetar covariables
      psi_v <- unmarked::predict(fm, type = "state")$Predicted
      
      # Misma lógica que chi2_historiales — frecuencias obs. y esp.
      historiales <- apply(y_mat, 1, function(x)
        paste(ifelse(is.na(x), "NA", as.character(x)), collapse = ""))
      obs_tbl  <- table(historiales)
      unico    <- names(obs_tbl)
      obs_freq <- as.numeric(obs_tbl)
      
      esp_freq <- sapply(unico, function(h) {
        chars <- strsplit(h, "")[[1]]
        sum(sapply(seq_len(nrow(p_mat)), function(i) {
          p_i   <- p_mat[i, ]
          psi_i <- psi_v[i]
          prob_ocu <- prod(ifelse(chars == "1", p_i,
                                  ifelse(chars == "0", 1 - p_i, 1)), na.rm = TRUE)
          if (all(chars[!is.na(chars)] == "0"))
            psi_i * prob_ocu + (1 - psi_i)
          else
            psi_i * prob_ocu
        }))
      })
      
      df_long <- data.frame(
        Historia   = rep(unico, 2),
        Frecuencia = c(obs_freq, esp_freq),
        Tipo       = factor(rep(c("Observado", "Esperado"), each = length(unico)),
                            levels = c("Observado", "Esperado"))
      )
      
      ggplot2::ggplot(df_long,
                      ggplot2::aes(x = Historia, y = Frecuencia, fill = Tipo)) +
        ggplot2::geom_col(
          position = ggplot2::position_dodge(width = 0.7),
          width = 0.65, alpha = 0.85
        ) +
        ggplot2::scale_fill_manual(
          values = c("Observado" = colores$primario,
                     "Esperado"  = colores$acento)
        ) +
        ggplot2::labs(
          x       = "Historial de detección",
          y       = "Número de sitios",
          fill    = NULL,
          caption = paste0("χ² = ", round(chi2_historiales(fm), 2),
                           "  |  Barras cercanas = buen ajuste")
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          legend.position  = "top",
          axis.text.x      = ggplot2::element_text(
            angle = 45, hjust = 1, family = "mono", size = 10),
          panel.grid.minor = ggplot2::element_blank(),
          plot.caption     = ggplot2::element_text(color = colores$texto, size = 10)
        )
    })
    
    # ────────────────────────────────────────────────────
    # PESTAÑA 4: Explorar
    # ────────────────────────────────────────────────────
    
    output$expl_sel_cov <- renderUI({
      df <- umf(); req(df)
      site_covs <- unmarked::siteCovs(df)
      req(!is.null(site_covs))
      nums <- names(site_covs)[sapply(site_covs, is.numeric)]
      req(length(nums) > 0)
      selectInput(ns("expl_cov"), "Covariable:", choices = nums, selected = nums[1])
    })
    
    output$expl_cards_resumen <- renderUI({
      df <- umf(); req(df)
      y_mat <- unmarked::getY(df)
      n_sitios    <- nrow(y_mat)
      n_detectados <- sum(apply(y_mat, 1, function(x) any(x == 1, na.rm = TRUE)), na.rm = TRUE)
      naive_ocu   <- round(n_detectados / n_sitios, 3)
      tagList(
        div(class = "d-flex gap-2 mt-2",
            div(class = "card text-center flex-fill p-2",
                style = paste0("background:", colores$fondo),
                h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
                   n_detectados),
                p(class = "small text-muted mb-0", "Sitios con \u22651 detecci\u00f3n")
            ),
            div(class = "card text-center flex-fill p-2",
                style = paste0("background:", colores$fondo),
                h5(style = paste0("color:", colores$acento, "; font-weight:700;"),
                   paste0(round(naive_ocu * 100), "%")),
                p(class = "small text-muted mb-0", "Ocupaci\u00f3n naive")
            )
        )
      )
    })
    
    output$expl_plot_cov <- renderPlot({
      df <- umf(); req(df, input$expl_cov)
      y_mat     <- unmarked::getY(df)
      site_covs <- unmarked::siteCovs(df)
      req(!is.null(site_covs), input$expl_cov %in% names(site_covs))
      
      det_naive <- as.integer(apply(y_mat, 1, function(x) any(x == 1, na.rm = TRUE)))
      x_var     <- site_covs[[input$expl_cov]]
      
      df_plot <- data.frame(x = x_var, det = det_naive)
      
      ggplot2::ggplot(df_plot, ggplot2::aes(x = x, y = det)) +
        ggplot2::geom_jitter(height = 0.04, alpha = 0.4, size = 2,
                             color = colores$primario) +
        ggplot2::geom_smooth(method = "glm", formula = y ~ x,
                             method.args = list(family = binomial()),
                             se = TRUE, color = colores$acento,
                             fill = colores$secundario, alpha = 0.15,
                             linewidth = 1.2) +
        ggplot2::scale_y_continuous(breaks = c(0, 1),
                                    labels = c("0 (no detectado)", "1 (detectado)")) +
        ggplot2::labs(x = input$expl_cov, y = "Detecci\u00f3n naive",
                      subtitle = paste0("Curva log\u00edstica \u2014 n = ",
                                        nrow(df_plot), " sitios")) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                       plot.subtitle = ggplot2::element_text(
                         color = colores$texto, size = 9))
    }, res = 96)
    
    output$expl_plot_corr <- renderPlot({
      df <- umf(); req(df)
      site_covs <- unmarked::siteCovs(df)
      req(!is.null(site_covs))
      nums <- site_covs[, sapply(site_covs, is.numeric), drop = FALSE]
      req(ncol(nums) >= 2)
      
      cor_mat <- cor(nums, use = "complete.obs")
      df_cor  <- as.data.frame(as.table(cor_mat))
      names(df_cor) <- c("Var1", "Var2", "Correlation")
      
      ggplot2::ggplot(df_cor, ggplot2::aes(x = Var1, y = Var2,
                                           fill = Correlation)) +
        ggplot2::geom_tile(color = "white") +
        ggplot2::geom_label(ggplot2::aes(
          label      = round(Correlation, 2),
          color      = ifelse(abs(Correlation) > 0.4, "light", "dark"),
          fill       = Correlation),
          size       = 3.5,
          label.size = 0,
          label.padding = ggplot2::unit(0.15, "lines"),
          alpha      = 0.55) +
        ggplot2::scale_color_manual(
          values = c(light = "white", dark = "grey20"),
          guide  = "none") +
        ggplot2::scale_fill_gradient2(
          low = colores$peligro, mid = "white",
          high = colores$primario, midpoint = 0,
          limits = c(-1, 1), name = "r") +
        ggplot2::labs(x = NULL, y = NULL) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                       panel.grid = ggplot2::element_blank())
    }, res = 96)
    
    # ────────────────────────────────────────────────────
    # PARÁMETROS: VIF + Importancia
    # ────────────────────────────────────────────────────
    
    output$tabla_vif <- renderUI({
      fm <- modelo_actual(); req(fm)
      tryCatch({
        vif_res <- unmarked::vif(fm, type = "state")
        df_vif  <- data.frame(
          Covariable = names(vif_res),
          VIF        = round(vif_res, 3)
        )
        df_vif$Estado <- ifelse(df_vif$VIF > 10, "⚠ Severo",
                                ifelse(df_vif$VIF > 5,  "⚠ Moderado", "✓ OK"))
        col_estado <- ifelse(df_vif$VIF > 10, colores$peligro,
                             ifelse(df_vif$VIF > 5,  colores$acento, colores$exito))
        filas <- lapply(seq_len(nrow(df_vif)), function(i) {
          tags$tr(
            tags$td(strong(df_vif$Covariable[i])),
            tags$td(style = "text-align:center;", df_vif$VIF[i]),
            tags$td(style = paste0("color:", col_estado[i],
                                   "; font-weight:600; text-align:center;"),
                    df_vif$Estado[i])
          )
        })
        tags$table(
          class = "table table-sm small mb-0",
          tags$thead(tags$tr(
            tags$th("Covariable"), tags$th("VIF"), tags$th("Estado")
          )),
          tags$tbody(filas)
        )
      }, error = function(e) {
        p(class = "small text-muted",
          "VIF disponible solo con \u22652 covariables en el submodelo de ocupaci\u00f3n.")
      })
    })
    
    output$plot_importancia <- renderPlot({
      fm_std <- modelo_actual_std(); req(fm_std)
      tryCatch({
        # Extraer coeficientes de ambos submodelos
        est_state <- unmarked::coef(fm_std, type = "state")
        est_det   <- unmarked::coef(fm_std, type = "det")
        se_state  <- unmarked::SE(fm_std, type = "state")
        se_det    <- unmarked::SE(fm_std, type = "det")
        
        # Combinar en un data.frame
        coef_all <- c(est_state, est_det)
        se_all   <- c(se_state,  se_det)
        submod   <- c(rep("Ocupación (ψ)", length(est_state)),
                      rep("Detección (p)",  length(est_det)))
        
        # z-value para significancia aproximada
        z_all  <- coef_all / se_all
        p_all  <- 2 * pnorm(-abs(z_all))
        
        coef_df <- data.frame(
          param    = names(coef_all),
          Estimate = coef_all,
          SE       = se_all,
          p        = p_all,
          submodelo = submod,
          row.names = NULL
        )
        
        # Quitar interceptos
        coef_df <- coef_df[!grepl("\\(Intercept\\)", coef_df$param), ]
        if (nrow(coef_df) == 0) return(
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0.5, y = 0.5,
                              label = "No hay covariables en el modelo.",
                              color = colores$texto, size = 4) +
            ggplot2::theme_void()
        )
        
        coef_df$abs_est <- abs(coef_df$Estimate)
        coef_df$dir     <- ifelse(coef_df$Estimate >= 0, "Positivo", "Negativo")
        coef_df$sig_chr <- ifelse(!is.na(coef_df$p) & coef_df$p < 0.05,
                                  "sig", "no_sig")
        coef_df$param   <- factor(coef_df$param,
                                  levels = coef_df$param[order(coef_df$abs_est)])
        
        ggplot2::ggplot(coef_df,
                        ggplot2::aes(x = abs_est, y = param,
                                     fill = dir, alpha = sig_chr)) +
          ggplot2::geom_col(width = 0.65) +
          ggplot2::geom_text(
            ggplot2::aes(label = sprintf("%+.3f", Estimate)),
            hjust = -0.15, size = 3.5, color = colores$texto) +
          ggplot2::facet_grid(submodelo ~ ., scales = "free_y", space = "free_y") +
          ggplot2::scale_fill_manual(
            values = c("Positivo" = colores$primario,
                       "Negativo" = colores$peligro),
            name = "Direcci\u00f3n") +
          ggplot2::scale_alpha_manual(
            values = c("sig" = 1, "no_sig" = 0.35), guide = "none") +
          ggplot2::scale_x_continuous(
            expand = ggplot2::expansion(mult = c(0, 0.2))) +
          ggplot2::labs(
            x = "Importancia (\u03b2 estandarizado en SD)",
            y = NULL,
            subtitle = "Barras transparentes = p \u2265 0.05 \u00b7 Mayor barra = mayor peso relativo") +
          ggplot2::theme_minimal(base_size = 12) +
          ggplot2::theme(
            panel.grid.minor   = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_blank(),
            legend.position    = "bottom",
            strip.text         = ggplot2::element_text(
              color = colores$primario, face = "bold"),
            plot.subtitle      = ggplot2::element_text(
              color = colores$texto, size = 9))
      }, error = function(e) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
                            label = "Ajusta el modelo para ver la importancia.",
                            color = colores$texto, size = 4) +
          ggplot2::theme_void()
      })
    }, res = 96)
    
    # ────────────────────────────────────────────────────
    # PESTAÑA 8: ψ por sitio — ranef + bup
    # ────────────────────────────────────────────────────
    
    ranef_data <- reactive({
      fm <- modelo_actual(); req(fm)
      tryCatch({
        re     <- unmarked::ranef(fm)
        bup_m  <- unmarked::bup(re, stat = "mean")
        preds  <- unmarked::predict(fm, type = "state")
        y_mat  <- unmarked::getY(fm@data)
        hist_v <- apply(y_mat, 1, function(x)
          paste(ifelse(is.na(x), "NA", as.character(x)), collapse = ""))
        data.frame(
          sitio     = paste("Sitio", seq_along(bup_m)),
          psi_cov   = round(preds$Predicted, 4),
          psi_post  = round(bup_m, 4),
          diff      = round(bup_m - preds$Predicted, 4),
          historial = hist_v
        )
      }, error = function(e) NULL)
    })
    
    output$plot_ranef_scatter <- renderPlot({
      df <- ranef_data(); req(df)
      ggplot2::ggplot(df, ggplot2::aes(x = psi_cov, y = psi_post,
                                       color = historial)) +
        ggplot2::geom_abline(slope = 1, intercept = 0,
                             linetype = "dashed", color = colores$texto,
                             linewidth = 0.8) +
        ggplot2::geom_point(size = 2.5, alpha = 0.7) +
        ggplot2::scale_color_viridis_d(name = "Historial", option = "D") +
        ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
        ggplot2::labs(
          x = "\u03c8 por covariables (predict)",
          y = "\u03c8 posterior (bup \u00b7 ranef)",
          subtitle = paste0(
            "Puntos bajo la diagonal = detecciones reducen la estimaci\u00f3n. ",
            "Puntos con historial 0000 tienden a estar m\u00e1s abajo.")) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                       legend.position  = "bottom",
                       plot.subtitle    = ggplot2::element_text(
                         color = colores$texto, size = 8))
    }, res = 96)
    
    output$plot_ranef_diff <- renderPlot({
      df <- ranef_data(); req(df)
      df <- df[order(df$diff), ]
      df$sitio <- factor(df$sitio, levels = df$sitio)
      ggplot2::ggplot(df, ggplot2::aes(x = diff, y = sitio,
                                       fill = diff > 0)) +
        ggplot2::geom_col(width = 0.7) +
        ggplot2::geom_vline(xintercept = 0, linewidth = 0.7,
                            color = colores$texto) +
        ggplot2::scale_fill_manual(
          values = c("TRUE" = colores$primario,
                     "FALSE" = colores$peligro),
          guide = "none") +
        ggplot2::labs(
          x = "\u03c8post \u2212 \u03c8cov",
          y = NULL,
          subtitle = "Rojo = detecciones reducen la estimaci\u00f3n (menos ocupaci\u00f3n de la esperada)") +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          axis.text.y      = ggplot2::element_text(size = 7),
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_blank(),
          plot.subtitle    = ggplot2::element_text(
            color = colores$texto, size = 8))
    }, res = 96)
    
    output$tabla_ranef <- renderDT({
      df <- ranef_data(); req(df)
      datatable(
        df,
        colnames = c("Sitio", "\u03c8 covariables", "\u03c8 posterior",
                     "Diferencia", "Historial"),
        options  = list(pageLength = 10, scrollX = TRUE,
                        dom = "tp",
                        language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json")),
        rownames = FALSE,
        class    = "table-sm table-striped"
      ) |>
        formatStyle("diff",
                    color = styleInterval(0, c(colores$peligro, colores$primario)),
                    fontWeight = "bold")
    })
    
    # ────────────────────────────────────────────────────
    # PESTAÑA 9: Performance — c-hat + crossVal
    # ────────────────────────────────────────────────────
    
    output$ui_chat <- renderUI({
      res <- resultado_gof()
      if (is.null(res)) return(
        div(class = "alert alert-warning small py-2 px-3 mb-0",
            bs_icon("exclamation-triangle", class = "me-1"),
            "Corre primero el GoF en la pesta\u00f1a ", strong("Diagn\u00f3stico"),
            " para calcular c-hat.")
      )
      tobs  <- res@t0[1]
      tsim  <- res@t.star[, 1]
      chat  <- round(tobs / mean(tsim), 3)
      col   <- if (chat > 3) colores$peligro else
        if (chat > 2) colores$acento  else colores$exito
      tagList(
        div(class = "text-center py-3",
            h2(style = paste0("color:", col, "; font-weight:700; font-size:2.5rem;"),
               chat),
            p(class = "text-muted mb-0", strong("\u0109 (c-hat)")),
            p(class = "small text-muted",
              if (chat <= 1.5) "\u2714 Buen ajuste (\u0109 \u2248 1)"
              else if (chat <= 2) "\u26a0 Sobredispersi\u00f3n leve"
              else if (chat <= 3) "\u26a0 Sobredispersi\u00f3n moderada — considerar QAICc"
              else "\u274c Sobredispersi\u00f3n severa — revisar el modelo"
            )
        ),
        tags$hr(),
        div(class = "small text-muted",
            p(strong("\u03c7\u00b2 observado: "), round(tobs, 2)),
            p(strong("Media \u03c7\u00b2 simulado: "), round(mean(tsim), 2)),
            p(strong("Simulaciones: "), length(tsim))
        )
      )
    })
    
    cv_resultado <- reactiveVal(NULL)
    
    observeEvent(input$correr_cv, {
      fm <- modelo_actual()
      if (is.null(fm)) {
        showNotification("Ajusta un modelo primero.",
                         type = "warning", duration = 3)
        return()
      }
      withProgress(message = "Corriendo validaci\u00f3n cruzada...", value = 0.3, {
        tryCatch({
          res_cv <- unmarked::crossVal(fm, method = "Kfold",
                                       folds = input$cv_folds)
          cv_resultado(res_cv)
        }, error = function(e) {
          showNotification(paste("Error en CV:", conditionMessage(e)),
                           type = "error", duration = 6)
        })
      })
    })
    
    output$resultado_cv <- renderUI({
      res <- cv_resultado()
      if (is.null(res)) return(
        div(class = "text-muted small py-3",
            bs_icon("arrow-repeat", class = "me-2"),
            "Haz clic en ", strong("Correr CV"), " para evaluar.")
      )
      tryCatch({
        s   <- summary(res)
        df  <- as.data.frame(s)
        tagList(
          tags$table(
            class = "table table-sm small mb-2",
            tags$thead(tags$tr(
              lapply(names(df), tags$th)
            )),
            tags$tbody(lapply(seq_len(nrow(df)), function(i) {
              tags$tr(lapply(df[i, ], function(v)
                tags$td(if (is.numeric(v)) round(v, 4) else v)))
            }))
          ),
          div(class = "alert alert-info small py-2 px-3 mb-0",
              bs_icon("info-circle", class = "me-1"),
              strong(paste0(input$cv_folds, "-fold CV")),
              " \u2014 menor RMSE = mejor capacidad predictiva.")
        )
      }, error = function(e) {
        div(class = "text-danger small",
            paste("Error al mostrar resultados:", conditionMessage(e)))
      })
    })
    
    # ────────────────────────────────────────────────────
    # CÓDIGO R
    # ────────────────────────────────────────────────────
    
    codigo_generado <- reactive({
      req(modelo_actual())
      det_covs <- if (is.null(input$cov_det) || length(input$cov_det) == 0)
        "1" else paste(input$cov_det, collapse = " + ")
      ocu_covs <- if (is.null(input$cov_ocu) || length(input$cov_ocu) == 0)
        "1" else paste(input$cov_ocu, collapse = " + ")
      fuente <- input$fuente_datos
      
      encabezado_script("StatMonitor", "Modelos de ocupación") |>
        paste0(
          "# ── Paquetes ──────────────────────────────────────────\n",
          "library(unmarked)\n",
          "library(tidyverse)\n",
          "library(AICcmodavg)  # opcional, para selección avanzada\n\n",
          
          "# ── Datos ─────────────────────────────────────────────\n",
          if (fuente == "propio") {
            paste0(
              "datos <- read_csv(\"tu_archivo.csv\")  # o read_excel()\n",
              "cols_y <- grep(\"^y\\\\.\", names(datos), value = TRUE)\n",
              "y_mat  <- as.matrix(datos[, cols_y])\n",
              "cov_sitio <- datos[, setdiff(names(datos), cols_y)]\n\n"
            )
          } else {
            paste0(
              "# Dataset: ", fuente, " (generado internamente en StatMonitor)\n",
              "# Para reproducir, usa los datos exportados desde la pestaña 'Los datos'\n\n"
            )
          },
          
          "# ── Crear objeto unmarkedFrame ────────────────────────\n",
          "umf <- unmarkedFrameOccu(\n",
          "  y        = y_mat,     # matrix: sitios × ocasiones (0/1/NA)\n",
          "  siteCovs = cov_sitio  # data.frame de covariables de sitio\n",
          "  # obsCovs = cov_obs   # descomenta si tienes cov. de observación\n",
          ")\n",
          "summary(umf)\n\n",
          
          "# ── Modelo ajustado ───────────────────────────────────\n",
          "# Notación: occu(~detección ~ocupación, data)\n",
          "fm <- occu(\n",
          "  formula = ~", det_covs, " ~", ocu_covs, ",\n",
          "  data    = umf\n",
          ")\n",
          "summary(fm)\n\n",
          
          "# ── Estimaciones en escala de probabilidad ────────────\n",
          "backTransform(fm, type = \"state\")  # ψ\n",
          "backTransform(fm, type = \"det\")    # p\n\n",
          
          "# ── Predicciones por sitio ────────────────────────────\n",
          "psi_pred <- predict(fm, type = \"state\")\n",
          "head(psi_pred)\n\n",
          
          "# ── Comparación de modelos ───────────────────────────\n",
          "# Ajusta modelos candidatos y usa fitList + modSel\n",
          "m_nulo  <- occu(~1 ~1, data = umf)\n",
          "m_ocu   <- occu(~1 ~", ocu_covs, ", data = umf)\n",
          "m_final <- fm\n\n",
          "fl  <- fitList(m_nulo, m_ocu, m_final)\n",
          "modSel(fl)\n\n",
          
          "# ── Bondad de ajuste (MacKenzie & Bailey 2004) ──────\n",
          "chi2 <- function(fm) {\n",
          "  obs  <- getY(fm@data)\n",
          "  pred <- fitted(fm)\n",
          "  sum((obs - pred)^2 / (pred + 0.5), na.rm = TRUE)\n",
          "}\n",
          "gof <- parboot(fm, statistic = chi2, nsim = 500)\n",
          "plot(gof)\n",
          "gof\n"
        )
    })
    
    output$codigo_r <- renderText({ codigo_generado() })
    
    output$descarga_codigo <- downloadHandler(
      filename = function() {
        paste0("ocupacion_", format(Sys.Date(), "%Y%m%d"), ".R")
      },
      content = function(file) {
        writeLines(codigo_generado(), file)
      }
    )
    
  }) # /moduleServer
} # /mod_occu_unmarked_server