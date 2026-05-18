# ============================================================
# mod_ocupacion.R — Modelos de ocupación de sitios
# StatOccu · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Familia: modelos de ocupación de una sola especie (MacKenzie et al. 2002)
# Datos: Ejemplo simulado de mamíferos con cámaras trampa, o CSV/XLSX propio
# Ecosistema: unmarked + tidyverse + ggplot2
# ============================================================

# ── UI ────────────────────────────────────────────────────
mod_ocupacion_ui <- function(id) {
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
      # PESTAÑA 4: Ajustar modelo
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
              ),
              actionButton(
                ns("guardar_modelo"),
                label = tagList(bs_icon("bookmark-plus", class = "me-1"), "Guardar para comparar"),
                class = "btn btn-outline-secondary"
              )
          ),
          div(class = "mt-3", uiOutput(ns("estado_ajuste_ui")))
        )
      ), # /PESTAÑA 4
      
      # ════════════════════════════════════════════════
      # PESTAÑA 5: Parámetros
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("list-ol", class = "me-1"), "Parámetros"),
        card_body(
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
          plotOutput(ns("plot_forest"), height = "300px")
        )
      ), # /PESTAÑA 5
      
      # ════════════════════════════════════════════════
      # PESTAÑA 6: Gráficos
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
      # PESTAÑA 7: ψ por sitio
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("pin-map", class = "me-1"), "ψ por sitio"),
        card_body(
          layout_columns(
            col_widths = c(4, 8),
            card(
              class = "border-0",
              style = paste0("background:", colores$fondo, ";"),
              card_body(
                sliderInput(ns("umbral_ocu"), "Umbral de ocupación:",
                            min = 0, max = 1, value = 0.5, step = 0.05),
                p(class = "small text-muted",
                  "Sitios con ψ estimado ≥ umbral = 'probablemente ocupados'."),
                uiOutput(ns("vbox_sitios_ocu")),
                tags$br(),
                downloadButton(ns("descarga_psi_sitio"), "Descargar tabla",
                               class = "btn btn-outline-secondary btn-sm w-100")
              )
            ),
            plotOutput(ns("plot_psi_sitio"), height = "420px")
          ),
          div(class = "mt-3", DTOutput(ns("tabla_psi_sitio")))
        )
      ), # /PESTAÑA 7
      
      # ════════════════════════════════════════════════
      # PESTAÑA 8: Comparar modelos
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("bar-chart-steps", class = "me-1"), "Comparar modelos"),
        card_body(
          p(class = "small text-muted mb-3",
            "Guarda modelos desde 'Ajustar modelo' y compáralos por AIC."
          ),
          actionButton(ns("limpiar_modelos"), "Limpiar lista",
                       class = "btn btn-outline-danger btn-sm mb-2",
                       icon = icon("trash")),
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
      # PESTAÑA 9: Diagnóstico
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
      # PESTAÑA 10: Código R
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

mod_ocupacion_server <- function(id) {
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
      req(modelo_actual(), nombre_modelo_actual())
      nueva_lista <- modelos_guardados()
      nombre <- nombre_modelo_actual()
      nueva_lista[[nombre]] <- modelo_actual()
      modelos_guardados(nueva_lista)
      showNotification(
        paste0("Modelo '", nombre, "' guardado."),
        type = "message"
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
} # /mod_ocupacion_server