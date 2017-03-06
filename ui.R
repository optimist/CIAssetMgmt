library(shiny)
library(shinydashboard)
library(dplyr)
library(xts)
library(plotly)
library(inhouse)

id_prices<- NULL
try({
  id_prices <- inhouse::price_ids()[-c(1:2)]
})
print(id_prices)

# path
initialPath <- "../../../" # para la version no portable
# initialPath <- as.character(readLines("../dropboxPath.txt")) # activar en versio portable
# base, controles, rendimientos, bond_info
db <- readRDS(paste0(initialPath, "ci_master/sinteticos/data/db_filled.RDS"))
bond_info <- readRDS(paste0(initialPath, "ci_master/sinteticos/data/bond_info.RDS")) %>% 
  filter(vigente == 1) %>% 
  mutate(vencimiento = as.Date(vencimiento)) 

lista_BENCHMARKS <- c(names(db)[grepl("benchmark_", names(db))], "_cmxpusdfix", "_cmxpusdv48")

ui <- dashboardPage(skin="green",
  dashboardHeader(title =  h5(strong("CIEstrategias"), br(), "Asset Management ", format(Sys.Date(), "%d/%m/%Y")), titleWidth = 220),
  dashboardSidebar(width=220,
                   box(width=12, solidHeader = TRUE, background = "olive",
                       radioButtons("func", label=NULL, choices=c(
                         "CURVAS" = "CURVAS",
                         "TASAS Y RENDIMIENTOS HISTORICOS" = "RENDIMIENTOS",
                         "NODOS EQUIVALENTES" = "EQUIVALENT_NODES",
                         "RIESGO RENDIMIENTO ACCIONES" = "RISKREWARD"),
                       selected = "RISKREWARD"
                       )
                   )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    # =============  CURVAS  ==============
    conditionalPanel(
      condition = "input.func == 'CURVAS'",
      selectInput(
        inputId = "tipo_curvas",
        label = "Tipo de curva",
        choices = c("BONOS", "UDIBONO"),
        selected = c("BONOS", "UDIBONO"),
        multiple = TRUE
      ),
      selectInput(
        inputId = "fechas_curvas",
        label = "Fechas",
        choices = sort(index(db), decreasing = TRUE),
        selected = max(index(db)),
        multiple = TRUE
      ),
      plotlyOutput("curva_bonos")
    ),
    # ============= RENDIMIENTOS =============
    conditionalPanel(
      condition = "input.func == 'RENDIMIENTOS'",
      selectInput("periodo", 
        label = "PERIODO",
        choices = c("<calendario>"=1, "5 dias habiles"=2, "1 mes"=3, "3 meses"=4, "6 meses"=5, "12 meses"=6, "MTD"=7, "YTD"=8), 
        selected = 1
      ),
      conditionalPanel(
        condition = "input.periodo == 1", 
        dateRangeInput(
          inputId = "fecha_rendimientos", 
          label = NULL,  
          start = min(index(db)), 
          end = max(index(db)), 
          language = "es", 
          separator = "/"
        )
      ),
      selectInput(
        inputId = "lista_BONOS",
        label = "BONOS",
        choices = bond_info$vencimiento[bond_info$tipovalor == "M_"],
        multiple = TRUE
      ),
      actionButton("BONOS_all", label = "Seleccionar todos"),
      actionButton("BONOS_none", label = "Quitar todos"),
      selectInput(
        inputId = "lista_UDIBONO",
        label = "UDIBONOS",
        choices = bond_info$vencimiento[bond_info$tipovalor == "S_"],
        multiple = TRUE
      ),
      actionButton("UDIBONO_all", label = "Seleccionar todos"),
      actionButton("UDIBONO_none", label = "Quitar todos"),
      selectInput(
        inputId = "lista_FONDOS",
        label = "FONDOS",
        choices = c("51_cigubc_0", "51_cigumpc_0", "51_cigulpc_0", "51_ciusdbf_2", "51_cibolsc_0", "52navigtrbf1"),
        multiple = TRUE
      ),
      actionButton("FONDOS_all", label = "Seleccionar todos"),
      actionButton("FONDOS_none", label = "Quitar todos"),
      selectInput(
        inputId = "lista_FONDOS_USD",
        label = "FONDOS EN USD",
        choices = c("51_ciusdbf_2_USD", "52navigtrbf1_USD"),
        multiple = TRUE
      ),
      selectInput(
        inputId = "lista_ACCIONES",
        label = "ACCIONES",
        choices = c("1iflot_"),
        multiple = TRUE
      ),
      selectInput(
        inputId = "lista_BENCHMARKS",
        label = "BENCHMARKS",
        choices = lista_BENCHMARKS,
        multiple = TRUE
      ),
      actionButton("BENCHMARKS_all", label = "Seleccionar todos"),
      actionButton("BENCHMARKS_none", label = "Quitar todos"),
      h3("Rendimientos historicos"),
      plotlyOutput("hist_ret_plot"),
      h3("Tasas historicas"),
      plotlyOutput("hist_yield_plot"),
      h3("Rendimiento / Riesgo"),
      plotlyOutput("risk_reward")
    ),
    
    # NODOS EQUIVALENTES ===============================================
    conditionalPanel(
      condition = "input.func == 'EQUIVALENT_NODES'",
      selectInput (
        inputId = "date_equivalent_nodes",
        label = "FECHA",
        choices = sort(index(db), decreasing = TRUE),
        selected = max(index(db))
      ),
      checkboxGroupInput (
        inputId = "interpolation_method",
        label = "METODO",
        choices = c("Lineal", "Splines", "Nelson-Siegel"),
        selected = NULL
      ),
      checkboxGroupInput(
        inputId = "curves_to_interpolate",
        label = "CURVAS",
        choices = c("BONOS M", "UDIBONO"),
        selected = c("BONOS M", "UDIBONO")
      ),
      h3("NODOS"),
      plotlyOutput("plot_interp_nodes"),
      downloadButton("download_interpolation", "Curvas Interpoladas"),
      tableOutput("interpolation_table"),
      h3("INFLACION IMPLICITA"),
      plotlyOutput("plot_spreads"),
      downloadButton("download_spreads", "Inflaciones Implicitas"),
      tableOutput("spreads_table")
    ),
    
    # RIESGO RENDIMIENTO ACCIONES =======================================
    conditionalPanel(
      condition = "input.func == 'RISKREWARD'",
      h3("SELECCION DE INTRUMENTOS"),
      selectInput(
        inputId = "instrumentos_db",
        label = "EN BASE LOCAL",
        choices = names(db),
        multiple = TRUE,
        selected = NULL
      ),
      selectInput(
        inputId = "instrumentos_prices",
        label = "EN PRICES",
        choices = id_prices,
        multiple = TRUE,
        selected = NULL
      ),
      textAreaInput(
        inputId = "instrumentos_yahoo",
        label = "EN YAHOO FINANCE",
        resize = "vertical"
      )
    )
  )
)

