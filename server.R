library(shinydashboard)
library(shiny)
library(ggplot2)
library(plotly)
library(xts)
library(dplyr)
library(lubridate)
library(tidyr)
library(readr)

# path
initialPath <- "../../../" # para la version no portable
# initialPath <- as.character(readLines("../dropboxPath.txt")) # activar en versio portable
# paqueteria
source(paste0(initialPath, "ci_master/sinteticos/modules/main.R"), local=TRUE, chdir=TRUE) # las nuevas funciones
# base, controles, rendimientos, bond_info
db <- readRDS(paste0(initialPath, "ci_master/sinteticos/data/db_filled.RDS"))
controles <- readRDS(paste0(initialPath, "ci_master/sinteticos/controles.RDS"))
bond_info <- readRDS(paste0(initialPath, "ci_master/sinteticos/data/bond_info.RDS")) %>% 
  filter(vigente == 1) %>% 
  mutate(vencimiento = as.Date(vencimiento)) 

lista_BENCHMARKS <- names(db)[grepl("benchmark_", names(db))]


shinyServer(function(input, output, session) {
  
  # ================ CURVAS ==================
  output$curva_bonos <- renderPlotly({
    if (length(input$tipo_curvas) == 0 | length(input$fechas_curvas) == 0) {
      return(NULL)
    } else{
      info <- bond_info %>% 
        filter(tipovalor %in% c("M_", "S_"))  %>% # udibono y bono m
        filter(emisora %in% input$tipo_curvas)
      db_col_name <- paste0(info$id, "_yield")
      in_db <- is.na(match(db_col_name, names(db)))
      plot_data <- data.frame()
      for(fecha in as.character(input$fechas_curvas)) {
        temp <- info %>% 
          select(emisora, vencimiento) %>% 
          mutate(curva = paste(emisora, fecha)) 
        temp$fecha <- factor(fecha)
        temp$tasa <- 100*as.numeric(db[fecha, db_col_name])
        plot_data <- rbind(plot_data, temp)
      }
      p <- ggplot(data = plot_data, aes(x = vencimiento, y = tasa, group = curva)) +
        geom_line(aes(color = emisora)) +
        geom_point(aes(shape = fecha)) +
        scale_shape_discrete(solid=FALSE) +
        scale_y_continuous(labels = function(x) paste0(x, "%")) +
        theme_bw() + 
        theme(legend.title = element_blank())
      return(ggplotly(p))
    }
  })
    
  # ================ RENDIMIENTOS Y TASAS ==================
  instrumentos_tasas <- reactive({ # tasados
    id_BONOS <- bond_info$id[bond_info$tipovalor == "M_" & as.character(bond_info$vencimiento) %in% input$lista_BONOS] 
    id_UDIBONO <- bond_info$id[bond_info$tipovalor == "S_" & as.character(bond_info$vencimiento) %in% input$lista_UDIBONO] 
    c(id_BONOS, id_UDIBONO)
  })
  instrumentos_rendimientos <- reactive({  # no tasados
    id_FONDOS <- input$lista_FONDOS
    id_BENCHMARKS <- input$lista_BENCHMARKS
    id_TASADOS <- character(0)
    if (length(instrumentos_tasas()) > 0) {
      id_TASADOS <- paste0(instrumentos_tasas(), "_adj")
    }
    if (length(id_FONDOS) > 0) {
      id_FONDOS <- paste0(id_FONDOS, "_lag")
    }
    c(id_TASADOS, id_FONDOS, id_BENCHMARKS)
  })
  
  observe({
    input$BONOS_all
    updateSelectInput(session, "lista_BONOS", selected = bond_info$vencimiento[bond_info$tipovalor == "M_"])
  })
  observe({
    input$BONOS_none
    updateSelectInput(session, "lista_BONOS", selected = character(0))
  })
  observe({
    input$UDIBONO_all
    updateSelectInput(session, "lista_UDIBONO", selected = bond_info$vencimiento[bond_info$tipovalor == "S_"])
  })
  observe({
    input$UDIBONO_none
    updateSelectInput(session, "lista_UDIBONO", selected = character(0))
  })
  observe({
    input$FONDOS_all
    updateSelectInput(session, "lista_FONDOS", selected = c("51_cigubbe_0", "51_cigumpbe_0", "51_cigulpbe_0",
                                                             "51_cigubc_0", "51_cigumpc_0", "51_cigulpc_0"))
  })
  observe({
    input$FONDOS_none
    updateSelectInput(session, "lista_FONDOS", selected = character(0))
  })
  observe({
    input$BENCHMARKS_all
    updateSelectInput(session, "lista_BENCHMARKS", selected = c(lista_BENCHMARKS))
  })
  observe({
    input$BENCHMARKS_none
    updateSelectInput(session, "lista_BENCHMARKS", selected = character(0))
  })
  
  dates <- reactive({ # prevWorkingDay con opcion 0 deja la fecha fija si es laboral y si no la inmediata anterior laboral
    current_date <- min(max(index(db)), prevWorkingDay(input$fecha_rendimientos[2], 0))
    if(input$periodo==1) dates <- c(prevWorkingDay(input$fecha_rendimientos[1], 0), current_date) # manual
    if(input$periodo==2) dates <- c(prevWorkingDay(current_date, 5), current_date) # 5 dias
    if(input$periodo==3) dates <- c(prevWorkingDay(monthShift(current_date, -1), 0), current_date)
    if(input$periodo==4) dates <- c(prevWorkingDay(monthShift(current_date, -3), 0), current_date)
    if(input$periodo==5) dates <- c(prevWorkingDay(monthShift(current_date, -6), 0), current_date)
    if(input$periodo==6) dates <- c(prevWorkingDay(monthShift(current_date, -12), 0), current_date)
    if(input$periodo==7) dates <- c(monthPrevWorkingDay(current_date), current_date)
    if(input$periodo==8) dates <- c(yearPrevWorkingDay(current_date), current_date)
    dates
  })
  
  # ================ ____a. TASAS ==================
  output$hist_yield_plot <- renderPlotly({
    if (length(instrumentos_tasas()) == 0) {
      return(NULL)
    } else {
      data <- db[paste(dates()[1], dates()[2], sep="/"), paste0(instrumentos_tasas(), "_yield")] 
      plot_data <- data.frame(Fecha = index(data), coredata(data), check.names = FALSE) %>% 
        gather(Instrumento, Tasa, -Fecha) %>% 
        mutate(Instrumento = gsub("_yield", "", Instrumento)) %>% 
        mutate(Tasa = 100*Tasa)
      p <- ggplot(data = plot_data, aes(x = Fecha, y = Tasa, color = Instrumento)) +
        geom_point(size = 0.1) +
        geom_line() + 
        scale_y_continuous(label = function(x) paste0(x, "%")) +
        theme_bw()  + 
        theme(legend.title = element_blank()) 
      return(ggplotly(p))
    }
  })
  
  # ================ ____b. RENDIMIENTOS ==================
  output$hist_ret_plot <- renderPlotly({
    if (length(instrumentos_rendimientos()) == 0) {
      return(NULL)
    } else {
      data <- db[paste(dates()[1], dates()[2], sep="/"), instrumentos_rendimientos()] %>% 
        dlyRet() %>% accumulate()
      plot_data <- data.frame(Fecha = index(data), coredata(data), check.names = FALSE) %>% 
        gather(Instrumento, Rendimiento, -Fecha) %>% 
        mutate(Instrumento = gsub("_adj", "", Instrumento)) %>% 
        mutate(Instrumento = gsub("_lag", "", Instrumento)) %>% 
        mutate(Rendimiento = 100*Rendimiento)
      p <- ggplot(data = plot_data, aes(x = Fecha, y = Rendimiento, color = Instrumento)) +
        geom_point(size = 0.1) +
        geom_line() + 
        scale_y_continuous(label = function(x) paste0(x, "%")) +
        theme_bw()  + 
        theme(legend.title = element_blank()) 
      return(ggplotly(p))
    }
  })
  
  # ================ ____c. RIESGO RENDIMIENTO ==================
  
  output$risk_reward <- renderPlotly({
    if (length(instrumentos_rendimientos()) == 0) {
      return(NULL)
    } else {
      data <- db[paste(dates()[1], dates()[2], sep="/"), instrumentos_rendimientos()] %>% 
        dlyRet() 
      reward <- accumulate(data, last = TRUE)
      risk <- apply(data, 2, sd) * sqrt(nrow(data))
      plot_data <- data.frame(
        Instrumento =  gsub("_adj|_lag", "", names(data)), 
        Rendimiento = 100*reward, 
        Riesgo = 100*risk, 
        check.names = FALSE, 
        row.names = NULL
      ) 
      slopes <- data.frame(
        intercept = 0, 
        slope = plot_data$Rendimiento/plot_data$Riesgo
      )
      p <- ggplot(data = plot_data, aes(x = Riesgo, y = Rendimiento, color = Instrumento)) +
        geom_point(size = 4, alpha = 0.4) +
        geom_abline(data = slopes, aes(intercept = intercept ,slope = slope), colour = "red", linetype = 2) +
        scale_y_continuous(
          labels = function(x) paste0(x, "%"), 
          limits = c(min(0, min(plot_data$Rendimiento)), max(plot_data$Rendimiento))
        ) + 
        scale_x_continuous(
          labels = function(x) paste0(x, "%"), 
          limits = c(min(0, min(plot_data$Riesgo)), max(plot_data$Riesgo))
        ) +
        theme_bw() + 
        theme(legend.title = element_blank()) 
      ggplotly(p) %>% 
        add_annotations(data = plot_data, x = ~Riesgo, y =~Rendimiento, text = ~Instrumento)
    }
  })
  
  # ===============   AJUSTE DE NODOS ============================================
  
  rates_table <- reactive({
    if (input$func != "EQUIVALENT_NODES") return(NULL)
    types <- input$curves_to_interpolate
    if (length(types) == 0) return(NULL)
    tabl <- data.frame()
    for (type in types) {
      code <- type %>% gsub("BONOS M", "m_bonos", .) %>% gsub("UDIBONO", "s_udibono", .)
      yields <- db[input$date_equivalent_nodes, grep(sprintf("(%s)\\d{6}_yield", code), names(db))]
      maturity_dates <- paste0("20", stringr::str_extract(names(yields), "\\d{6}"))
      tabl <- rbind(tabl, data.frame(
        FECHA = maturity_dates,
        CURVA = type,
        TASA = as.numeric(yields)
      ))
    }
    tabl <- tabl %>% 
      spread(key = CURVA, value = TASA, fill = NA) %>% 
      mutate(FECHA = ymd(FECHA)) %>% 
      arrange(FECHA) %>% 
      filter(FECHA >= max(index(db)))
    tabl
  })
  
  linear_interpolation <- reactive({
    tabl <- rates_table()
    approximation <- na.approx(xts(tabl[ ,-1], tabl[ ,1]))
    coredata(approximation)
  })
  
  splines_interpolation <- reactive({
    tabl <- rates_table()
    approximation <- na.spline(xts(tabl[ ,-1], tabl[ ,1]))
    coredata(approximation)
  })
  
  ns_interpolation <- reactive({
    tabl <- rates_table()
    approximation <- list()
    for (j in 2:ncol(tabl)) {
      idx <- !is.na(tabl[[j]])
      rate <- tabl[[j]][idx]
      maturity_with_na <- as.numeric(ymd(tabl[[1]]) - ymd(input$date_equivalent_nodes)) 
      maturity <- maturity_with_na[idx]
      NS_params <- YieldCurve::Nelson.Siegel(rate, maturity)
      NS_rates <- YieldCurve::NSrates(xts(NS_params, ymd(input$date_equivalent_nodes)),  maturity_with_na)
      approximation[[names(tabl)[j]]] <- as.numeric(NS_rates)
    }
    as.data.frame(approximation)
  })
  
  interpolated_rates_table <- reactive({
    tabl <- rates_table()
    col_names <- names(tabl)[-1]
    if (is.null(tabl)) return(NULL)
    if ("Lineal" %in% input$interpolation_method) {
      new_cols <- data.frame(linear_interpolation())
      names(new_cols) <- paste(col_names, "(Lin)")
      tabl <- cbind(tabl, new_cols)
    }
    if ("Splines" %in% input$interpolation_method) {
      new_cols <- data.frame(splines_interpolation())
      names(new_cols) <- paste(col_names, "(Spl)")
      tabl <- cbind(tabl, new_cols)
    }
    if ("Nelson-Siegel" %in% input$interpolation_method) {
      new_cols <- data.frame(ns_interpolation())
      names(new_cols) <- paste(col_names, "(NS)")
      tabl <- cbind(tabl, new_cols)
    }
    tabl
  })
   
  output$interpolation_table <- renderTable({
    tabl <- interpolated_rates_table()
    if (is.null(tabl)) return(NULL)
    tabl$FECHA <-  as.character(tabl$FECHA)
    for (j in 2:ncol(tabl)) {
      tabl[[j]][!is.na(tabl[[j]])] <- tabl[[j]][!is.na(tabl[[j]])]  %>% scales::percent()
    }
    tabl
  })
  
  spreads <- reactive({
    if (is.null(interpolated_rates_table())) return(NULL)
    if (length(input$curves_to_interpolate) != 2) return(NULL)
    tabl <- interpolated_rates_table()
    spreads <- data.frame(FECHA = tabl[["FECHA"]])
    col_names <- c("ORIGINAL", input$interpolation_method)
    for (i in 1:(length(input$interpolation_method) + 1)) {
      spreads[[i + 1]] <- tabl[[2*i]] - tabl[[2*i + 1]]
      names(spreads)[i + 1] <- col_names[i]
    }
    spreads
  })
  
  output$spreads_table <- renderTable({
    if (is.null(spreads())) return(NULL)
    tabl <- spreads()
    tabl$FECHA <- as.character(tabl$FECHA)
    for (j in 2:ncol(tabl)) {
      tabl[[j]][!is.na(tabl[[j]])] <- tabl[[j]][!is.na(tabl[[j]])]  %>% scales::percent()
    }
    tabl
  })
  
  
  output$plot_interp_nodes <- renderPlotly({
    if (is.null(interpolated_rates_table())) return(NULL)
    plot_data <- interpolated_rates_table() %>%
      gather(key = INSTRUMENTO, value = TASA, -FECHA) %>%
      mutate(CURVA = gsub("BONOS M|UDIBONO|\\(|\\)| ", "", INSTRUMENTO)) %>%
      mutate(TASA = 100*TASA) %>% 
      mutate(CURVA = replace(CURVA, CURVA == "", "ORIGINAL"))
    p <- ggplot() +
      geom_point(data = na.omit(plot_data[plot_data$CURVA == "ORIGINAL", ]), aes(x = FECHA, y = TASA, group = INSTRUMENTO, color = CURVA))
    if (length(input$interpolation_method) > 0) {
      p <- p +
        geom_line(data = plot_data[plot_data$CURVA != "ORIGINAL", ], aes(x = FECHA, y = TASA, group = INSTRUMENTO, color = CURVA)) 
    }
    p <- p +
      theme_bw() +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      ggtitle(input$interpolation_method)
    ggplotly(p)
  })

  output$plot_spreads <- renderPlotly({
    if (is.null(interpolated_rates_table())) return(NULL)
    if (length(input$curves_to_interpolate) != 2) return(NULL)
    plot_data <- spreads() %>%
      gather(key = INSTRUMENTO, value = TASA, -FECHA) %>%
      mutate(TASA = 100*TASA)
    p <- ggplot() +
      geom_point(data = na.omit(plot_data[plot_data$INSTRUMENTO == "ORIGINAL", ]), aes(x = FECHA, y = TASA, color = INSTRUMENTO))
    if (length(input$interpolation_method) > 0) {
      p <- p +
        geom_line(data = plot_data[plot_data$INSTRUMENTO != "ORIGINAL", ], aes(x = FECHA, y = TASA, color = INSTRUMENTO))
    }
    p <- p +
      theme_bw() +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      ggtitle(input$interpolation_method)
    ggplotly(p)
  })
  
  output$download_interpolation <- downloadHandler(
    filename = function() "interp_yields.csv",
    content = function(con) readr::write_excel_csv(interpolated_rates_table(), con)
  )
  
  output$download_spreads <- downloadHandler(
    filename = function() "inflacion_implicita.csv",
    content = function(con) readr::write_excel_csv(interpolated_rates_table(), con)
  )
  
  
  # ========================== RIESGO RENDIMIENTO ACCIONES ======================================
  
  
  
  session$onSessionEnded(function() stopApp())
  
})
