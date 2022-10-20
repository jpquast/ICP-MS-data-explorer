library(shiny)
library(readxl)
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(plotly)
library(protti)
library(formattable)
library(forcats)
library(DT)

parameters <- data.frame(
  parameter = c("file_name", 
                "date",
                "ppb_quantile_low", 
                "ppb_quantile_mid", 
                "ppb_quantile_high", 
                "fit_quantile_high",
                "fit_quantile_mid",
                "fit_quantile_low",
                "cps_low",
                "cps_high",
                "rsd_quality_low",
                "rsd_quality_high",
                "best_metal_modes",
                "dilution_factor",
                "dilution_factor_samples",
                "weight"),
  value = c("",
            "",
            "0.5",
            "0.75",
            "0.9",
            "0.5",
            "0.25",
            "0.1",
            "1000",
            "100000000",
            "1",
            "2",
            "",
            "",
            "",
            "")
)

metal_mw <- data.frame(metal = c("Ca",
                                 "Mg",
                                 "Fe",
                                 "Co",
                                 "Cu",
                                 "Ni",
                                 "Zn",
                                 "Mn"),
                       mw = c(40.078,
                              24.305,
                              55.845,
                              58.933,
                              63.546,
                              58.6934,
                              65.38,
                              54.938))

ui <- fluidPage(
  navbarPage("ICP-MS Data Explorer",
             tabPanel("Calibration overview",
                      sidebarPanel(
                        fileInput("raw_data",
                                  "ICP-MS raw data",
                                  accept = c(".xlsx")),
                        h4("Parameter file"),
                        helpText("If you performed an analysis on the same dataset before and you downloaded the 
                                 parameter file, you can provide it here in order to obtain the same result."),
                        fileInput("parameters_input", label = NULL, accept = c(".csv")),
                        downloadButton("parameters_output", label = "Download Parameters")
                      ),
                      mainPanel(
                        width = 7,
                        plotlyOutput("error_plot"),
                        br(),
                        plotlyOutput("fit_plot")
                      )
             ),
             tabPanel(
               "Explore metal modes",
               sidebarPanel(
                 selectizeInput("metal_mode", "Select a metal mode", choices = NULL)
               ),
               mainPanel(
                 width = 7,
                 plotlyOutput("curve_plot"),
                 br(),
                 formattableOutput("curve_table")
               )
             ),
             tabPanel(
               "Metal mode quality",
               sidebarPanel(
                 selectizeInput("metal", "Select a metal", choices = NULL),
                 selectizeInput("best_metal_mode", "Select the best metal modes for analysis", choices = NULL, multiple = TRUE),
                 br(),
                 h2("Advanced settings"),
                 h4("ppb quality"),
                 sliderInput("ppb_quantile_low", 'Quantile for "good" (below) ppb quality', min = 0, max = 1, value = as.numeric(parameters[parameters$parameter == "ppb_quantile_low",]$value), step = 0.05),
                 sliderInput("ppb_quantile_mid", 'Quantile for "okay" (below) ppb quality', min = 0, max = 1, value = as.numeric(parameters[parameters$parameter == "ppb_quantile_mid",]$value), step = 0.05),
                 sliderInput("ppb_quantile_high", 'Quantile for "not good" (below) and "bad" (above) ppb quality', min = 0, max = 1, value = as.numeric(parameters[parameters$parameter == "ppb_quantile_high",]$value), step = 0.05),
                 br(),
                 h4("fit quality"),
                 sliderInput("fit_quantile_high", 'Quantile for "good" (above) fit quality', min = 0, max = 1, value = as.numeric(parameters[parameters$parameter == "fit_quantile_high",]$value), step = 0.05),
                 sliderInput("fit_quantile_mid", 'Quantile for "okay" (above) fit quality', min = 0, max = 1, value = as.numeric(parameters[parameters$parameter == "fit_quantile_mid",]$value), step = 0.05),
                 sliderInput("fit_quantile_low", 'Quantile for "not good" (above) and "bad" (below) fit quality', min = 0, max = 1, value = as.numeric(parameters[parameters$parameter == "fit_quantile_low",]$value), step = 0.05),
                 br(),
                 h4("cps quality"),
                 numericInput("cps_low", "The lower cps bound", value = as.numeric(parameters[parameters$parameter == "cps_low",]$value)),
                 numericInput("cps_high", "The higher cps bound", value = as.numeric(parameters[parameters$parameter == "cps_high",]$value)),
                 br(),
                 h4("rsd quality"),
                 sliderInput("rsd_quality", 'rsd quality for "good" (below first), "okay" (below second) and "bad" (above second)', 
                             min = 0,
                             max = 10,
                             value = c(as.numeric(parameters[parameters$parameter == "rsd_quality_low",]$value), as.numeric(parameters[parameters$parameter == "rsd_quality_high",]$value)),
                             step = 0.1)
               ),
               mainPanel(
                 width = 7,
                 htmlOutput("metal_title"),
                 br(),
                 formattableOutput("quality_table"),
                 br(),
                 plotlyOutput("ppb_sample_distribution")
               )
             ),
             tabPanel("Experiment overview",
                      sidebarPanel(
                        h2("Selected metal modes"),
                        verbatimTextOutput("list_best_metals")
                      ),
                      mainPanel(
                        width = 7,
                        DT::dataTableOutput("sample_table")
                      )
             ),
             tabPanel("Analyse experiment",
                      sidebarPanel(
                        h2("Specify sample properties"),
                        helpText("Once sample properties have been specified they appear in the results table."),
                        selectizeInput("sample", "Select samples", choices = NULL, multiple = TRUE),
                        numericInput("dilution_factor", "Specify dilution factor", value = NULL),
                        numericInput("weight", "Specify sample weight if applicable", value = NULL),
                        actionButton("add_sample_properties", "Add", icon = icon("plus")),
                        actionButton("reset_sample_properties", "Reset", icon = icon("trash-can")),
                        br(),
                        br(),
                        verbatimTextOutput("sample_properties"),
                        br(),
                        h2("Specify expected values"),
                        helpText("For each metal and sample combination specify expected values. If you don't know 
                                 what to expect just don't specify anything."),
                        selectizeInput("metal_expected", "Select metal", choices = NULL),
                        selectizeInput("sample_expected", "Select samples", choices = NULL, multiple = TRUE),
                        numericInput("value_expected", "Specify the expected value", value = NULL),
                        selectizeInput("unit_expected", "Unit", choices = c("ppb", "uM")),
                        actionButton("add_sample_expected", "Add", icon = icon("plus")),
                        actionButton("reset_sample_expected", "Reset", icon = icon("trash-can")),
                        br(),
                        br(),
                        verbatimTextOutput("sample_expected_output"),
                        br(),
                        downloadButton("download_result", label = "Download Results")
                      ),
                      mainPanel(
                        width = 7,
                        DT::dataTableOutput("result")
                      )
             ),
             tabPanel("Plot results",
                      sidebarPanel(
                        h2("Create plot"),
                        selectizeInput("sample_plot", "Select samples", choices = NULL, multiple = TRUE),
                        selectizeInput("plot_type", "Plot type", choices = c("Concentration in ppb", "Concentration in uM")),
                        numericInput("multiply_concentration", "Multiplicator for concentration (to change the unit)", value = 1),
                        textInput("plot_title", "Plot title", value = "Plot title"),
                        textInput("x_axis", "X-Axis", value = "Sample"),
                        textInput("y_axis", "Y-Axis", value = "Concentration"),
                        selectizeInput(
                          'colour', label = "Colour", selected = protti_colours[1:2], choices = protti_colours, multiple = TRUE,
                          options = list(create = TRUE)
                        ),
                        helpText("You can also add your own colour by just pasting a colour name or hex code."),
                        checkboxInput("plot_expected", "Plot expected values", FALSE),
                        numericInput("plot_height", "Plot height", value = 600),
                        numericInput("plot_width", "Plot width", value = 1000),
                        downloadButton("download_plot", "Download Plot")
                      ),
                      mainPanel(
                        width = 7,
                        uiOutput("plot.ui")
                      )
             ),
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # variables
  start <- 0
  list_best_metal_modes <- NULL
  sample_names <- NULL
  sample_properties_df <- data.frame(dilution_factor = c(),
                                     weight = c(),
                                     sample_name = c())
  sample_names_expected_df <- NULL
  sample_expected_df <- data.frame(metal = c(),
                                   expected = c(),
                                   sample_name = c())

  # Update parameter file if uploaded
  observeEvent(input$parameters_input, {
    parameters <<- suppressMessages(read.csv(input$parameters_input$datapath))
    
    # update inputs
    updateSliderInput(session, "ppb_quantile_low", value = as.numeric(parameters[parameters$parameter == "ppb_quantile_low",]$value))
    updateSliderInput(session, "ppb_quantile_mid", value = as.numeric(parameters[parameters$parameter == "ppb_quantile_mid",]$value))
    updateSliderInput(session, "ppb_quantile_high", value = as.numeric(parameters[parameters$parameter == "ppb_quantile_high",]$value))
    
    updateSliderInput(session, "fit_quantile_high", value = as.numeric(parameters[parameters$parameter == "fit_quantile_high",]$value))
    updateSliderInput(session, "fit_quantile_mid", value = as.numeric(parameters[parameters$parameter == "fit_quantile_mid",]$value))
    updateSliderInput(session, "fit_quantile_low", value = as.numeric(parameters[parameters$parameter == "fit_quantile_low",]$value))
    
    updateNumericInput(session, "cps_low", value = as.numeric(parameters[parameters$parameter == "cps_low",]$value))
    updateNumericInput(session, "cps_high", value = as.numeric(parameters[parameters$parameter == "cps_high",]$value))
    
    updateSliderInput(session, "rsd_quality", value = c(as.numeric(parameters[parameters$parameter == "rsd_quality_low",]$value), as.numeric(parameters[parameters$parameter == "rsd_quality_high",]$value)))
    
    list_best_metal_modes <<- map(split(str_remove_all(unlist(str_split(parameters[parameters$parameter == "best_metal_modes",]$value, pattern = "\\| ")), pattern = "[:alpha:]+: "), 
              str_extract(unlist(str_split(parameters[parameters$parameter == "best_metal_modes",]$value, pattern = "\\| ")), pattern = "[:alpha:]+(?=:)")),
        .f = ~{
          unlist(str_split(.x, pattern = ", "))
        }
    )
    
    sample_properties_df <<- parameters %>% 
      filter(parameter %in% c("dilution_factor", "dilution_factor_samples", "weight")) %>% 
      mutate(parameter = case_when(parameter == "dilution_factor_samples" ~ "sample_name",
                                   TRUE ~ parameter)) %>% 
      pivot_wider(names_from = parameter, values_from = value) %>% 
      mutate(across(everything(), ~str_split(.x, pattern = "\\| "))) %>% 
      unnest(c(dilution_factor, weight, sample_name)) %>% 
      mutate(dilution_factor = as.numeric(dilution_factor),
             weight = as.numeric(weight))
    
    sample_names <<- sample_names[!sample_names %in% unlist(str_split(sample_properties_df$sample_name, pattern = ","))]
    
    updateSelectizeInput(session, "sample", choices = sample_names, server = TRUE) 
    
    # expected concentrations
    sample_expected_df <<- parameters %>% 
      filter(parameter %in% c("expected_ppb", "expected_uM", "expected_sample", "expected_metal")) %>% 
      mutate(parameter = case_when(parameter == "expected_sample" ~ "sample_name",
                                   parameter == "expected_metal" ~ "metal",
                                   TRUE ~ parameter)) %>% 
      pivot_wider(names_from = parameter, values_from = value) %>% 
      mutate(across(everything(), ~str_split(.x, pattern = "\\| "))) %>% 
      unnest(c(expected_ppb, expected_uM, sample_name, metal)) %>% 
      mutate(expected_ppb = as.numeric(expected_ppb),
             expected_uM = as.numeric(expected_uM))
    
    exclude_samples <- sample_expected_df %>% 
      mutate(sample_name = str_split(sample_name, pattern = ",")) %>% 
      unnest(sample_name)
    
    sample_names_expected_df <<- sample_names_expected_df %>% 
      left_join(exclude_samples, by = c("sample_name", "metal")) %>% 
      filter(!(!is.na(expected_ppb) | !is.na(expected_uM)))
    
    if (input$metal_expected != ""){
      sample_names_subset <- sample_names_expected_df %>% 
        filter(metal == input$metal_expected) %>% 
        pull(sample_name)
    } else {
      sample_names_subset <- NULL
    }
    
    updateSelectizeInput(session, "sample_expected", choices = sample_names_subset, server = TRUE)
  })
  
  # Generate parameters file reactively once it is downloaded
  
  parameters_download <- reactive({
    best_metal_mode_string <- paste0(
      map2(.x = list_best_metal_modes,
                  .y = names(list_best_metal_modes),
                  .f = ~ {
                    paste0(.y, ": ", paste0(.x, collapse = ", "))
                  }), 
      collapse = "| ")
    
    if (nrow(sample_properties_df) != 0){
      sample_result_parameters <- sample_properties_df %>% 
        mutate(across(everything(), ~ as.character((.x)))) %>% 
        pivot_longer(cols = c(dilution_factor, weight, sample_name), names_to = "parameter", values_to = "value") %>% 
        group_by(parameter) %>% 
        mutate(value = paste0(value, collapse = "| ")) %>% 
        ungroup() %>% 
        distinct() %>% 
        mutate(parameter = case_when(parameter == "sample_name" ~ "dilution_factor_samples",
                                     TRUE ~ parameter))
    } else {
      sample_result_parameters <- data.frame(parameter = c('dilution_factor', 'weight', 'dilution_factor_samples'),
                                             value = c("", "", ""))
    }
    
    if (nrow(sample_expected_df) != 0){
      sample_expected_parameters <- sample_expected_df %>% 
        mutate(across(everything(), ~ as.character((.x)))) %>% 
        pivot_longer(cols = c(expected_ppb, expected_uM, metal, sample_name), names_to = "parameter", values_to = "value") %>% 
        group_by(parameter) %>% 
        mutate(value = paste0(value, collapse = "| ")) %>% 
        ungroup() %>% 
        distinct() %>% 
        mutate(parameter = case_when(parameter == "sample_name" ~ "expected_sample",
                                     parameter == "metal" ~ "expected_metal",
                                     TRUE ~ parameter))
    } else {
      sample_expected_parameters <- data.frame(parameter = c('expected_sample', 'expected_metal', 'expected_ppb', 'expected_uM'),
                                             value = c("", "", ""))
    }
    
    
    parameters <<- parameters %>% 
      mutate(value = ifelse(parameter == "best_metal_modes", best_metal_mode_string, value),
             value = ifelse(parameter == "date", as.character(Sys.time()), value),
             value = ifelse(parameter == "file_name", input$raw_data$name, value)
      ) %>% 
      filter(!parameter %in% c("dilution_factor", "dilution_factor_samples", "weight")) %>% 
      bind_rows(sample_result_parameters) %>% 
      bind_rows(sample_expected_parameters)
  })
  
  # Download parameters
  output$parameters_output <- downloadHandler(
    filename = function() {
      paste0("parameters_", Sys.time(), ".csv")
    },
    content = function(file) {
      write.csv(parameters_download(), file, row.names = FALSE)
    }
  )
  
  read_result <- reactive({
    if(is.null(input$raw_data)){
      return(NULL)
    } else {
      # Read data
      raw_data <- suppressMessages(read_excel(input$raw_data$datapath)[-1,])
      
      col_names <- colnames(raw_data)
      
      col_names[1] <- "sample_type"
      col_names[2] <- "sample_name"
      
      for(i in 1:length(col_names)){
        if(i > 2){
          if(i%%3 == 0){
            metal_mode <- col_names[i]
            col_names[i] <- paste(metal_mode, "concentration_ppb")
          }
          if(i%%3 == 1){
            col_names[i] <- paste(metal_mode, "cps")
          }
          if(i%%3 == 2){
            col_names[i] <- paste(metal_mode, "rsd")
          }
          
        }
      }
      
      colnames(raw_data) <- col_names
      
      raw_data
    }
  })
  
  # Clean up calibration curve data
  clean_calibration_curve <- reactive({
    if(is.null(read_result())){
      return(NULL)
    } else {
      data <- read_result() |>
        filter(sample_type %in% c("CalBlk", "CalStd")) |>
        select(-sample_type) |>
        pivot_longer(cols = -sample_name,
                     names_to = "metal_mode",
                     values_to = "value") |> 
        mutate(value = suppressWarnings(as.numeric(value))) |>  
        mutate(type = str_extract(metal_mode, pattern = "concentration_ppb|cps|rsd")) |> 
        mutate(metal_mode = str_replace(metal_mode, pattern = "concentration_ppb|cps|rsd", replacement = "")) |> 
        pivot_wider(names_from = type, values_from = value) |> 
        mutate(expected_ppb = as.numeric(str_extract(sample_name, pattern = "\\d+(?= ppb)"))) %>% 
        mutate(metal = str_extract(metal_mode, pattern = "(?<=\\d  )[:alpha:]+"))
      
        data <- split(data, data$metal_mode) |>
        map_dfr(.f = ~ {
          .x |> 
            mutate(fit = summary(lm(concentration_ppb ~ expected_ppb, data = .))$r.squared)
        }) 
      
      data
    }
  })
  
  # clean up samples
  clean_samples <- reactive({
    if(is.null(read_result())){
      return(NULL)
    } else {
      data <- read_result() |>
        filter(sample_type %in% c("Sample")) |>
        select(-sample_type) |>
        pivot_longer(cols = -sample_name,
                     names_to = "metal_mode",
                     values_to = "value") |> 
        mutate(value = suppressWarnings(as.numeric(value))) |>  
        mutate(type = str_extract(metal_mode, pattern = "concentration_ppb|cps|rsd")) |> 
        mutate(metal_mode = str_replace(metal_mode, pattern = "concentration_ppb|cps|rsd", replacement = "")) |> 
        pivot_wider(names_from = type, values_from = value) |> 
        mutate(metal = str_extract(metal_mode, pattern = "(?<=\\d  )[:alpha:]+")) %>% 
        mutate(concentration_ppb = ifelse(is.na(concentration_ppb), 0, concentration_ppb)) %>% 
        group_by(sample_name, metal) %>% 
        mutate(outlier = concentration_ppb %in% boxplot.stats(concentration_ppb)$out) %>% 
        ungroup() 
      
      data
    }
  })
  
  # sample high rsd 
  sample_rsd <- reactive({
    if(is.null(clean_samples())){
      return(NULL)
    } else {
      data <- clean_samples() %>% 
        group_by(metal_mode) %>% 
        mutate(samples_high_rsd = sum(rsd > 2 | is.na(rsd), na.rm = TRUE),
               percent_samples_high_rsd = round(sum(rsd > 2 | is.na(rsd), na.rm = TRUE) / n() * 100, digits = 2)) %>% 
        distinct(samples_high_rsd, percent_samples_high_rsd, metal_mode)
      
      data
    }
  })
  
  assess_calibration_quality <- reactive({
    if(is.null(clean_calibration_curve()) | is.null(clean_samples())){
      return(NULL)
    } else {
    sample_outlier <- clean_samples() %>%
      distinct(sample_name, outlier, metal_mode) %>%
      group_by(metal_mode) %>%
      mutate(n_sample_outlier = sum(outlier, na.rm = TRUE),
             percent_sample_outlier = round((n_sample_outlier/n())*100, digits = 2)) %>%
      ungroup() %>%
      distinct(metal_mode, n_sample_outlier, percent_sample_outlier)
      
    data_quality <- clean_calibration_curve() %>% 
      select(-sample_name) %>% 
      filter(expected_ppb != 0) %>% 
      mutate(error = abs(expected_ppb - concentration_ppb)) %>% 
      group_by(expected_ppb) %>% 
      mutate(quant50 = quantile(error, na.rm = TRUE, probs = c(input$ppb_quantile_low)),
             quant75 = quantile(error, na.rm = TRUE, probs = c(input$ppb_quantile_mid)),
             quant90 = quantile(error, na.rm = TRUE, probs = c(input$ppb_quantile_high))) %>% 
      ungroup() %>% 
      mutate(quality_ppb = case_when(error <= quant50 ~ 2,
                                     error <= quant75 ~ 1,
                                     error <= quant90 ~ -1,
                                     TRUE ~ -2)) %>% 
      select(-c(quant50, quant75, quant90)) %>% 
      mutate(quant50 = quantile(unique(fit), na.rm = TRUE, probs = c(input$fit_quantile_high)),
             quant25 = quantile(unique(fit), na.rm = TRUE, probs = c(input$fit_quantile_mid)),
             quant10 = quantile(unique(fit), na.rm = TRUE, probs = c(input$fit_quantile_low))) %>% 
      mutate(quality_fit = case_when(fit >= quant50 ~ 10,
                                     fit >= quant25 ~ 5,
                                     fit >= quant10 ~ -5,
                                     TRUE ~ -10)) %>% 
      select(-c(quant50, quant25, quant10)) %>% 
      mutate(quality_cps = ifelse(cps < input$cps_low | cps > input$cps_high | is.na(cps), -2, 2)) %>% 
      mutate(quality_rsd = case_when(rsd <= input$rsd_quality[1] ~ 2,
                                     rsd <= input$rsd_quality[2] ~ 1,
                                     TRUE ~ -2)) %>% 
      group_by(metal_mode) %>% 
      mutate(quality = sum(quality_ppb, quality_rsd, quality_cps, unique(quality_fit))) %>% 
      ungroup() %>% 
      select(-c(concentration_ppb, cps, rsd, fit, error)) %>% 
      left_join(sample_rsd(), by = "metal_mode") %>% 
      left_join(sample_outlier, by = "metal_mode")
    
    data_quality
    }
  })
  
  # select metal mode
  select_metal_mode <- reactive({
    if(is.null(clean_calibration_curve())){
      return(NULL)
    } else {
      distinct_data <- clean_calibration_curve() %>% 
        distinct(metal_mode, metal)
      
      modes <- split(distinct_data$metal_mode, distinct_data$metal)
    }
  })
  observe({
    updateSelectizeInput(session, "metal_mode", choices = select_metal_mode(), server = TRUE)
  })
  
  # select metal
  select_metal <- reactive({
    if(is.null(clean_calibration_curve())){
      return(NULL)
    } else {
      metals <- unique(clean_calibration_curve()$metal)
    }
  })
  observe({
    updateSelectizeInput(session, "metal", choices = select_metal(), server = TRUE)
  })
  
  # select best metal modes 
  select_best_metal_modes <- reactive({
    if(is.null(assess_calibration_quality())){
      return(NULL)
    } else {
      metal_modes <- assess_calibration_quality() %>% 
        filter(metal == input$metal) %>% 
        pull(metal_mode) %>% 
        unique()
    }
  })
  observe({
    input$parameters_input
    if(!is.null(list_best_metal_modes[[input$metal]]) && list_best_metal_modes[[input$metal]] != ""){
      selected_modes <- list_best_metal_modes[[input$metal]]
    } else {
      selected_modes <- NULL
    }
    updateSelectizeInput(session, "best_metal_mode", choices = select_best_metal_modes(), selected = selected_modes, server = TRUE)
  })
  
  # Make header
  output$metal_title <- renderText({
    if(is.null(select_metal())){
      return(NULL)
    } else {
      paste("<h1>", input$metal, "</h1>")
    }
  })
  
  # metal mode selection
  observe({
    # observe events in
    input$best_metal_mode
    input$metal
    # make initial list
    if(start == 1){
    print("Metal list created")
    list_best_metal_modes <<- vector(mode = "list", length = length(select_metal()))
    names(list_best_metal_modes) <<- select_metal()
    }

    # modify list
    if(start > 1){
        print("Metal list modified")
      if(is.null(input$best_metal_mode)){
        list_best_metal_modes[[input$metal]] <<- ""
      } else {
        list_best_metal_modes[[input$metal]] <<- input$best_metal_mode
      }
    }
    
    print(list_best_metal_modes)
    
    # Update counter for inital list
    start <<- start + 1
  })
  
  output$list_best_metals <- renderPrint({
    if(is.null(input$best_metal_mode) & input$metal == ""){
      return("Please load data!")
    } else {
      glimpse(list_best_metal_modes)
    }
  })
  
  # Make cuve plot
  output$curve_plot <- renderPlotly({
    if(is.null(select_metal_mode())){
      return(NULL)
    } else {
      data_metal_mode <- clean_calibration_curve() |> 
        filter(metal_mode == input$metal_mode)
      
      plot <- data_metal_mode |> 
        ggplot(aes(x = expected_ppb, y = concentration_ppb))+
        geom_point(size = 2)+
        geom_smooth(formula = y ~ x,
                    method = "lm", size = 1, col = protti_colours[1])+
        labs(title = input$metal_mode, x = "Expected Concentration [ppb]", y = "Measured Concentration [ppb]")+
        theme_bw() +
        theme(plot.title = ggplot2::element_text(size = 20),
              axis.title.x = ggplot2::element_text(size = 15),
              axis.text.y = ggplot2::element_text(size = 15),
              axis.text.x = ggplot2::element_text(size = 12),
              axis.title.y = ggplot2::element_text(size = 15),
              legend.title = ggplot2::element_text(size = 15),
              legend.text = ggplot2::element_text(size = 15),
              strip.text.x = ggplot2::element_text(size = 15),
              strip.text = ggplot2::element_text(size = 15),
              strip.background = element_blank()
        )

      ggplotly(plot) %>% 
        add_annotations(
          x= 0.5,
          y= max(data_metal_mode$expected_ppb, na.rm = TRUE),
          xanchor = "left",
          text = paste0("<b>R<sup>2</sup>: ", unique(data_metal_mode$fit), "</b>"),
          showarrow = F
        )
    }
  })
  
  # Make error plot
  output$error_plot <- renderPlotly({
    if(is.null(clean_calibration_curve())){
      return(NULL)
    } else {
      plot <- clean_calibration_curve() |> 
        mutate(error = abs(expected_ppb - concentration_ppb)) %>% 
        mutate(expected_ppb = forcats::fct_reorder(as.character(expected_ppb),expected_ppb)) %>% 
        ggplot(aes(expected_ppb, error))+
        geom_boxplot()+
        labs(title = "Error distribution", x = "Expected Concentration [ppb]", y = "Measurment error [ppb]")+
        theme_bw() +
        theme(plot.title = ggplot2::element_text(size = 20),
              axis.title.x = ggplot2::element_text(size = 15),
              axis.text.y = ggplot2::element_text(size = 15),
              axis.text.x = ggplot2::element_text(size = 12),
              axis.title.y = ggplot2::element_text(size = 15),
              legend.title = ggplot2::element_text(size = 15),
              legend.text = ggplot2::element_text(size = 15),
              strip.text.x = ggplot2::element_text(size = 15),
              strip.text = ggplot2::element_text(size = 15),
              strip.background = element_blank()
        )
      
      ggplotly(plot)
    }
  })
  
  # Make r square plot
  output$fit_plot <- renderPlotly({
    if(is.null(clean_calibration_curve())){
      return(NULL)
    } else {
      plot <- clean_calibration_curve() |> 
        distinct(fit) %>% 
        ggplot(aes(x = "All curve fits", y = fit))+
        geom_boxplot()+
        labs(title = "R<sup>2</sup> Distribution", x = "", y = "R<sup>2</sup>")+
        theme_bw() +
        theme(plot.title = ggplot2::element_text(size = 20),
              axis.title.x = ggplot2::element_text(size = 15),
              axis.text.y = ggplot2::element_text(size = 15),
              axis.text.x = ggplot2::element_text(size = 12),
              axis.title.y = ggplot2::element_text(size = 15),
              legend.title = ggplot2::element_text(size = 15),
              legend.text = ggplot2::element_text(size = 15),
              strip.text.x = ggplot2::element_text(size = 15),
              strip.text = ggplot2::element_text(size = 15),
              strip.background = element_blank()
        )
      
      ggplotly(plot) %>% 
        layout(yaxis = list(hoverformat = '.6f')) 
    }
  })
  
  # Make table
  output$curve_table <- renderFormattable({
    if(is.null(select_metal_mode())){
      return(NULL)
    } else {
      clean_calibration_curve() |>
        mutate(error = abs(expected_ppb - concentration_ppb)) %>% 
        group_by(expected_ppb) %>% 
        mutate(quant50 = quantile(error, na.rm = TRUE, probs = c(input$ppb_quantile_low)),
               quant75 = quantile(error, na.rm = TRUE, probs = c(input$ppb_quantile_mid)),
               quant90 = quantile(error, na.rm = TRUE, probs = c(input$ppb_quantile_high))) %>% 
        ungroup() %>% 
        mutate(quality = case_when(error <= quant50 ~ "good",
                                   error <= quant75 ~ "okay",
                                   error <= quant90 ~ "not good",
                                   TRUE ~ "bad")) %>% 
        filter(metal_mode == input$metal_mode) |> 
        select(expected_ppb, concentration_ppb, cps, rsd, quality) |> 
        formattable(list(
          quality = FALSE,
          cps = formatter("span", style = x ~ ifelse(x > input$cps_high | x < input$cps_low | is.na(x),
                                                       style(color = "red", font.weight = "bold"), NA)),
          rsd = formatter("span", style = x ~ case_when(x > input$rsd_quality[2] ~ style(color = "red", font.weight = "bold"),
                                                        x <= input$rsd_quality[1] ~ style(color = "green", font.weight = "bold"),
                                                        x <= input$rsd_quality[2] ~ style(color = "#c4e84d", font.weight = "bold"))),
          concentration_ppb = formatter("span", style = ~ case_when(
            quality == "bad" ~ style(color = "red", font.weight = "bold"),
            quality == "not good" ~ style(color = "#e38c29", font.weight = "bold"),
            quality == "okay" ~ style(color = "#c4e84d", font.weight = "bold"),
            quality == "good" ~ style(color = "green", font.weight = "bold")
                                                                    ))
        ))
    }
  })
  
  # Make metal mode quality table
  output$quality_table <- renderFormattable({
    if(is.null(assess_calibration_quality())){
      return(NULL)
    } else {
      assess_calibration_quality() %>% 
        filter(metal == input$metal) |>
        arrange(desc(quality)) %>% 
        formattable(list(
          metal = FALSE,
          quality_ppb = formatter("span", style = x ~ case_when(x == 2 ~ style(color = "green", font.weight = "bold"),
                                                                x == 1 ~ style(color = "#c4e84d", font.weight = "bold"),
                                                                x == -1 ~ style(color = "#e38c29", font.weight = "bold"),
                                                                x == -2 ~ style(color = "red", font.weight = "bold"))),
          quality_cps = formatter("span", style = x ~ case_when(x == 2 ~ style(color = "green", font.weight = "bold"),
                                                                x == 1 ~ style(color = "#c4e84d", font.weight = "bold"),
                                                                x == -1 ~ style(color = "#e38c29", font.weight = "bold"),
                                                                x == -2 ~ style(color = "red", font.weight = "bold"))),
          quality_rsd = formatter("span", style = x ~ case_when(x == 2 ~ style(color = "green", font.weight = "bold"),
                                                                x == 1 ~ style(color = "#c4e84d", font.weight = "bold"),
                                                                x == -1 ~ style(color = "#e38c29", font.weight = "bold"),
                                                                x == -2 ~ style(color = "red", font.weight = "bold"))),
          quality_fit = formatter("span", style = x ~ case_when(x == 10 ~ style(color = "green", font.weight = "bold"),
                                                                x == 5 ~ style(color = "#c4e84d", font.weight = "bold"),
                                                                x == -5 ~ style(color = "#e38c29", font.weight = "bold"),
                                                                x == -10 ~ style(color = "red", font.weight = "bold"))),
          n_sample_outlier = formatter("span", style = x ~ case_when(x >= 1 ~ style(color = "red", font.weight = "bold"),
                                                                TRUE ~ style(color = "green", font.weight = "bold"))),
          percent_sample_outlier = formatter("span", style = x ~ case_when(x > 0 ~ style(color = "red", font.weight = "bold"),
                                                                     TRUE ~ style(color = "green", font.weight = "bold")))
          )
        )
    }
  })
  
  # Make sample table 
  output$sample_table <- DT::renderDataTable({
    if(is.null(clean_samples()) & is.null(input$best_metal_mode)){
      return(NULL)
    } else {
      clean_samples() %>% 
        filter(metal_mode %in% unlist(list_best_metal_modes)) %>% 
        formattable(list(
          rsd = formatter("span", style = x ~ case_when(x > 2 ~ style(color = "red", font.weight = "bold"),
                                                        x <= 1 ~ style(color = "green", font.weight = "bold"),
                                                        x <= 2 ~ style(color = "#c4e84d", font.weight = "bold")))
        )
        ) %>% 
        formattable::as.datatable()
    }
  })
  
  # Plot ppb distribution per sample and metal
  output$ppb_sample_distribution <- renderPlotly({
    if(is.null(clean_samples())){
      return(NULL)
    } else {
      plot <- clean_samples() %>% 
        filter(metal == input$metal) |>
        ggplot(aes(x = sample_name, y = concentration_ppb, col = outlier, label = metal_mode))+
        geom_jitter(width = 0.15)+
        labs(title = "ppb Distribution", x = "", y = "Concentration [ppb]")+
        scale_color_manual(values=c("black", "red"))+
        theme_bw() +
        theme(plot.title = ggplot2::element_text(size = 20),
              axis.title.x = ggplot2::element_text(size = 15),
              axis.text.y = ggplot2::element_text(size = 15),
              axis.text.x = ggplot2::element_text(size = 12, angle = 75, hjust = 1),
              axis.title.y = ggplot2::element_text(size = 15),
              legend.title = ggplot2::element_text(size = 15),
              legend.text = ggplot2::element_text(size = 15),
              strip.text.x = ggplot2::element_text(size = 15),
              strip.text = ggplot2::element_text(size = 15),
              strip.background = element_blank()
        )
      
      ggplotly(plot, height = 600) 
    }
      
  })
  ##### Sample properties
  # update sample selection
  observe({
    sample_names <<- unique(clean_samples()$sample_name)
    
    updateSelectizeInput(session, "sample", choices = sample_names, server = TRUE)
  })
  
  # Add samples to dataframe
  observeEvent(input$add_sample_properties, {
    new_entry <- data.frame(dilution_factor = input$dilution_factor,
                            weight = ifelse(input$weight == 0, NA, input$weight),
                            sample_name = paste0(input$sample, collapse = ","))
    
    sample_properties_df <<- sample_properties_df %>% 
      bind_rows(new_entry)
    
    sample_names <<- sample_names[!sample_names %in% input$sample]
    updateSelectizeInput(session, "sample", choices = sample_names, server = TRUE)
  })
  
  # Reset sample dataframe
  observeEvent(input$reset_sample_properties, {
    sample_properties_df <<- data.frame(dilution_factor = c(),
                                        weight = c(),
                                        sample_name = c())
    
    sample_names <<- unique(clean_samples()$sample_name)
    updateSelectizeInput(session, "sample", choices = sample_names, server = TRUE)
  })
  
  # return dataframe
  output$sample_properties <- renderPrint({
    input$add_sample_properties
    input$reset_sample_properties
    input$parameters_input
    print(str(sample_properties_df))
  })
  
  #### Specify expected values
  # update metal selection
  observe({
    updateSelectizeInput(session, "metal_expected", choices = unique(clean_samples()$metal), server = TRUE)
  })
  
  # update sample selection
  observe({
    if (!is.null(clean_samples()) & input$metal_expected == ""){
      sample_names_expected_df <<- clean_samples() %>% 
        distinct(metal, sample_name)
    }
    
    if (input$metal_expected != ""){
      sample_names_subset <- sample_names_expected_df %>% 
        filter(metal == input$metal_expected) %>% 
        pull(sample_name)
    } else {
      sample_names_subset <- NULL
    }
    
    updateSelectizeInput(session, "sample_expected", choices = sample_names_subset, server = TRUE)
  })
  
  # Add samples to expected dataframe
  observeEvent(input$add_sample_expected, {
    new_entry <- data.frame(metal = input$metal_expected,
                            expected_ppb = ifelse(input$value_expected == 0 | input$unit_expected == "uM", NA, input$value_expected),
                            expected_uM = ifelse(input$value_expected == 0 | input$unit_expected == "ppb", NA, input$value_expected),
                            sample_name = paste0(input$sample_expected, collapse = ","))

    sample_expected_df <<- sample_expected_df %>% 
      bind_rows(new_entry)
    
    sample_names_expected_df <<- sample_names_expected_df %>% 
      filter(!(metal == input$metal_expected & sample_name %in% input$sample_expected))
    
    if (input$metal_expected != ""){
      sample_names_subset <- sample_names_expected_df %>% 
        filter(metal == input$metal_expected) %>% 
        pull(sample_name)
    } else {
      sample_names_subset <- NULL
    }

    updateSelectizeInput(session, "sample_expected", choices = sample_names_subset, server = TRUE)
  })
  
  # Reset sample expected dataframe
  observeEvent(input$reset_sample_expected, {
    sample_expected_df <<- data.frame(metal = c(),
                                        expected_ppb = c(),
                                        expected_uM = c(),
                                        sample_name = c())
    
    sample_names_expected_df <<- clean_samples() %>% 
      distinct(metal, sample_name)
    
    if (input$metal_expected != ""){
      sample_names_subset <- sample_names_expected_df %>% 
        filter(metal == input$metal_expected) %>% 
        pull(sample_name)
    } else {
      sample_names_subset <- NULL
    }
    
    updateSelectizeInput(session, "sample_expected", choices = sample_names_subset, server = TRUE)
  })
  
  # return dataframe
  output$sample_expected_output <- renderPrint({
    input$add_sample_expected
    input$reset_sample_expected
    input$parameters_input
    print(str(sample_expected_df))
  })
  
  # create result table 
  calculate_result <- reactive({
    input$add_sample_properties
    input$reset_sample_properties
    input$add_sample_expected
    input$reset_sample_expected
    input$parameters_input
    if(is.null(clean_samples()) | nrow(sample_properties_df) == 0){
      return(NULL)
    } else {
      
      sample_properties_df_prepared <- sample_properties_df %>% 
        mutate(sample_name = str_split(sample_name, pattern = ",")) %>% 
        unnest(sample_name)
      
      if (nrow(sample_expected_df) != 0){
        sample_expected_df_prepared <- sample_expected_df %>% 
          mutate(sample_name = str_split(sample_name, pattern = ",")) %>% 
          unnest(sample_name)
      } else {
        sample_expected_df_prepared <- sample_expected_df %>% 
          mutate(sample_name = "",
                 metal = "",
                 expected_ppb = "",
                 expected_uM = "")
      }
      
      clean_samples() %>% 
        filter(metal_mode %in% unlist(list_best_metal_modes)) %>%  # select only the chosen metal modes
        filter(sample_name %in% sample_properties_df_prepared$sample_name) %>% 
        select(sample_name, metal, metal_mode, concentration_ppb, rsd) %>% 
        left_join(sample_properties_df_prepared, by = "sample_name") %>% 
        left_join(sample_expected_df_prepared, by = c("sample_name", "metal")) %>% 
        mutate(undiluted_concentration_ppb = ifelse(is.na(weight), 
                                                    concentration_ppb * dilution_factor,
                                                    (concentration_ppb * dilution_factor) / weight)) %>% 
        left_join(metal_mw, by = "metal") %>% 
        mutate(expected_ppb = ifelse(is.na(expected_ppb) & is.na(expected_uM), 
                                     NA,
                                     ifelse(is.na(expected_ppb),
                                            round(as.numeric(expected_uM) * mw, digits = 5),
                                            expected_ppb)
                                     )) %>%
        mutate(expected_uM = ifelse(is.na(expected_ppb) & is.na(expected_uM), 
                                    NA,
                                    ifelse(is.na(expected_uM),
                                           round(as.numeric(expected_ppb) / mw, digits = 5),
                                           expected_uM)
                                    )) %>%
        mutate(undiluted_concentration_uM = ifelse(is.na(weight), 
                                                   round(undiluted_concentration_ppb / mw, digits = 5),
                                                   NA)) %>% 
        select(-mw)
    }
  })
  
  # return table with sample concentrations
  output$result <- DT::renderDataTable({
    input$add_sample_properties
    input$add_sample_expected
    input$reset_sample_properties
    input$reset_sample_expected
    input$parameters_input
    if(is.null(clean_samples()) | nrow(sample_properties_df) == 0){
      return(NULL)
    } else {
      calculate_result() %>% 
        formattable(list(
          rsd = formatter("span", style = x ~ case_when(x > 2 ~ style(color = "red", font.weight = "bold"),
                                                        x <= 1 ~ style(color = "green", font.weight = "bold"),
                                                        x <= 2 ~ style(color = "#c4e84d", font.weight = "bold")))
        )) %>% 
        formattable::as.datatable()
    }
  })
  
  # Download result
  output$download_result <- downloadHandler(
    filename = function() {
      paste0("result_", Sys.time(), ".csv")
    },
    content = function(file) {
      write.csv(calculate_result(), file, row.names = FALSE) 
    }
  )
  
  # Update plot sample selection
  observe({
    updateSelectizeInput(session, "sample_plot", choices = unique(clean_samples()$sample_name), server = TRUE)
  })
  
  # Plot result
  create_plot <- reactive({
    if(is.null(calculate_result()) | is.null(input$sample_plot) | is.null(input$colour)){
      return(NULL)
    } else {
      
      if (input$plot_expected == FALSE){
        plot_output <- calculate_result() %>% 
          filter(sample_name %in% input$sample_plot) %>% 
          mutate(sample_name = fct_relevel(as.factor(sample_name), input$sample_plot)) %>% 
          mutate(concentration = ifelse(rep(input$plot_type == "Concentration in ppb", n()),
                                        undiluted_concentration_ppb * input$multiply_concentration,
                                        undiluted_concentration_uM * input$multiply_concentration)) %>%
          mutate(sd = rsd * concentration / 100) %>% 
          ggplot(aes(sample_name, concentration))+
          geom_col(fill = input$colour[1])+
          geom_errorbar(aes(ymin = concentration-sd, ymax=concentration+sd), 
                        width = 0.2, 
                        size = 1)+
          labs(title = input$plot_title, x = input$x_axis, y = input$y_axis) +
          facet_wrap(~metal_mode, ncol = 4, scale = "free")+
          scale_fill_manual(values = input$colour) +
          theme_bw() +
          theme(plot.title = ggplot2::element_text(size = 20),
                axis.title.x = ggplot2::element_text(size = 15),
                axis.text.y = ggplot2::element_text(size = 15),
                axis.text.x = ggplot2::element_text(size = 12, angle = 75, hjust = 1),
                axis.title.y = ggplot2::element_text(size = 15),
                legend.title = ggplot2::element_text(size = 15),
                legend.text = ggplot2::element_text(size = 15),
                strip.text.x = ggplot2::element_text(size = 15),
                strip.text = ggplot2::element_text(size = 15),
                strip.background = element_blank()
          )
      } 
      if (input$plot_expected == TRUE) {
        plot_output <- calculate_result() %>% 
          filter(sample_name %in% input$sample_plot) %>% 
          mutate(sample_name = fct_relevel(as.factor(sample_name), input$sample_plot)) %>% 
          mutate(concentration = ifelse(rep(input$plot_type == "Concentration in ppb", n()),
                                        undiluted_concentration_ppb * input$multiply_concentration,
                                        undiluted_concentration_uM * input$multiply_concentration)) %>%
          mutate(concentration_expected = ifelse(rep(input$plot_type == "Concentration in ppb", n()),
                                                 expected_ppb * input$multiply_concentration,
                                                 expected_uM * input$multiply_concentration)) %>% 
          pivot_longer(c(concentration_expected, concentration), names_to = "Category", values_to = "concentration") %>% 
          mutate(rsd = ifelse(Category == "concentration_expected", NA, rsd)) %>% 
          mutate(Category = ifelse(Category == "concentration_expected", "Expected concentration", "Measured concentration")) %>% 
          mutate(sd = rsd * concentration / 100) %>% 
          ggplot(aes(x = sample_name, y = concentration, fill = Category))+
          geom_bar(stat="identity", 
                   color="black", 
                   position=position_dodge(),
                   na.rm = TRUE) +
          geom_errorbar(aes(ymin = concentration-sd, ymax=concentration+sd), 
                        width = 0.2, 
                        size = 1,
                        position = position_dodge(0.9)) +
          labs(title = input$plot_title, x = input$x_axis, y = input$y_axis) +
          facet_wrap(~metal_mode, ncol = 4, scale = "free")+
          scale_fill_manual(values = input$colour) +
          theme_bw() +
          theme(plot.title = ggplot2::element_text(size = 20),
                axis.title.x = ggplot2::element_text(size = 15),
                axis.text.y = ggplot2::element_text(size = 15),
                axis.text.x = ggplot2::element_text(size = 12, angle = 75, hjust = 1),
                axis.title.y = ggplot2::element_text(size = 15),
                legend.title = ggplot2::element_text(size = 15),
                legend.text = ggplot2::element_text(size = 15),
                strip.text.x = ggplot2::element_text(size = 15),
                strip.text = ggplot2::element_text(size = 15),
                strip.background = element_blank()
          )
      }
    }
    plot_output
  })
  
  output$result_plot <- renderPlot({
    create_plot()
  },
  res = 72)
  
  # Update plot height
  output$plot.ui <- renderUI({
    plotOutput("result_plot", height = input$plot_height, width = input$plot_width)
  })
  
  # Plot download
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0(input$plot_title, Sys.time(), ".png")
    },
    content = function(file){
      ggsave(file, plot = create_plot(), device = "png", height = input$plot_height * 4, width = input$plot_width * 4, units = "px")
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
