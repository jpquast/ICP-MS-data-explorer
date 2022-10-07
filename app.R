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

# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage("ICP-MS Calibration Curve explorer",
             tabPanel("Calibration overview",
                      sidebarPanel(
                        fileInput("raw_data",
                                  "ICP-MS raw data")
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
                 sliderInput("ppb_quantile_low", 'Quantile for "good" (below) ppb quality', min = 0, max = 1, value = 0.5, step = 0.05),
                 sliderInput("ppb_quantile_mid", 'Quantile for "okay" (below) ppb quality', min = 0, max = 1, value = 0.75, step = 0.05),
                 sliderInput("ppb_quantile_high", 'Quantile for "not good" (below) and "bad" (above) ppb quality', min = 0, max = 1, value = 0.9, step = 0.05),
                 br(),
                 h4("fit quality"),
                 sliderInput("fit_quantile_high", 'Quantile for "good" (above) fit quality', min = 0, max = 1, value = 0.5, step = 0.05),
                 sliderInput("fit_quantile_mid", 'Quantile for "okay" (above) fit quality', min = 0, max = 1, value = 0.25, step = 0.05),
                 sliderInput("fit_quantile_low", 'Quantile for "not good" (above) and "bad" (below) fit quality', min = 0, max = 1, value = 0.1, step = 0.05),
                 br(),
                 h4("cps quality"),
                 numericInput("cps_low", "The lower cps bound", value = 1000),
                 numericInput("cps_high", "The higher cps bound", value = 100000000),
                 br(),
                 h4("rsd quality"),
                 sliderInput("rsd_quality", 'rsd quality for "good" (below first), "okay" (below second) and "bad" (above second)', 
                             min = 0,
                             max = 10,
                             value = c(1, 2),
                             step = 0.1)
               ),
               mainPanel(
                 width = 7,
                 htmlOutput("metal_title"),
                 br(),
                 formattableOutput("quality_table")
               )
             ),
             tabPanel("Experiment overview",
                      sidebarPanel(
                      ),
                      mainPanel(
                        width = 7
                      )
             ),
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  read_input <- reactive({
    if(is.null(input$raw_data)){
      return(NULL)
    } else {
      # Read data
      raw_data <- read_excel(input$raw_data$datapath)[-1,]
      
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
      
      data <- raw_data |>
        filter(sample_type %in% c("CalBlk", "CalStd")) |>
        select(-sample_type) |>
        pivot_longer(cols = -sample_name,
                     names_to = "metal_mode",
                     values_to = "value") |> 
        mutate(value = as.numeric(value)) |>  
        mutate(type = str_extract(metal_mode, pattern = "concentration_ppb|cps|rsd")) |> 
        mutate(metal_mode = str_replace(metal_mode, pattern = "concentration_ppb|cps|rsd", replacement = "")) |> 
        pivot_wider(names_from = type, values_from = value) |> 
        mutate(expected_ppb = as.numeric(str_extract(sample_name, pattern = "\\d+(?= ppb)"))) 
      
        data <- split(data, data$metal_mode) |>
        map_dfr(.f = ~ {
          .x |> 
            mutate(fit = summary(lm(concentration_ppb ~ expected_ppb, data = .))$r.squared)
        }) %>% 
        mutate(metal = str_extract(metal_mode, pattern = "(?<=\\d  )[:alpha:]+"))
      
      data
    }
  })
  
  assess_calibration_quality <- reactive({
    if(is.null(read_input())){
      return(NULL)
    } else {
    data_quality <- read_input() %>% 
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
      select(-c(concentration_ppb, cps, rsd, fit, error))
    
    data_quality
    }
  })
  
  # select metal mode
  select_metal_mode <- reactive({
    if(is.null(read_input())){
      return(NULL)
    } else {
      distinct_data <- read_input() %>% 
        distinct(metal_mode, metal)
      
      modes <- split(distinct_data$metal_mode, distinct_data$metal)
    }
  })
  observe({
    updateSelectizeInput(session, "metal_mode", choices = select_metal_mode(), server = TRUE)
  })
  
  # select metal
  select_metal <- reactive({
    if(is.null(assess_calibration_quality())){
      return(NULL)
    } else {
      metals <- unique(assess_calibration_quality()$metal)
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
    updateSelectizeInput(session, "best_metal_mode", choices = select_best_metal_modes(), server = TRUE)
  })
  
  # Make header
  output$metal_title <- renderText({
    if(is.null(select_metal())){
      return(NULL)
    } else {
      paste("<h1>", input$metal, "</h1>")
    }
  })
  
  # Make cuve plot
  output$curve_plot <- renderPlotly({
    if(is.null(select_metal_mode())){
      return(NULL)
    } else {
      data_metal_mode <- read_input() |> 
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
    if(is.null(read_input())){
      return(NULL)
    } else {
      plot <- read_input() |> 
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
    if(is.null(read_input())){
      return(NULL)
    } else {
      plot <- read_input() |> 
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
      read_input() |>
        mutate(error = abs(expected_ppb - concentration_ppb)) %>% 
        group_by(expected_ppb) %>% 
        mutate(quant50 = quantile(error, na.rm = TRUE, probs = c(0.5)),
               quant75 = quantile(error, na.rm = TRUE, probs = c(0.75)),
               quant90 = quantile(error, na.rm = TRUE, probs = c(0.9))) %>% 
        ungroup() %>% 
        mutate(quality = case_when(error <= quant50 ~ "good",
                                   error <= quant75 ~ "okay",
                                   error <= quant90 ~ "not good",
                                   TRUE ~ "bad")) %>% 
        filter(metal_mode == input$metal_mode) |> 
        select(expected_ppb, concentration_ppb, cps, rsd, quality) |> 
        formattable(list(
          quality = FALSE,
          cps = formatter("span", style = x ~ ifelse(x > 100000000 | x < 1000 | is.na(x),
                                                       style(color = "red", font.weight = "bold"), NA)),
          rsd = formatter("span", style = x ~ case_when(x > 2 ~ style(color = "red", font.weight = "bold"),
                                                        x <= 1 ~ style(color = "green", font.weight = "bold"),
                                                        x <= 2 ~ style(color = "#c4e84d", font.weight = "bold"))),
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
                                                                x == -10 ~ style(color = "red", font.weight = "bold"))))
        )
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
