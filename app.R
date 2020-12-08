
# Required libraries
library(shiny)
library(ggplot2)
library(dplyr)

# Helper files
source(here::here("Scripts", "002_create_plots.R"))

# Data
shades_values = readr::read_rds(here::here("Data",
                                           "shade_values.rds"))
shade_palette = readr::read_rds(here::here("Data",
                                           "shade_palette.rds"))


# Shiny App UI
ui = fluidPage(

  
  # Title
  titlePanel(
    title = h1("What's in a shade? An exploration of the color composition of foundation shades",
               align = "center"),
    windowTitle = "Whats in a shade?"),
  
  fluidRow(
    
    column(12,
           align = 'center',
           wellPanel(
             plotOutput("plot", 
                        width = "800px",
                        click = "plot_click"),
             br()
           )
        )
  ),
  
  fluidRow(
    
    column(4,
           wellPanel(
             selectInput("brand_name",
                         label = "Brand Name",
                         choices = c("all brands", sort(unique(shades_values$brand)))),
             
             selectInput("shade_type",
                         label = "Shade Color Composition", 
                         choices = colnames(shades_values)[6:9],
                         selected = colnames(shades_values)[6])
           )
        ),
    
    column(8,
           wellPanel(
             tabsetPanel(id = "info_tabs",
                         type = "tabs",
                         # Static background tab
                         tabPanel("Background", 
                                  includeHTML(here::here("Text", "background_text.html"))),
                         
                         # tabs to dynamically appear
                         tabPanel(title = "Hue Information",
                                  value = 1,
                                  includeHTML(here::here("Text", "hue.html"))),
                         tabPanel(title = "Saturation Information",
                                  value = 2,
                                  includeHTML(here::here("Text", "saturation.html"))),
                         tabPanel(title = "Brightness Information",
                                  value = 3,
                                  includeHTML(here::here("Text", "brightness.html"))),
                         tabPanel(title = "Lightness Information",
                                  value = 4,
                                  includeHTML(here::here("Text", "lightness.html"))),
                         tabPanel(title = "Foundation Shade",
                                  value = 5,
                                  htmlOutput("info_plot_click")))
           )
          )
  )
)
  

# Shiny App Server
server <- function(input, output, session) {
  
   #Limiting data to UI selections
  plot_data = reactive({
    filter_df(shades_values,
              input$brand_name,
              input$shade_type)
  })
  
  
  # Outputting plot based off of UI input
    output$plot = renderPlot({
      
      generate_plot(plot_data(),
                    shade_palette,
                    input$shade_type)
    })
  
  
  
  # Dynamically show tab based on selection
  observeEvent(input$shade_type, {
    
    if(input$shade_type == "Hue"){
      showTab(inputId = "info_tabs", target = "1")
      hideTab(inputId = "info_tabs", target = "2")
      hideTab(inputId = "info_tabs", target = "3")
      hideTab(inputId = "info_tabs", target = "4")
      
    } else if(input$shade_type == "Saturation"){
      
      showTab(inputId = "info_tabs", target = "2")
      hideTab(inputId = "info_tabs", target = "1")
      hideTab(inputId = "info_tabs", target = "3")
      hideTab(inputId = "info_tabs", target = "4")         
      
    } else if(input$shade_type == "Brightness"){
      
      showTab(inputId = "info_tabs", target = "3")
      hideTab(inputId = "info_tabs", target = "1")
      hideTab(inputId = "info_tabs", target = "2")
      hideTab(inputId = "info_tabs", target = "4")  
      
    } else if(input$shade_type == "Lightness"){
      showTab(inputId = "info_tabs", target = "4")
      hideTab(inputId = "info_tabs", target = "1")
      hideTab(inputId = "info_tabs", target = "2")
      hideTab(inputId = "info_tabs", target = "3")  
      
    }
    
    
  })
  
  # Shade information for each click
  output$info_plot_click = renderPrint({
    
    shade_info_str = function(clicked_df){
      
      details = if(nrow(clicked_df) == 0){
        p(tags$b("Brand: "), br(),
          tags$b("Product: "), br(),
          tags$b("Hexadecimal: "), br(),
          tags$b("Hue: "), br(),
          tags$b("Brightness: "), br(),
          tags$b("Saturation: "), br(),
          tags$b("Lightness: "), br()) }
      else if(nrow(clicked_df) >= 1){
        p(tags$b("Brand: "), tags$em(clicked_df$brand), br(),
          tags$b("Product: "), tags$em(clicked_df$product), br(),
          tags$b("Hexadecimal: "), tags$em(clicked_df$hex), br(),
          tags$b("Hue: "), tags$em(clicked_df$Hue), br(),
          tags$b("Brightness: "), tags$em(clicked_df$Brightness), br(),
          tags$b("Saturation: "), tags$em(clicked_df$Saturation), br(),
          tags$b("Lightness: "), tags$em(clicked_df$Lightness))
      }
      else { NA }
      return(details)
    }
    
  shade_info_str(nearPoints(plot_data(),
                            coordinfo = input$plot_click,
                            threshold = 2,
                            addDist = T) %>%
                   arrange(dist_) %>%
                   slice(1) %>%
                 select(brand, product, hex, Hue,
                        Saturation, Brightness, Lightness))
    
    
  })
  
  
  # Creating summary below
  min_max_val = reactive({
    create_summary_keys(plot_data(), input$shade_type)
  })
  
  perc_coverage = reactive({
    (min_max_val()[[2]] - min_max_val()[[1]])
  })
  
  number_shades = reactive({
    nrow(plot_data())
  })
  

    
} 
  

shinyApp(ui, server)
