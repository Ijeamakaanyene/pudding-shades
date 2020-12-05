library(shiny)
library(ggplot2)
library(dplyr)

# Data
shades_values = readr::read_rds(here::here("Data",
                                           "shade_values.rds"))
shade_palette = readr::read_rds(here::here("Data",
                                           "shade_palette.rds"))

# Reading in functions
source(here::here("Scripts", "002_create_plots.R"))


# Shiny App UI
ui = fluidPage(
  
  # Title
  titlePanel(
    title = h1("What's in a shade? An exploration of the color composition of foundation shades",
               align = "center"),
    windowTitle = "Whats in a shade?"),
  
  # Sidebar of inputs
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("brand_name",
                  label = "Brand Name",
                  choices = c("all brands", sort(unique(shades_values$brand)))),
      
      selectInput("shade_type",
                  label = "Shade Color Composition", 
                  choices = colnames(shades_values)[6:9],
                  selected = colnames(shades_values)[6]),
      
      actionButton("show", "Generate Information")
      
    ),
    
    # Main panel of plots and information
    mainPanel(
      plotOutput("plot", 
                 width = "800px",
                 click = "plot_click"),
      br(),
      
      # tabset panel showing information about color 
      tabsetPanel(id = "info_tabs",
                  type = "tabs",
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
                  
                  # Static Tabs
                  tabPanel("Summary", textOutput("summary")),
                  tabPanel("Shade Information", textOutput("click_info")))
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
  
  
  
  # Dynamically show tab
  observeEvent(input$show, {
    
    
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
      
    } else {
      showTab(inputId = "info_tabs", target = "4")
      hideTab(inputId = "info_tabs", target = "1")
      hideTab(inputId = "info_tabs", target = "3")
      hideTab(inputId = "info_tabs", target = "4")  
      
    }
    
    
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
  
  
  output$summary = renderText({
    
    paste0("For ", input$brand_name, " the ", stringr::str_to_lower(input$shade_type), " value ranges from ",
           min_max_val()[[1]], " to ", min_max_val()[[2]], " which covers ", perc_coverage(), 
           "% of the ", stringr::str_to_lower(input$shade_type), " spectrum.\n",
           number_shades(), " shades are represented for the brand(s).")
  })

    
} 
  

shinyApp(ui, server)
