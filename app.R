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
  
  titlePanel(
    title = h1("What's in a shade? An exploration of the composition of foundation shades",
               align = "center"),
    windowTitle = "Whats in a shade?"),
  
  fluidRow(
    column(6,
           selectInput("brand_name",
                       label = "Brand Name",
                       choices = c("all brands", sort(unique(shades_values$brand))))
           ),
    column(6,
           selectInput("shade_type",
                       label = "Shade Type", 
                       choices = colnames(shades_values)[6:9],
                       selected = colnames(shades_values)[6])
           )
  ),
  
  fluidRow(
    column(12,
           mainPanel(
             plotOutput("plot", 
                        width = "800px",
                        click = "plot_click"),
             textOutput("summary")
           )
    )
    )
)


server <- function(input, output, session) {
  
  # Limiting data to UI selections
  plot_data = reactive({
    filter_df(shades_values,
              input$brand_name,
              input$shade_type)
  })
  
  # Outputting plot based off of UI suggestions  
  output$plot = renderPlot({
      
    generate_plot(plot_data(),
                    shade_palette,
                    input$shade_type,
                    paste0("Increasing ", input$shade_type))
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
    paste0("For ", input$brand_name, " the minimum ", stringr::str_to_lower(input$shade_type), " value is ",
           min_max_val()[[1]], " and the maximum is ", min_max_val()[[2]], " which covers ", perc_coverage(), 
           "% of the ", stringr::str_to_lower(input$shade_type), " spectrum.\n",
           "There is a total of ", number_shades(), " shades represented.")
  })

    
} 
  

shinyApp(ui, server)
