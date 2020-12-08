
# Required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)

# Helper files
source(here::here("Scripts", "002_create_plots.R"))

# Data
shades_values = readr::read_rds(here::here("Data",
                                           "shade_values.rds"))
shade_palette = readr::read_rds(here::here("Data",
                                           "shade_palette.rds"))


# Shiny App UI
ui = fluidPage(
  theme = shinytheme("yeti"),
  
  # Title
  titlePanel(
    title = h1("What's in a shade? An exploration of the color components of foundation shades",
               align = "center"), 
    windowTitle = "Whats in a shade?"),
  
  # Plot
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
  
  # Inputs 
  fluidRow(
    column(4,
           wellPanel(
             selectInput("brand_name",
                         label = "Brand Name",
                         choices = c("all brands", sort(unique(shades_values$brand)))),
             
             selectInput("shade_type",
                         label = "Shade Color Component", 
                         choices = colnames(shades_values)[6:9],
                         selected = colnames(shades_values)[6]),
             
             p("Click on a square and check out foundation shade tab for more information",
               style = "font-size: 70%;font-style: italic")
           )
        ),
    
    # Summary information
    column(8,
           wellPanel(
             tabsetPanel(id = "info_tabs",
                         type = "tabs",
                         # Static background tab
                         tabPanel("Background", 
                                  includeHTML(here::here("Text", "background_text.html"))),
                         
                         # tabs to dynamically appear
                         tabPanel(title = "Component: Hue",
                                  value = 1,
                                  includeHTML(here::here("Text", "hue.html"))),
                         tabPanel(title = "Component: Saturation",
                                  value = 2,
                                  includeHTML(here::here("Text", "saturation.html"))),
                         tabPanel(title = "Component: Brightness",
                                  value = 3,
                                  includeHTML(here::here("Text", "brightness.html"))),
                         tabPanel(title = "Component: Lightness",
                                  value = 4,
                                  includeHTML(here::here("Text", "lightness.html"))),
                         
                         # Tab updates based on click
                         tabPanel(title = "Foundation Shade",
                                  value = 5,
                                  htmlOutput("info_plot_click")),
                         # Static tab
                         tabPanel(title = "Credit",
                                  includeHTML(here::here("Text", "about.html"))))
           )
        )
  ),
  
  
  # Footer / Credits
  hr(),
  fluidRow(
    column(8,
           p("Shiny App Created by ", tags$a(href = "https://ijeamaka-anyene.netlify.app/", "Ijeamaka Anyene"),
             style = "font-size: 70%;font-style: italic"),
           p("Created December 2020 / Last Updated December 2020",
             style = "font-size: 70%;font-style: italic"),
           p("Code can be found on ", tags$a(href = "https://github.com/Ijeamakaanyene/pudding-shades", "GitHub"),
             style = "font-size: 70%;font-style: italic"),
           p("Data from ", tags$a(href = "https://pudding.cool/", "The Pudding"),
             style = "font-size: 70%;font-style: italic"))
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

} 
  

shinyApp(ui, server)
