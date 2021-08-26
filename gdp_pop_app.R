# Just for learning R Shiny 
# Author Luke Korir 
# Date: August2021
# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

# Load libraries
pacman::p_load(dplyr, forcats, ggplot2, shiny, tidyr, rnaturalearth, ggtext)

# Load data
source("load_process_data.R")

# Load user defined functions
source("functions.R")

# User interface design
ui <- fluidPage(
  # Title
  titlePanel("World Population, GDP, Meat supply and Protein sources"),
  mainPanel(
    column(12,
           tabsetPanel(
             tabPanel("Country level trends", 
                      fluidRow(column(8, selectizeInput("Code", "Country", choices = setNames(protein_asf$Code, protein_asf$Entity), width = "100%", selected = "Albania"
           ))),
           fluidRow(
    column(3, plotOutput("population")),
    column(3, plotOutput("gdp")),
    column(3, plotOutput("meat_ss")),
    column(3, plotOutput("asf_plant"))),
  fluidRow(column(3, plotOutput("pop_change")),
    column(3, plotOutput("gdp_change")),
    column(3, plotOutput("meat_ss_change")),
    column(3, plotOutput("asf_change")))),
  tabPanel("Leading and trailing country tables",
           fluidRow(column(2, selectInput("Continent", "Choose continent", choices = factor(unique(protein_asf$Continent)), width = "100%", selected = "Africa")),
    column(2, selectInput("n_countries", "Top n countries: ", choices = seq(100), width = "100%", selected = 5)),
    column(2, selectInput("indicator", "Choose indicator: ", c("population", "gdp_capita", "meat_ss", "animal", "plant"), width = "100%", selected = "population")),
    column(2, selectInput("year", "Choose year: ", choices =  unique(protein_asf$Year), width = "100%", selected = 2012))
 ),
 fluidRow(column(3, dataTableOutput("indicator_table"))),
 # _ _  _ _ _ Lowest country tables
fluidRow(column(2,selectInput("low_continent", "Choose continent", choices = factor(unique(protein_asf$Continent)), width = "60%", selected = "Europe" )),
  column(2, selectInput("low_n_countries", "Lowest n countries: ", choices = seq(11), width = "100%", selected = 5)),
  column(2, selectInput("low_indicator", "Choose indicator: ", c("population", "gdp_capita", "meat_ss", "animal", "plant"), width = "100%", selected = "gdp_capita")),
  column(2, selectInput("low_year", "Choose year: ", choices =  unique(protein_asf$Year), width = "100%", selected = 2017))
), fluidRow(
  column(3, dataTableOutput("low_indicator_table")))),
# Map
tabPanel("World map",
         fluidRow(column(2, selectInput("map_indicator", "Choose indicator: ", c("population", "gdp_change",     "pop_change", "meat_ss_change", "animal_change"), width = "100%", selected = "population")),
                  column(2, selectInput("map_year", "Choose year: ", choices =  unique(protein_asf$Year), width = "100%", selected = 2012))),
         fluidRow(column(3, plotOutput("world_map")))),
# Scatter plots
tabPanel("Relationship between indicators",
         fluidRow(column(2, selectInput("xcol", "X Variable", setdiff(names(protein_asf), c("Entity", "Year", "Code", "Continent")), width = "100%", selected = "animal")),
                  column(2, selectInput("ycol", "Y Variable", setdiff(names(protein_asf), c("Entity", "Year", "Code", "Continent")), width = "100%", selected = "meat_ss")),
                  column(2, selectInput("plot_year", "Choose year: ", choices =  unique(protein_asf$Year), width = "100%", selected = 2013))),
         fluidRow(column(3, plotOutput("scatter_plot"))))))))

mysubtitle <- '<span style="color:red;">—Animal</span>   <span style="color:blue;">—Plant</span>'
  
server <- function(input, output, session){
  summary <- reactive(protein_asf  %>% filter(Code == input$Code))
  
  #<< plots
  output$population <- renderPlot({
    plot_line_chart(summary(), Year, population_millions) +
      labs(title = paste0("Population (", min(summary()$Year), " - ", max(summary()$Year), ")"), 
           y = "Total (millions)")
  }, res = 96)
  
  output$meat_ss <- renderPlot({
    plot_line_chart(summary(), Year, meat_ss) +
      labs(title = paste0("Meat supply (", min(summary()$Year), " - ", max(summary()$Year), ")"),
           y = "Per capita (kg/yr)")
  }, res = 96)
  
  output$asf_plant <- renderPlot({
    plot_line_chart(summary(), Year, plant) +
      geom_line(aes(summary()$Year, summary()$animal, colour = "red", na.rm = TRUE), size = 0.8) +
      labs(title = paste0("Protein source (", min(summary()$Year), " - ", max(summary()$Year), ")"),
           subtitle = mysubtitle,
           y = "Per capita (g/day)") +
      theme(plot.subtitle = element_markdown(lineheight = 1.1, face = "bold"))
  }, res = 96)
  
   output$gdp <- renderPlot({
    plot_line_chart(summary(), Year, gdp_capita) +
      labs(title = paste0("Real GDP (", min(summary()$Year), " - ", max(summary()$Year), ")"),
           y = "Per capita (USD)")
  }, res = 96)
    
    output$pop_change <- renderPlot({
      plot_line_chart(summary(), Year, pop_change) +
        labs(title = paste0("Population growth rate (%)")) +
        ylab("")
  }, res = 96)
    
    output$gdp_change <- renderPlot({
      plot_line_chart(summary(), Year, gdp_change) +
        labs(title = paste0("GDP growth rate (%)")) +
        ylab("")
    }, res = 96)

    output$meat_ss_change <- renderPlot({
      plot_line_chart(summary(), Year, meat_ss_change) +
        labs(title = paste0("Meat supply growth rate (%)")) +
        ylab("")
    }, res = 96)
    
    output$asf_change <- renderPlot({
      plot_line_chart(summary(), Year, animal_change) +
        labs(title = paste0("Animal protein growth (%)")) +
        ylab("") +
        labs(caption = "Data: ourworldindata.org")
    }, res = 96)

    #<< tables top countries    
    table_data <- reactive(protein_asf %>% ungroup() %>% select(-c(contains("_change"))) %>%   
                             filter(Continent == input$Continent, Year == input$year))

    output$indicator_table <- renderDataTable({
      if (input$indicator == "population"){
      top_countries(table_data(), population, input$n_countries)
       } else if (input$indicator == "gdp_capita"){
         top_countries(table_data(), gdp_capita, input$n_countries)
       } else if (input$indicator == "meat_ss"){
         top_countries(table_data(), meat_ss, input$n_countries)
       } else if (input$indicator == "animal"){
         top_countries(table_data(), animal, input$n_countries)
      } else {
        top_countries(table_data(), plant, input$n_countries)
        }
    })
    # << tables lowest countries
    low_table_data <- reactive(protein_asf %>% ungroup() %>% select(-c(contains("_change"))) %>%  
                             filter(Continent == input$low_continent, Year == input$low_year))
    
    output$low_indicator_table <- renderDataTable({
      if (input$low_indicator == "population"){
        low_countries(low_table_data(), population, as.integer(input$low_n_countries))
      } else if (input$low_indicator == "gdp_capita"){
        low_countries(low_table_data(), gdp_capita, as.integer(input$low_n_countries))
      } else if (input$low_indicator == "meat_ss"){
        low_countries(low_table_data(), meat_ss, as.integer(input$low_n_countries))
      } else if (input$low_indicator == "animal"){
        low_countries(low_table_data(), animal, as.integer(input$low_n_countries))
      } else {
        low_countries(low_table_data(), plant, as.integer(input$low_n_countries))
      }
    })
    # << World map of growth rate in GDP and population
    map_data <- reactive(world %>% filter(Year == input$map_year))
    
    output$world_map <- renderPlot({
      if (input$map_indicator == "pop_change"){
      generate_map(map_data(), pop_change) + labs(title = "Yearly percentage change in total population") + 
          theme(plot.title = element_text(size=30, colour = "Blue"))
      } else if (input$map_indicator == "population"){
          generate_map(map_data(), population) + labs(title = paste("Total population in the year", map_data()$Year)) + 
          theme(plot.title = element_text(size=30, colour = "Blue"))
      } else if(input$map_indicator == "meat_ss_change"){
          generate_map(map_data(), meat_ss_change) + labs(title = "Annual percentage change in meat supply") + 
          theme(plot.title = element_text(size=30, colour = "Blue"))
      } else if(input$map_indicator == "animal_change"){
          generate_map(map_data(), animal_change) + labs(title = "Annual percentage change in animal protein") + 
          theme(plot.title = element_text(size=30, colour = "Blue"))
      } else if(input$map_indicator == "gdp_change"){
          generate_map(map_data(), gdp_change) + labs(title = "Annual percentage change in GDP per capita") + 
          theme(plot.title = element_text(size=30, colour = "Blue"))
        }
      }, height = 1000, width = 1500)
    # << Relationship between indicators
    scatter_data <- reactive(protein_asf %>% 
      ungroup() %>% 
      select(-c(Entity, Code)) %>%   
      filter(Year == input$plot_year))
    output$scatter_plot <- renderPlot({
      gen_scatter_plot(scatter_data(), .data[[input$xcol]], .data[[input$ycol]])
    }, res = 96,  height = 900, width = 1500)
}

shinyApp(ui, server)


