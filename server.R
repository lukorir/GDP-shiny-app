# Just for learning R Shiny 
# Author Luke Korir 
# Date: August2021
# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

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
    # << Relationship between indicators
    scatter_data <- reactive(protein_asf %>% 
      ungroup() %>% 
      select(-c(Entity, Code)) %>%   
      filter(Year == input$plot_year))
    output$scatter_plot <- renderPlot({
      gen_scatter_plot(scatter_data(), .data[[input$xcol]], .data[[input$ycol]])
    }, res = 96,  height = 900, width = 1500)
}



