# Just for learning R Shiny 
# Author Luke Korir 
# Date: August2021
# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

ui <- fluidPage(
  # Title
  titlePanel("World Population, GDP, Meat supply and Protein sources"),
  mainPanel(
    column(12,
           tabsetPanel(
             tabPanel("Country level trends", 
                      fluidRow(column(8, selectizeInput("Code", "Country", choices = stats::setNames(protein_asf$Code, protein_asf$Entity), width = "100%", selected = "Albania"
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