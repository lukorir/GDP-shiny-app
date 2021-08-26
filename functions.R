# This file contains user defined functions used in the app
# Author Luke Korir
# Date: August 2021
# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ 
# Define function to plot line chart
plot_line_chart <- function(df, var1, var2){
  df %>%  
    ggplot(aes({{ var1 }}, {{ var2 }})) +
    geom_line(na.rm = TRUE, size = 0.8, colour = "blue") +
    theme(legend.position = "none") +
    scale_x_continuous(breaks = seq(1990, 2020, by = 5)) +
    theme(panel.grid.major = element_line(color = gray(0.8),
                                          linetype = "solid",
                                          size = 0.2),
          panel.background = element_rect(fill = "white")) +
    theme(axis.line.x = element_line(colour = "black", size = 1)) 
}

# Function to get top countries by indicator
top_countries <- function(df, var, n) {
  df %>% 
    filter(!is.na( {{ var }})) %>% 
    arrange (desc({{ var }})) %>% 
    mutate(Rank = row_number()) %>% 
    slice(1:{{ n }})
}

# Function to get lowest countries by indicator
low_countries <- function(df, var, x) {
  df %>% 
    filter(!is.na( {{ var }})) %>% 
    arrange (desc({{ var }})) %>% 
    mutate(Rank = row_number()) %>% 
    slice_tail( n = {{ x }})
}

# Function to create map

generate_map <- function(df, var){
  df %>% 
    ggplot() +
    geom_sf(aes(fill = {{ var }})) +
    xlab("Longitude") + ylab("Latitude") +
    scale_fill_viridis_c(option = "plasma") +
    theme_bw()
}

# Function to plot scatter plots
gen_scatter_plot <- function(df, var1, var2){
  df %>% 
    ggplot(aes({{ var1 }}, {{ var2 }})) +
    geom_point(aes(color = Continent), size = 10) +
    theme_bw() +
    labs(caption = "Data: ourworldindata.org")
  }