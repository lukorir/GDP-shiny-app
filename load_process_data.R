# This file loads and pre-process data needed for the app
# Author Luke Korir 
# Date: August 2021
# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ 

# Load data
daily_protein_ss <- read.csv("daily_protein_ss.csv") %>% 
  select(Entity, Year,
         animal = "Daily.protein.supply.of.animal.origin..FAO..2017..",
         plant = "Daily.protein.supply.of.plant.origin..FAO..2017..") %>% 
  filter(Year >= 1990) 

meat_consumption <- read.csv("meat-consumption-vs-gdp-per-capita.csv") %>%
  filter(Year >= 1990) %>% 
  select(Entity, Year, Code,
         meat_ss = "Meat.food.supply.quantity..kg.capita.yr...FAO..2020.",
         population = "Total.population..Gapminder..HYDE...UN.") %>% 
  left_join(daily_protein_ss, by = c("Entity", "Year"))

protein_asf <- read.csv("share-of-protein-supply-from-animal-based-foods-vs-gdp-per-capita.csv") %>% 
  filter(Year >= 1990) %>% 
  select(Entity, Year, Code, Continent,
         share_asf = "Daily.protein.supply.of.animal.origin..FAO..2017..",
         gdp_capita = "Output.side.real.GDP.per.capita..gdppc_o...PWT.9.1..2019..") %>% 
  left_join(meat_consumption, by = c("Entity", "Year", "Code")) %>% 
  filter(!grepl("OWID_", Code),
         !is.na(gdp_capita) & !is.na(population)) %>%
  group_by(Entity) %>% 
  arrange(Year) %>% 
  mutate(pop_change = 100 * (population -lag(population))/lag(population),
         meat_ss_change = 100 * (meat_ss - lag(meat_ss))/lag(meat_ss),
         gdp_change = 100 * (gdp_capita - lag(gdp_capita))/lag(gdp_capita),
         animal_change = 100 * (animal - lag(animal))/lag(animal),
         population_millions = population/1e6, 
         Continent =  case_when(Continent == "" ~ NA_character_,
                                TRUE ~ Continent)) %>%
  ungroup() %>% group_by(Entity) %>% 
  fill(Continent, .direction = "updown")

# Map data
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  rename(Code = wb_a3) %>% select(name, Code, geometry) %>% 
  full_join(protein_asf, by = "Code") %>% select(Entity, Year, population, geometry, contains("_change"))

