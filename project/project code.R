#load and import packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(maps)

#import datasets into R 


#select columns of raw data to keep from Wine Information Data
Wines_Info <- Wines_Info %>% 
  select(WineID, WineName, Type, ABV, Body, Acidity, Country, RegionName, Vintages)

#select columns of raw data to keep from Wine Ratings Data
Wines_Ratings <- Wines_Ratings %>% 
  select(WineID, Vintage, Rating)

#sort columns of Wine Ratings Data in order of WineID
Wines_Ratings <- Wines_Ratings[order(Wines_Ratings$WineID), ]
    
#create overall wine rating for each Wine Name (WineID)
Wine_Overall <- Wines_Ratings %>%
  group_by(WineID) %>% 
  summarise(overall_rating = mean(Rating, na.rm = TRUE)) %>%
  ungroup()

#merge datasets to one including both Wine Information and Overall Wine Rating
Wine_Final <- Wines_Info %>%
  left_join(Wine_Overall, by = "WineID")

#keep wines with highest ratings per country 
Top_Wines <- Wine_Final %>%
  group_by(Country) %>%
  slice_max(overall_rating, n = 1)

#creating colour vector for each wine type 
wine_colours <- c(
"Red" = "firebrick",
"White" = "lightgoldenrod",
"Sparkling" = "lemonchiffon",
"Rosé" = "pink",
"Port/Dessert" = "deeppink4",
"Dessert" = "deeppink4"
)

#testing interactive world map code
install.packages(c("sf", "dplyr", "leaflet", "rnaturalearth", "rnaturalearthdata"))
library(sf)
library(dplyr)
library(leaflet)
library(rnaturalearth)
library(rnaturalearthdata)

# Load libraries
library(dplyr)
library(sf)
library(leaflet)
library(tools)  # for toTitleCase

# -----------------------------
# Step 1: Prepare the data
# -----------------------------

# Make sure country names are character and trimmed
Top_Wines$Country <- trimws(as.character(Top_Wines$Country))
world$admin <- trimws(as.character(world$admin))

# Fix common mismatches (adjust if needed)
Top_Wines$Country <- recode(Top_Wines$Country,
                            "USA" = "United States",
                            "UK" = "United Kingdom",
                            "South Korea" = "Korea, Republic of"
)

# Keep only countries present in the world map
Top_Wines <- Top_Wines %>% filter(Country %in% world$admin)

# Join wine data to the world map
world_wine <- world %>%
  left_join(Top_Wines, by = c("admin" = "Country"))

# Keep only countries with geometry and wine data
world_wine_plot <- world_wine %>%
  filter(!st_is_empty(geometry)) %>%
  filter(!is.na(Type))

# -----------------------------
# Step 2: Assign colours
# -----------------------------

# Standardize Type column
world_wine_plot$Type <- as.character(world_wine_plot$Type)
world_wine_plot$Type <- trimws(world_wine_plot$Type)
world_wine_plot$Type <- toTitleCase(world_wine_plot$Type)  # ensures Red, White, Sparkling

# Define colour mapping
wine_colours <- c(
  "Red"       = "red4",
  "White"     = "lightgoldenrod",
  "Sparkling" = "lemonchiffon"
)

# Assign colours
world_wine_plot$fill_colour <- wine_colours[ world_wine_plot$Type ]

# Safety fallback (should not be needed after filtering)
world_wine_plot$fill_colour[ is.na(world_wine_plot$fill_colour) ] <- "grey80"

# -----------------------------
# Step 3: Plot the map
# -----------------------------

leaflet(world_wine_plot) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~fill_colour,
    weight = 1,
    color = "black",
    fillOpacity = 0.8,
    highlight = highlightOptions(weight = 3, color = "white", bringToFront = TRUE),
    label = ~paste0(
      "<strong>", admin, "</strong><br/>",
      "<strong>Wine:</strong> ", WineName, "<br/>",
      "<strong>Type:</strong> ", Type, "<br/>",
      "<strong>ABV:</strong> ", ABV, "%<br/>",
      "<strong>Acidity:</strong> ", Acidity, "<br/>",
      "<strong>Body:</strong> ", Body
    ) %>% lapply(htmltools::HTML)
  ) %>%
addLegend(
  position = "bottomright",
  colors = c("red4", "lightgoldenrod", "lemonchiffon"),
  labels = c("Red", "White", "Sparkling"),
  opacity = 0.8,
  title = "Wine Type"
)



