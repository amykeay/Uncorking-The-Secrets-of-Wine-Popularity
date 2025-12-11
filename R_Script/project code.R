#DATA PREPARATION -------------------------------------------------------

#load and import packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(maps)
library(patchwork)
library(tidyr)
library(ggiraph)

#CLEANING DATA -------------------------------------------------------

#import datasets into R 
#Import Wine Information Data
Wine_Info <- read_csv("Wine_Info.csv")
#Import Wine Ratings Data
Wine_Ratings <- read_csv("Wine_Ratings.csv")

#select columns of raw data to keep from Wine Information Data
Wine_Info <- Wine_Info %>% 
  select(WineID, WineName, Type, ABV, Body, Acidity, Country, RegionName, Vintages)

#select columns of raw data to keep from Wine Ratings Data
Wine_Ratings <- Wine_Ratings %>% 
  select(WineID, Vintage, Rating)

#sort columns of Wine Ratings Data in order of WineID
Wine_Ratings <- Wine_Ratings[order(Wines_Ratings$WineID), ]
    
#create overall wine rating for each Wine Name (WineID)
Wine_Overall <- Wine_Ratings %>%
  group_by(WineID) %>% 
  summarise(overall_rating = mean(Rating, na.rm = TRUE)) %>%
  ungroup()

#merge datasets to one including both Wine Information and Overall Wine Rating
Wine_Final <- Wine_Info %>%
  left_join(Wine_Overall, by = "WineID")

#VISUALISATIONS -------------------------------------------------------

#creating colour vector for each wine type 
wine_colours <- c(
"Red" = "red3",
"White" = "lightgoldenrod",
"Sparkling" = "lemonchiffon",
"Rosé" = "pink",
"Dessert/Port" = "deeppink4",
"Dessert" = "deeppink4"
)
#merge wine type colour vector into data 
Wine_Final$WineTypeColour <- wine_colours[Wine_Final$Type]

# creating a custom theme function for all visualisations 
theme_project <- function() {
  theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 16),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#fff0f3", color = "grey80")
    )
}

#BASIC BAR PLOTS USING GGPLOT -------------------------------------------------------
#ABV by WineID (with colour coded bars)
p1_barplot + p2_barplot + p3_barplot + p4_barplot
p1_barplot <- ggplot(Wine_Final, aes(x = as.factor(WineID), y = ABV, fill = Type)) +
  geom_col(color = "black") +
  labs(title = "ABV Percentage by WineID", 
       x = "WineID", y = "ABV Percentage") +
  scale_fill_manual(values = wine_colours) +
  theme_project()

#Body by WineID (with colour coded bars)
p2_barplot <- ggplot(Wine_Final, aes(x = as.factor(WineID), y = Body, fill = Type)) +
  geom_col(color = "black") +
  labs(title = "Body by WineID", 
       x = "WineID", y = "Body") +
  scale_fill_manual(values = wine_colours) +
  theme_project()

#Acidity by WineID (with colour coded bars)
p3_barplot <- ggplot(Wine_Final, aes(x = as.factor(WineID), y = Acidity, fill = Type)) +
  geom_col(color = "black") +
  labs(title = "Acidity by WineID", 
       x = "WineID", y = "Acidity") +
  scale_fill_manual(values = wine_colours) + 
  theme_project()

#Overall Ratings by WineID (with colour coded bars)
p4_barplot <- ggplot(Wine_Final, aes(x = as.factor(WineID), y = overall_rating, fill = Type)) +
  geom_col(color = "black") +
  labs(title = "Overall Rating by WineID", 
       x = "WineID", y = "Overall Rating") +
  scale_fill_manual(values = wine_colours) + 
  theme_project()

# PLOTS GROUPED BY COUNTRY USING GGPLOT -------------------------------------------------------

#ABV by Country (with colour coded bars)
ggplot(Wine_Final, aes(x = Country, y = ABV, fill = Type)) +
  geom_col(aes(group = interaction(Type, WineID)),
           color = "black",
           position = position_dodge(width = 0.9)) +
  labs(title = "ABV % by Country and Type",
       x = "Country", y = "ABV %") +
  scale_fill_manual(values = wine_colours) +
  theme_project()

#Body by WineID (with colour coded bars)
ggplot(Wine_Final, aes(x = Country, y = Body, fill = Type)) +
  geom_col(aes(group = interaction(Type, WineID)),
           color = "black",
           position = position_dodge(width = 0.9)) +
  labs(title = "Body by Country and Type",
       x = "Country", y = "Body") +
  scale_fill_manual(values = wine_colours) +
  theme_project()

#Acidity by WineID (with colour coded bars)
ggplot(Wine_Final, aes(x = Country, y = Acidity, fill = Type)) +
  geom_col(aes(group = interaction(Type, WineID)),
           color = "black",
           position = position_dodge(width = 0.9)) +
  labs(title = "Acidity by Country and Type",
       x = "Country", y = "Acidity") +
  scale_fill_manual(values = wine_colours) +
  theme_project()

#Overall Ratings by WineID (with colour coded bars)
ggplot(Wine_Final, aes(x = Country, y = overall_rating, fill = Type)) +
  geom_col(aes(group = interaction(Type, WineID)),
           color = "black",
           position = position_dodge(width = 0.9)) +
  labs(title = "Overall Rating by Country and Type",
       x = "Country", y = "Overall Rating") +
  scale_fill_manual(values = wine_colours) +
  theme_project()

# GROUPED BAR PLOT ARRANGED BY COUNTRY CODE USING GGPLOT -------------------------------------------------------
#create new data by keeping wines with highest ratings per country 
Top_Wines <- Wine_Final %>%
  group_by(Country) %>%
  slice_max(overall_rating, n = 1)


#Preparing numeric attribute data (converting groups to numbers so they can be plotted)
Top_Wines <- Top_Wines %>%
  mutate(
    Body_num = recode(Body,
                      "Very light-bodied" = 1,
                      "Light-bodied" = 2,
                      "Medium-bodied" = 3,
                      "Full-bodied" = 4,
                      "Very full-bodied" = 5),
    
    Acidity_num = recode(Acidity,
                         "Low" = 1,
                         "Medium" = 2,
                         "High" = 3),
    
    ABV_num = ABV / 10 
  )
#Round overall rating to 2 decimal places (so that plots can be cleaner)
Top_Wines$overall_rating <- round(Top_Wines$overall_rating, 2)

#modifying colour vector for each wine type 
wine_colours_plot <- c(
  "Red" = "#CD0000",
  "White" = "#EEDD82",
  "Sparkling" = "#FFFACD")


#Convert to long format (required for grouped bars)
wine_long <- Top_Wines %>%
  select(Type, Country, ABV_num, Body_num, Acidity_num, overall_rating) %>%
  pivot_longer(
    cols = c(ABV_num, Body_num, Acidity_num, overall_rating),
    names_to = "Attribute",
    values_to = "Value"
  )
#Grouped bar chart per country
ggplot(wine_long, aes(x = Attribute, y = Value, fill = Type)) +
  geom_col() +
  facet_wrap(~ Country) +
  scale_fill_manual(values = wine_colours_plot) +
  labs(
    title = "Characteristics of the Most Popular Wine by Country",
    x = "Characteristic",
    y = "Value"
  ) +
  theme_project() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
  )


# INTERACTIVE WORLD MAP CODE USING GGPLOT -------------------------------------------------------

#testing interactive world map code
install.packages(c("sf", "dplyr", "leaflet", "rnaturalearth", "rnaturalearthdata"))
library(sf)
library(dplyr)
library(leaflet)
library(rnaturalearth)
library(rnaturalearthdata)
library(tools)

#Prepare the data
#loading world map 
world <- ne_countries(scale = "medium", returnclass = "sf")

# Make sure country names are character and trimmed
Top_Wines$Country <- trimws(as.character(Top_Wines$Country))
world$admin <- trimws(as.character(world$admin))

#recode that handles different spellings and extra spaces so that it matches the world map
Top_Wines$Country <- dplyr::recode(
  Top_Wines$Country,
  "USA" = "United States of America",
  "United States" = "United States of America",
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

#Step 2: Assign colours
# Standardize Type column
world_wine_plot$Type <- as.character(world_wine_plot$Type)
world_wine_plot$Type <- trimws(world_wine_plot$Type)
world_wine_plot$Type <- toTitleCase(world_wine_plot$Type)  

# Assign colour map 
world_wine_plot$fill_colour <- wine_colours_plot[ world_wine_plot$Type ]
world_wine_plot$fill_colour <- as.character(world_wine_plot$fill_colour)
world_wine_plot$fill_colour[is.na(world_wine_plot$fill_colour)] <- "#CCCCCC"

#Step 3: Plot the map
leaflet(world_wine_plot) %>%
  addTiles() %>%
  setView(lng = 0, lat = 0, zoom = 1) %>%
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
  colors = c("#CD0000", "#EEDD82", "#FFFACD"),
  labels = c("Red", "White", "Sparkling"),
  opacity = 0.8,
  title = "Wine Type"
)
