library(ggplot2)
library(shiny)
library(tidyverse)

# CSV
data <- read_csv("Inflation.csv", skip = 1,
                     col_names = c("Year", "Country", "Category", "Filter2", "Filter3", "Scale", "Symbol", "Value"))

# Prepare and clean data in general
data <- data %>%
        transmute(
          Year = as.numeric(str_trim(Year)),
          Country = str_trim(Country),
          Category = str_trim(Category),
          Value = as.numeric(Value)
        )

# First visualization

## Data preparation
### Remove Total (only interested in categories)
data_categories <- data %>% filter(!Category %in% c("Total"))

### Translate Categories
all_categories <- unique(data_categories$Category)
categories_dict <- c(
  "Acessórios para o lar, equipamento doméstico e manutenção corrente da habitação" = "Household Equipment, Domestic Goods & Home Maintenance",
  "Bebidas alcoólicas e tabaco" = "Alcoholic Beverages & Tobacco",
  "Bens e serviços diversos" = "Miscellaneous Goods & Services",
  "Comunicações" = "Communications",
  "Educação" = "Education",
  "Habitação, água, electricidade, gás e outros combustíveis" = "Housing, Water, Electricity, Gas & Other Fuels",
  "Lazer, recreação e cultura" = "Recreation & Culture",
  "Produtos alimentares e bebidas não alcoólicas" = "Food & Non-Alcoholic Beverages",
  "Restaurantes e hotéis" = "Restaurants & Hotels",
  "Saúde" = "Health",
  "Transportes" = "Transport",
  "Vestuário e calçado" = "Clothing & Footwear"
)
data_categories <- data_categories %>% mutate(Category = recode(Category, !!!categories_dict))

all_categories <- unique(data_categories$Category)

### Calculate the European average by category and year
europe_avg <- data_categories %>% group_by(Year, Category) %>%
              summarise(Europe_Avg = mean(Value, na.rm = TRUE), .groups = "drop")

### Join both data (only Portugal on the first one)
data_plot_categories <- data_categories %>% filter(Country == "Portugal") %>%
                        left_join(europe_avg, by = c("Year", "Category")) %>%
                        rename(Portugal = Value) %>%
                        select(Year, Category, Portugal, Europe_Avg) %>%
                        mutate(
                          Year = as.integer(Year),
                          Category = factor(Category, levels = all_categories),
                          Portugal = as.numeric(Portugal),
                          Europe_Avg = as.numeric(Europe_Avg)
                        )

### Convert the columns Portugal and Europe_Avg in Region and the values of the inflation comes in new column called Inflation
data_plot_categories <- data_plot_categories %>%
                        pivot_longer(cols = c("Portugal", "Europe_Avg"),
                                     names_to = "Region",
                                     values_to = "Inflation")
### Create new column for the colors
data_plot_categories <- data_plot_categories %>% mutate(
                          Region = factor(Region, levels = c("Portugal", "Europe_Avg")),
                          Sign = ifelse(Inflation >= 0, "Positive", "Negative"),
                          RegionSign = paste(Region, Sign, sep = "_")
                        ) %>% select(-Sign)

# Second visualization

## Data preparation
data_map <- data %>% filter(Category == "Total") %>%
            select(Year, Country, Inflation = Value)

### Load map
europe <- ne_countries(scale = "medium", continent = "Europe", returnclass = "sf")

### Translate Countries
all_countries <- unique(data_map$Country)
countries_dict <- c(
    "Alemanha"       = "Germany",
    "Áustria"        = "Austria",
    "Bélgica"        = "Belgium",
    "Chéquia"        = "Czech Republic",
    "Dinamarca"      = "Denmark",
    "Eslováquia"     = "Slovakia",
    "Eslovénia"      = "Slovenia",
    "Espanha"          = "Spain",
    "Estónia"        = "Estonia",
    "Finlândia"      = "Finland",
    "França"         = "France",
    "Grécia"         = "Greece",
    "Hungria"        = "Hungary",
    "Irlanda"        = "Ireland",
    "Itália"         = "Italy",
    "Lituânia"       = "Lithuania",
    "Luxemburgo"     = "Luxembourg",
    "Portugal"       = "Portugal",
    "Roménia"        = "Romania",
    "Suécia"         = "Sweden",
    "Chipre"         = "Cyprus",
    "Letónia"        = "Latvia",
    "Malta"          = "Malta",
    "Países Baixos"  = "Netherlands",
    "Polónia"        = "Poland",
    "Bulgária"       = "Bulgaria",
    "Croácia"        = "Croatia"
)
data_map <- data_map %>% mutate(Country = recode(Country, !!!countries_dict))

# Data Visualization
## Shiny App
ui <- fluidPage(
  titlePanel("Inflation Dashboard: Portugal vs Europe"),
  fluidRow(
    column(width = 6,
           plotOutput("categoryPlot", height = "700px")),
    column(width = 6,
           plotOutput("mapPlot", height = "700px"))
  ),
  br(),
  sliderInput(
    "year", "Year:",
    min = min(data_plot_categories$Year),
    max = max(data_plot_categories$Year),
    value = min(data_plot_categories$Year),
    step = 1,
    sep = "",
    animate = animationOptions(interval = 700, loop = TRUE)
  )
)

server <- function(input, output) {

  output$categoryPlot <- renderPlot({

    # Filter by year
    df_year <- data_plot_categories %>% filter(Year == input$year)

    p <- ggplot(df_year, aes(x = Category, y = Inflation, fill = RegionSign))

    # Bars side to side and flip the coordinates
    p <- p + geom_col(position = position_dodge(width = 0.9), width = 0.8) + coord_flip()

    # Percentage
    p <- p + geom_text(
      aes(label = paste0(round(Inflation, 1), "%")),
          position = position_dodge(width = 0.9),
          hjust = ifelse(df_year$Inflation >= 0, -0.1, 1.1),
          size = 3.2,
          color = "black"
    )

    # Colors
    p <- p + scale_fill_manual(
      name = "Region & Sign",
      values = c(
        "Portugal_Positive"   = "#d73027",
        "Portugal_Negative"   = "#1a9850",
        "Europe_Avg_Positive" = "#fc8d59",
        "Europe_Avg_Negative" = "#66bd63"
      ),
      labels = c(
        "Portugal_Positive"   = "Portugal (Positive)",
        "Portugal_Negative"   = "Portugal (Negative)",
        "Europe_Avg_Positive" = "Europe Avg (Positive)",
        "Europe_Avg_Negative" = "Europe Avg (Negative)"
      )
    )

    p <- p + labs(title = "Inflation by Category: Portugal vs European Average", subtitle = paste("Year:", input$year),
                  x = NULL, y = "Inflation (%)")

    p <- p + theme_minimal(base_size = 14) +
    theme(
      plot.title      = element_text(face = "bold", size = 16),
      plot.subtitle   = element_text(size = 14),
      axis.text.y     = element_text(size = 11),
      legend.position = "bottom",
      legend.title    = element_text(face = "bold"),
      legend.text     = element_text(size = 11),
      plot.margin     = margin(10, 10, 30, 10)
    )

    print(p)
  })

  output$mapPlot <- renderPlot({
    df_map <- data_map %>% filter(Year == input$year)
    europe_map <- europe %>% left_join(df_map, by = c("name" = "Country"))

    # Countries with highest and lowest inflation
    max_country <- df_map %>% filter(Inflation == max(Inflation, na.rm = TRUE)) %>% pull(Country)
    min_country <- df_map %>% filter(Inflation == min(Inflation, na.rm = TRUE)) %>% pull(Country)

    p <- ggplot(europe_map)

    p <- p + geom_sf(aes(fill = Inflation), color = "black", size = 0.3)

    # Highlights Portugal, country with the highest inflation and lowest
    p <- p + geom_sf(data = subset(europe_map, name == "Portugal"), fill = NA, color = "black", size = 1) +
        geom_sf(data = subset(europe_map, name %in% min_country), fill = NA, color = "green", size = 1) +
        geom_sf(data = subset(europe_map, name %in% max_country), fill = NA, color = "red", size = 1)

    # Percentage
    p <- p + geom_sf_text(
      aes(label = ifelse(!is.na(Inflation), paste0(round(Inflation,1), "%"), "")),
      size = 3.5,
      color = "black",
      fontface = "bold"
    )

    p <- p + scale_fill_gradient2(low = "#1a9850", mid = "white", high = "#d73027", midpoint = 0, name = "Inflation (%)") +
         coord_sf(xlim = c(-25, 45), ylim = c(34, 72), expand = FALSE)

    p <- p + labs(title = "Average Inflation by Country in Europe", subtitle = paste("Year:", input$year))

    p <- p + theme_void() +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        legend.position = "bottom",
        legend.title = element_text(face = "bold")
      )

    print(p)
  })
}

## Runs the app
shinyApp(ui, server)
