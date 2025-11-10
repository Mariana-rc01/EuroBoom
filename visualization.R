#setwd("C:/Users/iaras/MECD/VisualizacaoDados/projeto")
setwd("//wsl.localhost/Ubuntu-20.04/home/mariana/GitHub/EuroBoom")

library(ggplot2)
library(shiny)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# ============================
#            DATA
# ============================

data <- read_csv("Inflation.csv", skip = 1,
                 col_names = c("Year", "Country", "Category",
                               "Filter2", "Filter3", "Scale", "Symbol", "Value"))
# Prepare and clean data in general
data <- data %>%
  transmute(
    Year = as.numeric(str_trim(Year)),
    Country = str_trim(Country),
    Category = str_trim(Category),
    Value = as.numeric(Value)
  )

# ===================================
#   CATEGORIES (First Visualization)
# ===================================

# Data preparation
# Remove Total (only interested in categories)
data_categories <- data %>% filter(!Category %in% c("Total"))

# Translate Categories
categories_dict <- c(
  "Acessórios para o lar, equipamento doméstico e manutenção corrente da habitação" =
    "Household Equipment, Domestic Goods & Home Maintenance",
  "Bebidas alcoólicas e tabaco" = "Alcoholic Beverages & Tobacco",
  "Bens e serviços diversos"    = "Miscellaneous Goods & Services",
  "Comunicações"                = "Communications",
  "Educação"                    = "Education",
  "Habitação, água, electricidade, gás e outros combustíveis" =
    "Housing, Water, Electricity, Gas & Other Fuels",
  "Lazer, recreação e cultura"  = "Recreation & Culture",
  "Produtos alimentares e bebidas não alcoólicas" =
    "Food & Non-Alcoholic Beverages",
  "Restaurantes e hotéis"       = "Restaurants & Hotels",
  "Saúde"                       = "Health",
  "Transportes"                 = "Transport",
  "Vestuário e calçado"         = "Clothing & Footwear"
)

data_categories <- data_categories %>%
  mutate(Category = recode(Category, !!!categories_dict))

all_categories <- unique(data_categories$Category)

# Group in only 4 categories
group_dict <- c(
    "Household Equipment, Domestic Goods & Home Maintenance" = "Housing & Utilities",
    "Housing, Water, Electricity, Gas & Other Fuels" = "Housing & Utilities",

    "Food & Non-Alcoholic Beverages" = "Food & Leisure",
    "Restaurants & Hotels" = "Food & Leisure",
    "Recreation & Culture" = "Food & Leisure",

    "Health" = "Health & Education",
    "Education" = "Health & Education",

    "Alcoholic Beverages & Tobacco" = "Other Goods & Services",
    "Miscellaneous Goods & Services" = "Other Goods & Services",
    "Communications" = "Other Goods & Services",
    "Transport" = "Other Goods & Services",
    "Clothing & Footwear" = "Other Goods & Services"
)

data_categories <- data_categories %>%
  mutate(Group = recode(Category, !!!group_dict))

# Calculate the European average by category and year
europe_avg <- data_categories %>%
  group_by(Year, Group) %>%
  summarise(Europe_Avg = mean(Value, na.rm = TRUE), .groups = "drop")

# Calculate the Portugal average by category and year
portugal_avg <- data_categories %>%
  filter(Country == "Portugal") %>%
  group_by(Year, Group) %>%
  summarise(Portugal = mean(Value, na.rm = TRUE), .groups = "drop")

# Join both data (only Portugal on the first one)
data_plot_categories <- portugal_avg %>%
  left_join(europe_avg, by = c("Year", "Group")) %>%
  mutate(
    Year       = as.integer(Year),
    Group      = factor(Group, levels = c("Housing & Utilities",
                                          "Food & Leisure",
                                          "Health & Education",
                                          "Other Goods & Services")),
    Portugal   = as.numeric(Portugal),
    Europe_Avg = as.numeric(Europe_Avg)
  )

# Convert the columns Portugal and Europe_Avg in Region and the values of the inflation comes in new column called Inflation
# Create new column for the colors with 'mutate'
data_plot_categories <- data_plot_categories %>%
  pivot_longer(
    cols = c("Portugal", "Europe_Avg"),
    names_to = "Region",
    values_to = "Inflation"
  ) %>%
  mutate(
    Region    = factor(Region, levels = c("Portugal", "Europe_Avg")),
    Sign      = ifelse(Inflation >= 0, "Positive", "Negative"),
    RegionSign = paste(Region, Sign, sep = "_")
  ) %>%
  select(-Sign)

# ============================
#  MAP (Second visualization)
# ============================

# Data preparation
data_map <- data %>%
  filter(Category == "Total") %>%
  select(Year, Country, Inflation = Value)

# Load map
europe <- ne_countries(scale = "medium", continent = "Europe", returnclass = "sf")

# Translate Countries
countries_dict <- c(
  "Alemanha"       = "Germany",
  "Áustria"        = "Austria",
  "Bélgica"        = "Belgium",
  "Chéquia"        = "Czech Republic",
  "Dinamarca"      = "Denmark",
  "Eslováquia"     = "Slovakia",
  "Eslovénia"      = "Slovenia",
  "Espanha"        = "Spain",
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

data_map <- data_map %>%
  mutate(Country = recode(Country, !!!countries_dict))

# ============================
#            UI
# ============================

ui <- fluidPage(
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@400;700&display=swap",
      rel = "stylesheet"
    ),
    tags$style(HTML("
    html, body {
      background-color: #0C2947 !important;
      margin: 0 !important;
      padding: 0 !important;
      overflow-x: hidden;
      font-family: 'Montserrat', sans-serif !important;
    }

    .container-fluid {
      background-color: #0C2947 !important;
      margin: 0 !important;
      padding: 0 !important;
      width: 100% !important;
      font-family: 'Montserrat', sans-serif !important;
    }

    h3, h4, h5, h6, .irs, label {
      font-family: 'Montserrat', sans-serif !important;
    }
  "))
  ),

  titlePanel(
    tags$div("Inflation Dashboard: Portugal vs Europe", style = "color:white;")
  ),

  # Subtitle
  tags$div(paste0("Date: ", Sys.Date(), " | Authors: Beatriz Iara, Luana Lima, Mariana Rocha"),
           style = "color:white; font-size:12px; margin-bottom:15px;"),

  fluidRow(
    style = "padding:0; margin:0;",

    # Left side: 3 Visualizations (categoryPlot, temporalPlot and rankedPlot)
    column(
      width = 6, style = "padding:0 10px; margin:3;",

      h4("Percentage Change in Inflation by Category Relative to Last Year", style = "color:white; margin-left:5px;"),
      h5(textOutput("subtitleCategory"), style = "color:white; margin-left:15px; margin-top:-5px;"),
      plotOutput("categoryPlot", height = "200px", width = "100%"),

      h4("ToDo - temporalPlot", style = "color:white; margin-left:5px; margin-top:15px;"),
      plotOutput("toDo", height = "200px", width = "100%"),

      h4("ToDo - rankedPlot", style = "color:white; margin-left:5px; margin-top:15px;"),
      plotOutput("toDo", height = "200px", width = "100%")
    ),

    # Right side: map
    column(
      width = 6, style = "padding:0; margin:0;",
      h3("Yearly Percentage Change in Inflation by Country", style = "color:white; margin-left:15px;"),
      h5(textOutput("subtitleMap"), style = "color:white; margin-left:15px; margin-top:-5px;"),
      plotOutput("mapPlot", height = "700px", width = "100%")
    )
  ),

  br(),
  fluidRow(
    column(
      width = 12, style = "padding:0 40px;",
      sliderInput(
        "year", "Year:",
        min = min(data_plot_categories$Year),
        max = max(data_plot_categories$Year),
        value = min(data_plot_categories$Year),
        step = 1,
        sep = "",
        ticks = TRUE,
        animate = animationOptions(interval = 800, loop = TRUE)
      )
    )
  ),
  tags$style(HTML("
    .irs {
      width: 100% !important;
    }

    label[for='year'], .irs .irs-grid-text, .irs .irs-min, .irs .irs-max {
      color: white !important;
      font-weight: bold;
    }

    .irs .irs-line, .irs .irs-bar, .irs .irs-bar-edge {
      background: #1E90FF !important;
      border-radius: 7px !important;
    }

    .irs .irs-from, .irs .irs-to, .irs .irs-single {
      background: #5500FF !important;
      color: white !important;
      border: none;
    }
  "))
)

# ============================
#          SERVER
# ============================

server <- function(input, output) {

  # Subtítulos fora dos gráficos
  output$subtitleCategory <- renderText({
    paste("Year:", input$year)
  })

  output$subtitleMap <- renderText({
    paste("Europe - Year:", input$year)
  })

  # -------- Visualization 1: Categories --------
  output$categoryPlot <- renderPlot(bg="#0C2947", {
    df_year <- data_plot_categories %>% filter(Year == input$year)

    ggplot(df_year, aes(x = Group, y = Inflation, fill = RegionSign)) +
      geom_col(position = position_dodge(width = 0.9), width = 0.8) +
      coord_flip() +
      geom_text(
        aes(label = paste0(round(Inflation, 1), "%")),
        position = position_dodge(width = 0.9),
        hjust = ifelse(df_year$Inflation >= 0, -0.1, 1.1),
        size = 3.2,
        color = "white"
      ) +
      scale_fill_manual(
        values = c(
          "Portugal_Positive"   = "#ff6666",
          "Portugal_Negative"   = "#66ff66",
          "Europe_Avg_Positive" = "#ff9999",
          "Europe_Avg_Negative" = "#99ff99"
        ),
        labels = c(
          "Portugal_Positive"   = "Portugal (Positive)",
          "Portugal_Negative"   = "Portugal (Negative)",
          "Europe_Avg_Positive" = "Europe Avg (Positive)",
          "Europe_Avg_Negative" = "Europe Avg (Negative)"
        )
      ) +
      labs(x = NULL, y = "Inflation (%)", fill = NULL) + # hide legend title
      theme_minimal(base_size = 14) +
      theme(
        plot.background  = element_rect(fill = "#0C2947", color = "#0C2947"),
        panel.background = element_rect(fill = "#0C2947", color = NA),
        plot.margin      = margin(10, 40, 10, 5),
        panel.grid       = element_line(color = "#5a5a5a", linetype = "dotted"),
        axis.text.y      = element_text(size = 11, color = "white"),
        axis.text.x      = element_text(size = 11, color = "white"),
        axis.title       = element_text(size = 12, color = "white"),
        legend.position  = "bottom",
        legend.title     = element_text(face = "bold", color = "white"),
        legend.text      = element_text(size = 10, color = "white"),
        legend.key.size  = unit(0.5, "lines")
      )
  })

  # -------- Visualization 2: Map --------
  output$mapPlot <- renderPlot(bg="#0C2947", {
    df_map <- data_map %>% filter(Year == input$year)
    europe_map <- europe %>% left_join(df_map, by = c("name" = "Country"))

    # Countries with highest and lowest inflation
    max_country <- df_map %>% filter(Inflation == max(Inflation, na.rm = TRUE)) %>% pull(Country)
    min_country <- df_map %>% filter(Inflation == min(Inflation, na.rm = TRUE)) %>% pull(Country)

    ggplot(europe_map) +
      geom_sf(aes(fill = Inflation), color = "#0C2947", size = 0.3, na.fill = "#6C819C") +  # linhas invisíveis
      geom_sf(data = subset(europe_map, name == "Portugal"), fill = NA, color = "#0C2947", size = 1) +
      geom_sf(data = subset(europe_map, name %in% min_country), fill = NA, color = "#66ff66", size = 1) +
      geom_sf(data = subset(europe_map, name %in% max_country), fill = NA, color = "#ff6666", size = 1) +
      geom_sf_text(
        aes(label = ifelse(!is.na(Inflation), paste0(round(Inflation, 1), "%"), "")),
        size = 3.5, color = "black", fontface = "bold"
      ) +
      scale_fill_gradient2(
        low = "#66ff66",
        mid = "white",
        high = "#ff6666",
        midpoint = 0,
        name = "Inflation (%)",
        na.value = "#6C819C"
      ) +
      coord_sf(xlim = c(-30, 50), ylim = c(20, 70), expand = FALSE, clip = "off") +
      labs(x = NULL, y = NULL) +
      theme_minimal() +
      theme(
        plot.background   = element_rect(fill = "#0C2947", color = "#0C2947"),
        panel.background  = element_rect(fill = "#0C2947", color = NA),
        panel.grid        = element_blank(),
        plot.margin       = margin(10, 15, 10, 15),
        panel.spacing     = unit(0, "pt"),
        legend.position   = "bottom",
        legend.background = element_rect(fill = "#0C2947", color = NA),
        legend.key        = element_rect(fill = "#0C2947", color = NA),
        legend.title      = element_text(face = "bold", color = "white"),
        legend.text       = element_text(color = "white", size = 10),
        axis.text         = element_blank(),
        axis.title        = element_blank(),
        axis.ticks        = element_blank()
      )
  })
}

shinyApp(ui, server)

