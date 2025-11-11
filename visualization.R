# ============================
# Set working directory
# ============================
#setwd("C:/Users/iaras/MECD/VisualizacaoDados/projeto")
setwd("//wsl.localhost/Ubuntu-20.04/home/mariana/GitHub/EuroBoom")
#setwd("/Users/luanalima/Documents/Mestrado/VPD/EuroBoom")

# ============================
# Libraries
# ============================
library(ggplot2)
library(shiny)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggtext)
library(stringr)

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
  ) %>%
  filter(Year >= 2000)

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
europe <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(continent == "Europe" | name == "Cyprus")

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

# ==========================================
# Total inflation data (Third Visualization)
# ==========================================

data_total <- data %>%
  filter(Category == "Total") %>%
  select(Year, Country, Value)

eu_total_avg <- data_total %>%
  group_by(Year) %>%
  summarise(Europe_Avg = mean(Value, na.rm = TRUE), .groups = "drop")

pt_total <- data_total %>%
  filter(Country == "Portugal") %>%
  rename(Portugal = Value)

data_total_plot <- pt_total %>%
  left_join(eu_total_avg, by = "Year") %>%
  pivot_longer(
    cols = c("Portugal", "Europe_Avg"),
    names_to = "Region",
    values_to = "Inflation"
  ) %>%
  mutate(
    Region = factor(Region, levels = c("Portugal", "Europe_Avg"))
  )

# ============================
#  RANK (Fourth visualization)
# ============================

# Data preparation
data_rank <- data %>%
  filter(Category == "Total") %>%
  select(Year, Country, Inflation = Value) %>%
  mutate(Country = recode(Country, !!!countries_dict)) %>%
  group_by(Year) %>%
  mutate(Rank = min_rank(Inflation)) %>%
  ungroup()

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

    /* Slider track */
    .irs-bar,
    .irs-bar-edge {
      background: #cccccc !important;
      border-color: #cccccc !important;
    }

    /* Slider line */
    .irs-line {
      background: #e6e6e6 !important;
      border-color: #e6e6e6 !important;
    }

    /* Slider handle */
    .irs-handle {
      background: white !important;
      border: 1px solid #666666 !important;
    }

    /* Slider text */
    .irs-grid-text,
    .irs-min,
    .irs-max,
    .irs-single {
      color: white !important;
      font-weight: bold;
    }
  "))
  ),

  titlePanel(
    tags$div("Where Does Portugal Stand in UE Inflation Landscape?", style = "color:white;")
  ),

  # Subtitle
  tags$div(paste0("Date: ", Sys.Date(), " | Authors: Beatriz Silva, Luana Lima, Mariana Rocha"),
           style = "color:white; font-size:12px; margin-bottom:15px;"),

  fluidRow(
    style = "padding:0; margin:0; margin-bottom:0px;",

    # Category plot (left)
    column(
      width = 6, style = "padding:0; margin:0;",
      h3("Inflation by Category", style = "color:white; margin-left:15px;"),
      h5(textOutput("subtitleCategory"), style = "color:white; margin-left:15px; margin-top:-5px;"),

      div(
        style = "margin-left:15px; margin-bottom:10px; color:white; font-size:13px; display:flex; gap:25px; flex-wrap:wrap;",

        # Portugal
        div(
          style = "display:flex; align-items:center; gap:6px;",
          div(
        style = "
          width:14px; height:14px;
          background: linear-gradient(to bottom, #F5A623);
          border: 1px solid white;
          border-radius: 2px;
        "
          ),
          "Portugal"
        ),

        # EU Average
        div(
          style = "display:flex; align-items:center; gap:6px;",
          div(
        style = "
          width:14px; height:14px;
          background: linear-gradient(to bottom, #E63946 60%);
          border: 1px solid white;
          border-radius: 2px;
        "
          ),
          "UE Average"
        )
      ),

      plotOutput("categoryPlot", height = "600px", width = "100%")
    ),

    # Map plot (right)
    column(
      width = 6, style = "padding:0; margin:0;",
      h3("Average Inflation by Country", style = "color:white; margin-left:15px;"),
      h5(textOutput("subtitleMap"), style = "color:white; margin-left:15px; margin-top:0px;"),

      # Custom legend shapes
      div(
        style = "margin-left:15px; margin-bottom:10px; display:flex; gap:15px; flex-wrap:wrap; align-items:center; color:white;",

        # Portugal
        div(style="display:flex; align-items:center;",
            div(style='width:14px; height:14px; border:2px solid #F5A623; border-radius:50%; background:transparent; margin-right:6px;'),
            "Portugal"),

        # Max Inflation
        div(style="display:flex; align-items:center;",
            tags$svg(
              width = "16px", height = "16px",
              tags$polygon(
                points = "8,2 14,14 2,14",
                fill = "none",
                stroke = "#F5A623",
                `stroke-width` = "2"
              ),
              style = "margin-right:6px;"
            ),
            "Max Inflation"
        ),

        # Min Inflation
        div(style="display:flex; align-items:center;",
            div(style='width:14px; height:14px; border:2px solid #F5A623; background:transparent; margin-right:6px;'),
            "Min Inflation")
      ),

      # Gradient for map fill
      div(
        style = "margin-left:15px; margin-bottom:5px;",
        div(style="
       width:260px;
       height:18px;
       border:1px solid white;
       background: linear-gradient(to right, #65C3A5, #F6E7A2, #EB4D4D);
       ")
      ),
      div(
        style="margin-left:15px; color:white; font-size:12px; display:flex; justify-content:space-between; width:260px;",
        span("Low"), span("0%"), span("High")
      ),

      plotOutput("mapPlot", height = "700px", width = "100%")
    )
  ),

  fluidRow(
    style = "padding:0; margin:0; position:relative; top:-80px;",
    column(
      width = 12,
      div(
        style = "display:flex; justify-content:center;",
        sliderInput(
          "year", "Year:",
          min = min(data_plot_categories$Year),
          max = max(data_plot_categories$Year),
          value = min(data_plot_categories$Year),
          step = 1,
          sep = "",
          ticks = TRUE,
          animate = animationOptions(interval = 700, loop = TRUE),
          width = "60%"
        )
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
  ")),

  # Total inflation over time
  fluidRow(
    style = "padding:0 18px; margin-top:10px;",
    column(
      width = 12,
      h3("Total Inflation Over Time", style = "color:white; margin-left:5px;"),
      h5(textOutput("subtitleTotal"), style = "color:#dfefff; margin-left:5px; margin-top:-5px;"),
      div(
        style = "margin-left:15px; margin-bottom:10px; color:white; font-size:13px; display:flex; gap:25px; flex-wrap:wrap;",
        
        # Portugal
        div(
          style = "display:flex; align-items:center; gap:6px;",
          div(
            style = "
          width:14px; height:14px;
          background: linear-gradient(to bottom, #F5A623 60%);
          border: 1px solid white;
          border-radius: 2px;
        "
          ),
          "Portugal"
        ),
        
        # EU Average
        div(
          style = "display:flex; align-items:center; gap:6px;",
          div(
            style = "
          width:14px; height:14px;
          background: linear-gradient(to bottom, #E63946 60%);
          border: 1px solid white;
          border-radius: 2px;
        "
          ),
          "UE Average"
        )
      ),
      plotOutput("totalPlot", height = "350px", width = "100%")
    )
  ),

  fluidRow(
    style = "padding:0 18px; margin-top:10px;",
    column(
      width = 12,
      h3("Ranking of Annual Percentage Change in Inflation by Country",
         style = "color:white; margin-left:5px;"),
      div(
        style = "margin-left:15px; margin-bottom:10px; color:white; font-size:13px; display:flex; gap:25px; flex-wrap:wrap;",
        
        # Portugal
        div(
          style = "display:flex; align-items:center; gap:6px;",
          div(
            style = "
          width:14px; height:14px;
          background: linear-gradient(to bottom, #FFD700);
          border: 1px solid white;
          border-radius: 2px;
        "
          ),
          "Portugal"
        ),
        
        # Selected Countries
        div(
          style = "display:flex; align-items:center; gap:6px;",
          div(
            style = "
          width:14px; height:14px;
          background: linear-gradient(to bottom, #FF6F3C);
          border: 1px solid white;
          border-radius: 2px;
        "
          ),
          "Selected Countries"
        )
      ),
      plotOutput("rankPlot", height = "350px", width = "100%")
    )
  ),
  
  fluidRow(
    style = "padding:5px 18px; margin-top:5px;",
    column(
      width = 12,
      h5("Select a country to highlight:", style = "color:white; margin-left:5px;"),
      uiOutput("countryButtons")
    )
  ),

  br()
)

# ============================
#          SERVER
# ============================

server <- function(input, output) {

  # ===============================
  # Plot 1: Category-wise inflation
  # ===============================
  output$subtitleCategory <- renderText({
    paste("How inflation varies by category: Portugal in comparison with the UE average in", input$year)
  })

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
          "Portugal_Positive"   = "#F5A623",
          "Portugal_Negative"   = "#F5A623",
          "Europe_Avg_Positive" = "#E63946",
          "Europe_Avg_Negative" = "#E63946"
        ),
        labels = c(
          "Portugal_Positive"   = "Portugal (Positive)",
          "Portugal_Negative"   = "Portugal (Negative)",
          "Europe_Avg_Positive" = "Europe Avg (Positive)",
          "Europe_Avg_Negative" = "Europe Avg (Negative)"
        )
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.background  = element_rect(fill = "#0C2947", color = "#0C2947"),
        panel.background = element_rect(fill = "#0C2947", color = NA),
        plot.margin      = margin(10, 40, 10, 5),
        panel.grid       = element_line(color = "#5a5a5a", linetype = "dotted"),
        axis.text.y      = element_text(size = 11, color = "white"),
        axis.text.x      = element_text(size = 11, color = "white"),
        axis.title       = element_text(size = 12, color = "white"),
        legend.position  = "none",
        legend.key.size  = unit(0.5, "lines")
      )
  })

  # ===============================
  # Plot 2: Map
  # ===============================
  output$subtitleMap <- renderText({
    paste("Mapping UE inflation: Where Portugal stands among the highest and lowest in", input$year)
  })

  output$mapPlot <- renderPlot(bg="#0C2947", {
    df_map <- data_map %>% filter(Year == input$year)
    europe_map <- europe %>% left_join(df_map, by = c("name" = "Country"))

    max_country <- df_map %>% filter(Inflation == max(Inflation, na.rm = TRUE)) %>% mutate(Type = "Max")
    min_country <- df_map %>% filter(Inflation == min(Inflation, na.rm = TRUE)) %>% mutate(Type = "Min")
    portugal     <- df_map %>% filter(Country == "Portugal") %>% mutate(Type = "Portugal")

    if ("Portugal" %in% c(min_country, max_country)) {
      portugal <- NULL
    }

    highlight_countries <- bind_rows(max_country, min_country, portugal)

    # Calculate centroids for labels
    centroids <- sapply(highlight_countries$Country, function(cntry) {
      st_coordinates(st_centroid(europe_map[europe_map$name == cntry, ]))
    })
    centroids <- t(centroids) %>% as.data.frame()
    colnames(centroids) <- c("x", "y")

    highlight_countries <- cbind(highlight_countries, centroids)

    # Assign external label positions
    n <- nrow(highlight_countries)
    x_offset <- 45
    y_offsets <- seq(max(highlight_countries$y) + 5, min(highlight_countries$y) - 5, length.out = n)

    highlight_countries <- highlight_countries %>%
      mutate(xout = x_offset, yout = y_offsets)

    ggplot(europe_map) +
      geom_sf(aes(fill = Inflation), color = "#0C2947", size = 0.3, na.fill = "#223B5A") +

      geom_curve(
        data = highlight_countries,
        aes(x = x, y = y, xend = xout, yend = yout),
        color = "white", size = 0.5, curvature = 0.2
      ) +

      geom_point(
        data = highlight_countries,
        aes(x = xout, y = yout, shape = Type),
        color = "#F5A623", size = 5
      ) +

      scale_shape_manual(
        values = c(
          "Portugal" = 21,
          "Max"      = 24,
          "Min"      = 22
        )
      ) +

      # Labels
      geom_label(
        data = highlight_countries,
        aes(x = xout, y = yout, label = paste0(Country, "\n", round(Inflation,1), "%")),
        fill = "#0C2947", color = "white", size = 3.5, fontface = "bold", label.size = 0,
        hjust = 0, nudge_x = 1
      ) +
      scale_fill_gradient2(
        low      = "#65C3A5",
        mid      = "#F6E7A2",
        high     = "#EB4D4D",
        midpoint = 0,
        name     = "Inflation (%)",
        na.value = "#223B5A"
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
        legend.position   = "none",
        legend.background = element_rect(fill = "#0C2947", color = NA),
        legend.key        = element_rect(fill = "#0C2947", color = NA),
        legend.title      = element_text(face = "bold", color = "white"),
        legend.text       = element_text(color = "white", size = 10),
        axis.text         = element_blank(),
        axis.title        = element_blank(),
        axis.ticks        = element_blank()
      )
  })

  # ===============================
  # Plot 3: Total inflation over time
  # ===============================
  output$totalPlot <- renderPlot(bg = "#0C2947", {
    df_years <- data_total_plot

    anos <- 2000:2024
    labels_anos <- c("2000", paste0(substr(anos[-1], 3, 4), "'"))

    df_wide <- df_years |>
      tidyr::pivot_wider(names_from = Region, values_from = Inflation) |>
      dplyr::mutate(
        diff = abs(Portugal - Europe_Avg),
        offset_pt   = ifelse(diff < 0.3,  0.25, 0),
        offset_eu   = ifelse(diff < 0.3, -0.25, 0)
      )

    # mean value 22
    mid_value_2022 <- df_wide |>
      dplyr::filter(Year == 2022) |>
      dplyr::summarise(mean_val = mean((Portugal + Europe_Avg) / 2)) |>
      dplyr::pull(mean_val)

    top_y <- max(df_wide$Portugal, df_wide$Europe_Avg) + 1

    ggplot(df_wide, aes(x = Year)) +
      # Important period bands with more transparency
      geom_rect(aes(xmin = 2008, xmax = 2009, ymin = -Inf, ymax = Inf),
                fill = "#1A3E66", alpha = 0.03) +
      geom_rect(aes(xmin = 2011, xmax = 2014, ymin = -Inf, ymax = Inf),
                fill = "#1A3E66", alpha = 0.03) +
      geom_rect(aes(xmin = 2020, xmax = 2021, ymin = -Inf, ymax = Inf),
                fill = "#1A3E66", alpha = 0.03) +
      geom_rect(aes(xmin = 2022, xmax = 2024, ymin = -Inf, ymax = Inf),
                fill = "#1A3E66", alpha = 0.03) +

      # Vertical lines at the start and end of each event
      geom_vline(xintercept = c(2008, 2009, 2011, 2014, 2020, 2021, 2021, 2022),
                 color = "white", linetype = "dashed", linewidth = 0.8) +

      # Labels at the top of the bands (positioned at the center of each band)
      annotate("text", x = (2008 + 2009)/2, y = top_y, label = "Global Financial Crisis",
               color = "white", size = 3.5, fontface = "bold", hjust = 0.5) +
      annotate("text", x = (2011 + 2014)/2, y = top_y, label = "Troika & Austerity",
               color = "white", size = 3.5, fontface = "bold", hjust = 0.5) +
      annotate("text", x = (2020 + 2021)/2, y = top_y, label = "COVID-19 pandemic",
               color = "white", size = 3.5, fontface = "bold", hjust = 0.5) +
      annotate("text", x = (2022 + 2024)/2, y = top_y, label = "Start of Russia–Ukraine war",
               color = "white", size = 3.5, fontface = "bold", hjust = 0.5) +

      # Segments connecting Portugal vs EU
      geom_segment(aes(y = Portugal, yend = Europe_Avg, xend = Year),
                   color = "white", linewidth = 2.5, lineend = "round") +

      # Labels for Portugal and EU average
      geom_point(aes(y = Portugal),
                 shape = 21, fill = "#F5A623", color = "white", stroke = 1.5, size = 6) +
      geom_point(aes(y = Europe_Avg),
                 shape = 21, fill = "#E63946", color = "white", stroke = 1.5, size = 6) +

      # Rótulos
      geom_text(aes(
        x = Year - 0.25,
        y = Portugal + offset_pt,
        label = paste0(round(Portugal, 1), "%")
      ),
      color = "white", size = 3.7, hjust = 1) +

      geom_text(aes(
        x = Year - 0.25,
        y = Europe_Avg + offset_eu,
        label = paste0(round(Europe_Avg, 1), "%")
      ),
      color = "white", size = 3.7, hjust = 1) +

      scale_x_continuous(
        breaks = anos,
        labels = labels_anos,
        expand = expansion(mult = 0.03)
      ) +

      labs(x = "Year", y = "Inflation (%)") +
      theme_minimal(base_size = 14) +
      theme(
        plot.background = element_rect(fill = "#0C2947", color = "#0C2947"),
        panel.background = element_rect(fill = "#0C2947", color = NA),
        panel.grid = element_line(color = "#5a5a5a", linetype = "dotted"),
        axis.text = element_text(size = 11, color = "white"),
        axis.title = element_text(size = 12, color = "white"),
        legend.position = "none"
      )
  })

  # =========================
  # Plot 4: Rank over time
  # =========================

  selected_countries <- reactiveVal(c("Portugal"))

  # UI Render for country buttons
  output$countryButtons <- renderUI({
    sel <- selected_countries()

    div(
      style = "display:flex; flex-wrap:wrap; gap:8px;",
      lapply(sort(setdiff(unique(data_rank$Country), "Portugal")), function(cntry) {
        actionButton(
          inputId = paste0("btn_", gsub(" ", "_", cntry)),
          label = cntry,
          style = paste0(
            "background-color:", ifelse(cntry %in% sel, "#FF6F3C", "#1F3B5A"), ";",
            "color:", ifelse(cntry %in% sel, "white", "white"), ";",
            "border:none; border-radius:20px;",
            "padding:6px 14px; font-size:13px; font-weight:600;",
            "transition: all 0.2s;",
            "cursor:pointer;",
            "box-shadow:", ifelse(cntry %in% sel, "0 2px 5px rgba(0,0,0,0.3);", "none;")
          ),
          title = cntry
        )
      })
    )
  })

  # Observers to update selected countries
  lapply(sort(setdiff(unique(data_rank$Country), "Portugal")), function(cntry) {
    observeEvent(input[[paste0("btn_", gsub(" ", "_", cntry))]], {
      current <- selected_countries()
      if(cntry %in% current) {
        selected_countries(setdiff(current, cntry))
      } else {
        selected_countries(c(current, cntry))
      }
    })
  })


  output$rankPlot <- renderPlot(bg = "#0C2947", {

    sel_countries <- selected_countries()

    first_year  <- min(data_rank$Year, na.rm = TRUE)
    last_year   <- max(data_rank$Year, na.rm = TRUE)
    n_countries <- max(data_rank$Rank, na.rm = TRUE)

    # Side labels with Portugal highlighted
    make_labels <- function(d, y) {
      d %>%
        filter(Year == y) %>%
        group_by(Rank) %>%
        summarise(
          Year   = y,
          has_pt = any(Country == "Portugal"),
          others_raw = paste(setdiff(sort(unique(Country)), "Portugal"), collapse = ", "),
          .groups = "drop"
        ) %>%
        mutate(
          label_html = ifelse(
            has_pt,
            paste0("<span style='color:#FFD700;font-weight:700'>Portugal</span>",
                   ifelse(others_raw != "", paste0(", ", others_raw), "")),
            others_raw
          )
        )
    }

    side_labels <- bind_rows(
      make_labels(data_rank, first_year) %>% mutate(side = "left"),
      make_labels(data_rank, last_year) %>% mutate(side = "right")) %>%
      mutate(
        x = if_else(side == "left", first_year, last_year),
        hjust = if_else(side == "left", 1.05, 0.00))

    ggplot(data_rank, aes(x = Year, y = Rank, group = Country)) +
      geom_segment(
        data = data.frame(y = 1:n_countries),
        aes(x = first_year, xend = last_year, y = y, yend = y),
        inherit.aes = FALSE,
        linetype = "dotted", color = "#5a5a5a", linewidth = 0.3) +

      geom_line(data = subset(data_rank, !(Country %in% sel_countries) & Country != "Portugal"),
                color = "grey70", alpha = 0.6, linewidth = 0.6) +
      geom_point(data = subset(data_rank, !(Country %in% sel_countries) & Country != "Portugal"),
                 color = "grey70", alpha = 0.6, size = 1.1) +

      # Highlight Selected Countries
      geom_line(data = subset(data_rank, Country %in% sel_countries & Country != "Portugal"),
                color = "#FF6F3C", linewidth = 1.6) +
      geom_point(data = subset(data_rank, Country %in% sel_countries & Country != "Portugal"),
                 color = "#FF6F3C", size = 2.2) +

      # Highlight Portugal
      geom_line(data = subset(data_rank, Country == "Portugal"), color = "#FFD700", linewidth = 1.6) +
      geom_point(data = subset(data_rank, Country == "Portugal"), color = "#FFD700", size = 2.2) +

      geom_richtext(
        data = side_labels,
        aes(x = x, y = Rank, label = label_html, hjust = hjust),
        color = "white", size = 3,
        label.size = 0, fill = NA, label.color = NA,
        inherit.aes = FALSE) +

      scale_y_reverse(breaks = 1:n_countries, expand = expansion(mult = c(0.02, 0.10))) +
      scale_x_continuous(limits = c(first_year, last_year),
                         breaks = seq(first_year, last_year, by = 4),
                         expand = expansion(mult = c(0, 0))) +
      coord_cartesian(clip = "off") +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 12) +
      theme(
        plot.background  = element_rect(fill = "#0C2947", color = "#0C2947"),
        panel.background = element_rect(fill = "#0C2947", color = NA),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        axis.text.x  = element_text(color = "white", size = 9),
        axis.text.y  = element_blank(),
        plot.margin  = margin(6, 100, 6, 70)
      )
  })
}

shinyApp(ui, server)

