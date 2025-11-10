# ============================
# Set working directory
# ============================
setwd("C:/Users/iaras/MECD/VisualizacaoDados/projeto")
#setwd("//wsl.localhost/Ubuntu-20.04/home/mariana/GitHub/EuroBoom")

# ============================
# Libraries
# ============================
library(ggplot2)
library(shiny)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(stringr)

# ============================
# Load and clean data
# ============================

data <- read_csv("Inflation.csv", skip = 1,
                 col_names = c("Year", "Country", "Category",
                               "Filter2", "Filter3", "Scale", "Symbol", "Value"))

data <- data %>%
  transmute(
    Year = as.numeric(str_trim(Year)),
    Country = str_trim(Country),
    Category = str_trim(Category),
    Value = as.numeric(Value)
  ) %>%
  filter(Year >= 2000)   # <<--- ANOS >= 2000


# ============================
#   CATEGORIES (First Visualization) 
# ============================

data_categories <- data %>% filter(!Category %in% c("Total"))

# Translate categories to English
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

# Compute EU average per year/category
europe_avg <- data_categories %>%
  group_by(Year, Category) %>%
  summarise(Europe_Avg = mean(Value, na.rm = TRUE), .groups = "drop")

# Prepare Portugal data and join with EU average
data_plot_categories <- data_categories %>%
  filter(Country == "Portugal") %>%
  left_join(europe_avg, by = c("Year", "Category")) %>%
  rename(Portugal = Value) %>%
  select(Year, Category, Portugal, Europe_Avg) %>%
  mutate(
    Year       = as.integer(Year),
    Portugal   = as.numeric(Portugal),
    Europe_Avg = as.numeric(Europe_Avg)
  )

# Pivot to long format for plotting multiple regions
data_plot_categories <- data_plot_categories %>%
  pivot_longer(
    cols = c("Portugal", "Europe_Avg"),
    names_to = "Region",
    values_to = "Inflation"
  ) %>%
  mutate(
    Region = factor(Region, levels = c("Portugal", "Europe_Avg"))
  )

# Keep main categories, group others as "Others"
selected_categories <- c(
  "Housing, Water, Electricity, Gas & Other Fuels",
  "Alcoholic Beverages & Tobacco",
  "Food & Non-Alcoholic Beverages",
  "Education",
  "Health",
  "Transport"
)

data_plot_categories <- data_plot_categories %>%
  mutate(
    Category = ifelse(Category %in% selected_categories, Category, "Others")
  ) %>%
  group_by(Year, Region, Category) %>%
  summarise(Inflation = sum(Inflation, na.rm = TRUE), .groups = "drop") %>%
  # Define explicitamente a ordem do factor: categorias selecionadas primeiro, Others por último
  mutate(
    Category = factor(Category, levels = c(selected_categories, "Others")),
    Region   = factor(Region, levels = c("Portugal", "Europe_Avg"))
  )


# ============================
#  MAP (Second visualization)
# ============================

data_map <- data %>%
  filter(Category == "Total") %>%
  select(Year, Country, Inflation = Value)

europe <- ne_countries(scale = "medium", continent = "Europe", returnclass = "sf")

# Translate country names to match map shapefile
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
# Total inflation data (Plot 3)
# ============================

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
#            UI
# ============================
ui <- fluidPage(
  tags$style(HTML("
    html, body { background-color: #0C2947 !important; margin: 0 !important; padding: 0 !important; overflow-x: hidden; }
    .container-fluid { background-color: #0C2947 !important; margin: 0 !important; padding: 0 !important; width: 100% !important; }
    .row, .col-sm-6, .shiny-plot-output { background-color: #0C2947 !important; margin: 0 !important; padding: 0 !important; }

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
  ")),
  
  titlePanel(
    tags$div("Where Does Portugal Stand in UE Inflation Landscape?", style = "color:white;")
  ),
  
  # Row with Category plot and Map plot
  fluidRow(
    style = "padding:0; margin:0; margin-bottom:0px;",  # colado no slider
    
    # Category plot (left)
    column(
      width = 6, style = "padding:0; margin:0;",
      h3("Inflation by Category", style = "color:white; margin-left:15px;"),
      h5(textOutput("subtitleCategory"), style = "color:white; margin-left:15px; margin-top:-5px;"),
      div(
        style = "margin-left:15px; margin-bottom:10px; color:white; font-size:13px; display:flex; gap:25px; flex-wrap:wrap;",
        div(style="display:flex; align-items:center;",
            div(style='width:14px; height:14px; background:#F5A623; margin-right:6px;'), "Portugal"),
        div(style="display:flex; align-items:center;",
            div(style='width:14px; height:14px; background:#3A6EA5; margin-right:6px;'), "EU Average")
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
  
  # Slider for year selection
  fluidRow(
    column(
      width = 12,
      style="display:flex; justify-content:center; margin-top:0px; margin-bottom:20px;", # <--- colado
      sliderInput(
        "year", "",
        min = min(data_plot_categories$Year),
        max = max(data_plot_categories$Year),
        value = min(data_plot_categories$Year),
        step = 1,
        sep = "",
        animate = animationOptions(interval = 700, loop = TRUE),
        width = "50%"
      )
    )
  ),
  
  # Total inflation over time (Plot 3)
  fluidRow(
    style = "padding:0; margin:0;",
    column(
      width = 12,
      h3("Total Inflation Over Time", style = "color:white; margin-left:15px;"),
      h5(textOutput("subtitleTotal"), style = "color:white; margin-left:15px; margin-top:-5px;"),
      plotOutput("totalPlot", height = "350px", width = "100%")
    )
  ),
  
  br()
)


# ============================
#          SERVER
# ============================

server <- function(input, output) {
  
  # Helper function: split long labels into multiple lines
  label_lines <- function(labels, max_lines = 2) {
    n <- length(labels)
    lapply(seq_len(n), function(i) {
      words <- strsplit(labels[i], " ")[[1]]
      mid <- ceiling(length(words) / max_lines)
      split_words <- split(words, ceiling(seq_along(words)/mid))
      paste(sapply(split_words, paste, collapse = " "), collapse = "\n")
    }) %>% unlist()
  }
  
  # ===============================
  # Plot 1: Category-wise inflation
  # ===============================
  output$subtitleCategory <- renderText({
    paste("How inflation varies by category: Portugal in comparison with the EU average in", input$year)
  })
  
  output$categoryPlot <- renderPlot(bg="#0C2947", {
    df_year <- data_plot_categories %>% filter(Year == input$year)
    df_year <- df_year %>%
      mutate(Category = factor(Category, levels = c(selected_categories, "Others")))

    category_labels <- label_lines(levels(df_year$Category), max_lines = 4)
    ggplot(df_year, aes(x = Category, y = Inflation, fill = Region)) +
      geom_col(position = position_dodge(width = 0.9), width = 0.8) +
      coord_flip() +
      geom_text(
        aes(label = paste0(round(Inflation, 1), "%")),
        position = position_dodge(width = 0.9),
        hjust = ifelse(df_year$Inflation >= 0, -0.1, 1.1),
        size = 3.2,
        color = "white"
      ) +
      scale_x_discrete(labels = category_labels) +
      scale_fill_manual(
        values = c(
          "Portugal"   = "#F5A623",
          "Europe_Avg" = "#3A6EA5"
        )
      ) +
      labs(x = NULL, y = "Inflation (%)") +
      theme_minimal(base_size = 14) +
      theme(
        plot.background  = element_rect(fill = "#0C2947", color = "#0C2947"),
        panel.background = element_rect(fill = "#0C2947", color = NA),
        plot.margin      = margin(10, 15, 10, 2),
        panel.grid       = element_line(color = "#5a5a5a", linetype = "dotted"),
        axis.text.y      = element_text(size = 9, color = "white"),
        axis.text.x      = element_text(size = 9, color = "white"),
        axis.title       = element_text(size = 12, color = "white"),
        legend.position  = "none"
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
    
    # Plot map
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
      theme_minimal() +
      theme(
        plot.background   = element_rect(fill = "#0C2947", color = "#0C2947"),
        panel.background  = element_rect(fill = "#0C2947", color = NA),
        panel.grid        = element_blank(),
        plot.margin       = margin(10, 15, 10, 15),
        legend.position   = "none",  
        legend.text       = element_text(color = "white"),
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
    
    ggplot(df_wide, aes(x = Year)) +
      # Annotation for war event
      geom_segment(
        data = data.frame(x = 2022, xend = 2020.5, y = mid_value_2022, yend = mid_value_2022),
        aes(x = x, xend = xend, y = y, yend = yend),
        color = "white",
        linewidth = 0.8  
      ) +
      
      annotate(
        "label",
        x = 2020.25,
        y = mid_value_2022,
        label = "Start of Russia–Ukraine war",
        fill = "#1A3E66",
        color = "white",
        size = 3.5,
        label.size = 0  # remove borda extra
      ) +
      
      # Segments connecting Portugal vs EU
      geom_segment(aes(y = Portugal, yend = Europe_Avg, xend = Year),
                   color = "white", linewidth = 2.5, lineend = "round") +
      
      # Labels for Portugal and EU average
      geom_point(aes(y = Portugal),
                 shape = 21, fill = "#F5A623", color = "white", stroke = 1.5, size = 6) +
      geom_point(aes(y = Europe_Avg),
                 shape = 21, fill = "#3A6EA5", color = "white", stroke = 1.5, size = 6) +
      
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
}

shinyApp(ui, server)