# CCMR Analysis Rebuild
# This script focuses on CCMR variables and removes control variables from primary visualizations.

### 1. Packages and Setup
# Install packages if they are missing (commented out to prevent auto-run issues)
# install.packages(c("plotly", "shiny", "ggplot2", "rmarkdown", "tidyverse", "ggridges", "GGally", "RColorBrewer", "viridis", "extrafont", "ggpubr"), repos = "http://cran.rstudio.com/")

library(plotly)
library(shiny)
library(ggplot2)
library(rmarkdown)
library(tidyverse)
library(ggridges)
library(GGally)
library(RColorBrewer)
library(viridis)
library(extrafont)
library(ggpubr)

# Use standard font to avoid "invalid font type" errors
font_family <- "sans"

### 2. Data Loading and Cleaning
# Load the new dataset
raw_data <- read.csv("FinalData.csv")

# Map new variables to the logic of the old script
# Old Variable                -> New Variable
# VoterTurnoutRate            -> GraduationRates (and other CCMR vars)
# MedianHouseholdIncome       -> PovertyRate (Socio-economic proxy)
# ObesityRate                 -> ViolentRate (Health/Social stressor)
# FoodInsecurity              -> PropertyRate (Economic stressor)
# LimitedAccesstoHealthyFoods -> MobilityRate (Stability indicator)
# Region                      -> CountyCode (Derived from DistrictNumber)

# Create CountyCode to serve as "Region"
# DistrictNumber is typically 6 digits, first 3 are County Code.
raw_data <- raw_data %>%
    mutate(
        # Ensure DistrictNumber is a string, pad with leading zeros if necessary (though usually read as int)
        DistrictStr = sprintf("%06d", DistrictNumber),
        CountyCode = substr(DistrictStr, 1, 3),
        RegionLabel = case_when(
            CountyCode == "057" ~ "Dallas",
            CountyCode == "220" ~ "Tarrant",
            CountyCode == "101" ~ "Harris",
            TRUE ~ CountyCode
        )
    )

# Select and rename columns for clarity in analysis
data <- raw_data %>%
    select(
        Name,
        TotalPopulation,
        Region = RegionLabel, # Mapping RegionLabel to Region
        GraduationRates, # Target Variable 1
        CCMR.Grads, # Target Variable 2
        CCMR.College, # Target Variable 3
        TSI.Both, # Target Variable 4
        DualCredit, # Target Variable 5
        APBI, # Target Variable 6
        Associates, # Target Variable 7
        OnRamps, # Target Variable 8
        PovertyRate, # (was MedianHouseholdIncome)
        ViolentRate, # (was ObesityRate)
        PropertyRate, # (was FoodInsecurity)
        MobilityRate # (was LimitedAccesstoHealthyFoods)
    ) %>%
    filter(complete.cases(.)) # Filter missing data

# Define colors for the new "Regions" (Counties)
region_choices <- c("All", unique(data$Region))
num_counties <- length(unique(data$Region))

### 3. Visualizations

# --- Ridge Plot ---
# Distribution of CCMR Graduates by Region (County)
top_counties <- data %>%
    group_by(Region) %>%
    summarise(Pop = sum(TotalPopulation)) %>%
    top_n(20, Pop) %>%
    pull(Region)

p_ridge <- data %>%
    filter(Region %in% top_counties) %>%
    ggplot(aes(x = CCMR.Grads, y = Region, fill = Region)) +
    geom_density_ridges() +
    theme_ridges() +
    theme(legend.position = "none") +
    labs(x = "CCMR Graduates (%)", y = "County Code", title = "CCMR Graduates by County (Top 20 by Pop)") +
    theme(text = element_text(family = font_family))

print(p_ridge)

# --- Scatterplot Matrix ---
# Relationships between CCMR and Crime variables (Removed Poverty/Mobility)

panel.hist <- function(x, ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5))
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks
    len <- length(breaks)
    y <- h$counts / max(h$counts)
    rect(breaks[-len], 0, breaks[-1], y, col = "#17BEBB")
}

# Save settings
old_par <- par(no.readonly = TRUE)
par(family = font_family)

data %>%
    select(CCMR.Grads, CCMR.College, ViolentRate, PropertyRate) %>%
    plot(
        pch = 19,
        cex = 0.8,
        lower.panel = panel.smooth,
        diag.panel = panel.hist,
        col = "#0E7C7B",
        labels = c("CCMR Grads", "CCMR College", "Violent", "Property"),
        gap = 0.3,
        upper.panel = NULL,
        main = "Scatterplot Matrix: CCMR vs Crime"
    )

# Restore settings
par(old_par)

# --- Bar-Box Plot ---
# College Readiness distribution by Region (Top 20)
p_barbox <- data %>%
    filter(Region %in% top_counties) %>%
    ggbarplot(
        x = "Region", y = "CCMR.College",
        color = "Region",
        add = c("median", "boxplot", "jitter"),
        add.params = list(width = 0.3),
        position = position_dodge(0.8),
        size = 1.2
    ) +
    labs(y = "College Readiness Rate", x = "", title = "College Readiness by County (Top 20)") +
    theme_minimal() +
    theme(
        legend.position = "none",
        text = element_text(family = font_family),
        panel.background = element_rect(fill = "gray90", color = NA),
        axis.text.x = element_text(angle = 45, hjust = 1)
    )

# Interactive version
if (interactive()) {
    print(ggplotly(p_barbox) %>% layout(legend = list(x = 0.75, y = 1)))
} else {
    print(p_barbox)
}

# --- Waffle Chart ---
# Binned by CCMR.Grads instead of GraduationRates

top_6_counties <- data %>%
    group_by(Region) %>%
    summarise(Pop = sum(TotalPopulation)) %>%
    top_n(6, Pop) %>%
    pull(Region)

waffle_data <- data %>%
    filter(Region %in% top_6_counties) %>%
    mutate(GradBin = case_when(
        CCMR.Grads >= 80 ~ "High (>80%)",
        CCMR.Grads >= 60 ~ "Med (60-80%)",
        TRUE ~ "Low (<60%)"
    )) %>%
    group_by(Region, GradBin) %>%
    summarise(Count = n(), .groups = "drop") %>%
    arrange(Region, desc(GradBin)) %>%
    group_by(Region) %>%
    mutate(
        Total = sum(Count),
        # Create a sequence for waffle blocks
        BlockID = list(1:sum(Count))
    ) %>%
    unnest(BlockID) %>%
    mutate(
        # Simple logic to assign blocks to categories based on cumulative counts
        Category = case_when(
            BlockID <= Count[GradBin == "High (>80%)"] ~ "High (>80%)",
            BlockID <= (Count[GradBin == "High (>80%)"] + Count[GradBin == "Med (60-80%)"]) ~ "Med (60-80%)",
            TRUE ~ "Low (<60%)"
        )
    ) %>%
    # Grid coordinates (10 columns wide)
    mutate(
        x = (BlockID - 1) %% 10,
        y = (BlockID - 1) %/% 10
    )

p_waffle <- ggplot(waffle_data, aes(x = x, y = y, fill = Category)) +
    geom_tile(color = "white", size = 0.5) +
    facet_wrap(~Region) +
    scale_fill_manual(values = c("High (>80%)" = "#02d914", "Med (60-80%)" = "#a82eff", "Low (<60%)" = "#ff0400")) +
    coord_equal() +
    theme_void() +
    labs(title = "District Count by CCMR Grad Rate Tier (Top 6 Counties)", fill = "CCMR Rate") +
    theme(text = element_text(family = font_family), legend.position = "bottom")

print(p_waffle)


# --- Bubble Chart 1: Violent Crime vs CCMR Grads (Size = Poverty) ---
p_bubble1 <- ggplot(data, aes(
    x = ViolentRate, y = CCMR.Grads, size = PovertyRate,
    color = Region, text = paste("Name:", Name)
)) +
    geom_point(alpha = 0.5, shape = 16) +
    scale_size(range = c(1, 10), name = "Poverty Rate", guide = "none") +
    theme_minimal() +
    labs(y = "CCMR Graduates", x = "Violent Crime Rate", title = "Violent Crime vs CCMR Grads (Size = Poverty)") +
    theme(text = element_text(family = font_family), legend.position = "none")

if (interactive()) {
    print(ggplotly(p_bubble1, tooltip = "text") %>% layout(legend = list(x = 0.75, y = 1)))
} else {
    print(p_bubble1)
}

# --- Bubble Chart 2: Property Crime vs CCMR College Ready (Size = Mobility) ---
p_bubble2 <- ggplot(data, aes(
    x = PropertyRate, y = CCMR.College, size = MobilityRate,
    color = Region, text = paste("Name:", Name)
)) +
    geom_point(alpha = 0.5, shape = 16) +
    scale_size(range = c(1, 10), name = "Mobility Rate", guide = "none") +
    theme_minimal() +
    labs(y = "CCMR College Ready", x = "Property Crime Rate", title = "Property Crime vs CCMR College (Size = Mobility)") +
    theme(text = element_text(family = font_family), legend.position = "none")

if (interactive()) {
    print(ggplotly(p_bubble2, tooltip = "text") %>% layout(legend = list(x = 0.75, y = 1)))
} else {
    print(p_bubble2)
}

### 4. Shiny Application
# Full implementation of the interactive scatterplot explorer with Modern UI

ui <- fluidPage(
    # Custom CSS for Modern "React-like" Look
    tags$head(
        tags$style(HTML("
            @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;600&display=swap');

            body {
                background-color: #f4f6f8;
                color: #374151;
                font-family: 'Inter', sans-serif;
            }

            .container-fluid {
                padding: 20px;
                max-width: 1200px;
                margin: 0 auto;
            }

            h2 {
                font-weight: 600;
                color: #111827;
                margin-bottom: 24px;
                font-size: 24px;
            }

            .card {
                background-color: #ffffff;
                border-radius: 12px;
                box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06);
                padding: 24px;
                margin-bottom: 24px;
                border: 1px solid #e5e7eb;
            }

            .control-label {
                font-weight: 600;
                font-size: 14px;
                color: #374151;
                margin-bottom: 8px;
            }

            .form-control {
                border-radius: 8px;
                border: 1px solid #d1d5db;
                padding: 8px 12px;
                box-shadow: none;
                transition: border-color 0.15s ease-in-out;
            }

            .form-control:focus {
                border-color: #6366f1;
                box-shadow: 0 0 0 3px rgba(99, 102, 241, 0.1);
            }

            .selectize-input {
                border-radius: 8px !important;
                border: 1px solid #d1d5db !important;
                padding: 8px 12px !important;
                box-shadow: none !important;
            }

            .selectize-input.focus {
                border-color: #6366f1 !important;
                box-shadow: 0 0 0 3px rgba(99, 102, 241, 0.1) !important;
            }

            /* Custom Grid Layout */
            .dashboard-grid {
                display: grid;
                grid-template-columns: 300px 1fr;
                gap: 24px;
            }

            @media (max-width: 768px) {
                .dashboard-grid {
                    grid-template-columns: 1fr;
                }
            }
        "))
    ),
    div(
        class = "container-fluid",
        h2("District Variable Explorer"),
        div(
            class = "dashboard-grid",
            # Control Panel
            div(
                class = "card",
                h4("Controls", style = "margin-top: 0; margin-bottom: 20px; font-size: 18px; font-weight: 600;"),
                selectInput("x_var", "Predictor (X-Axis)",
                    choices = c(
                        "Violent Crime Rate" = "ViolentRate",
                        "Property Crime Rate" = "PropertyRate"
                    ),
                    selected = "ViolentRate"
                ),
                selectInput("y_var", "Outcome (Y-Axis)",
                    choices = c(
                        "CCMR Grads" = "CCMR.Grads",
                        "CCMR College Ready" = "CCMR.College",
                        "Graduation Rates" = "GraduationRates",
                        "TSI Both" = "TSI.Both",
                        "Dual Credit" = "DualCredit",
                        "AP/IB" = "APBI",
                        "Associates Degree" = "Associates",
                        "OnRamps" = "OnRamps"
                    ),
                    selected = "CCMR.Grads"
                ),
                uiOutput("region_selector")
            ),

            # Visualization Panel
            div(
                class = "card",
                plotlyOutput("distPlot", height = "500px")
            )
        )
    )
)

server <- function(input, output) {
    output$region_selector <- renderUI({
        selectInput("region", "Filter by County", choices = region_choices)
    })

    output$distPlot <- renderPlotly({
        req(input$x_var, input$y_var)

        # Filter data based on region selection
        plot_data <- data
        if (!is.null(input$region) && input$region != "All") {
            plot_data <- plot_data %>% filter(Region == input$region)
        }

        # Get x and y variables
        x_var <- input$x_var
        y_var <- input$y_var

        # Create base ggplot
        p <- ggplot(plot_data, aes_string(x = x_var, y = y_var, fill = "Region", text = "Name")) +
            geom_point(shape = 21, size = 3, color = "white", stroke = 0.5) +
            labs(
                x = names(which(c(
                    "Violent Crime Rate" = "ViolentRate",
                    "Property Crime Rate" = "PropertyRate"
                ) == x_var)),
                y = names(which(c(
                    "Graduation Rates" = "GraduationRates",
                    "CCMR Grads" = "CCMR.Grads",
                    "CCMR College Ready" = "CCMR.College",
                    "TSI Both" = "TSI.Both",
                    "Dual Credit" = "DualCredit",
                    "AP/IB" = "APBI",
                    "Associates Degree" = "Associates",
                    "OnRamps" = "OnRamps"
                ) == y_var))
            ) +
            theme_minimal() +
            theme(
                text = element_text(family = font_family),
                axis.title = element_text(size = 12, face = "bold", color = "#374151"),
                axis.text = element_text(color = "#6b7280"),
                panel.grid.major = element_line(color = "#f3f4f6"),
                panel.grid.minor = element_blank(),
                legend.position = "none" # Hide legend in interactive plot for cleaner look
            )

        # Add regression line
        if (!is.null(input$region) && input$region == "All") {
            p <- p + geom_smooth(aes(group = 1), method = "lm", color = "#111827", alpha = 0.2, size = 0.8)
        } else {
            p <- p + geom_smooth(method = "lm", color = "#111827", alpha = 0.2, size = 0.8)
        }

        # Convert to Plotly with enhanced config
        ggplotly(p, tooltip = c("text", "x", "y")) %>%
            config(
                displayModeBar = TRUE,
                displaylogo = FALSE,
                modeBarButtonsToRemove = c(
                    "select2d", "lasso2d", "autoScale2d",
                    "hoverClosestCartesian", "hoverCompareCartesian"
                )
            ) %>%
            layout(
                hoverlabel = list(bgcolor = "white", font = list(family = "Inter")),
                margin = list(t = 20, r = 20, b = 20, l = 20)
            )
    })
}

# Run the application only in interactive mode
if (interactive()) {
    shinyApp(ui = ui, server = server)
}
