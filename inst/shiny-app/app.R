# inst/shiny-app/app.R
# Palmer Penguins Explorer - Ultimate Edition with Dark Mode

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(bslib)
library(penguinDash)

# Load data and ensure species is character
data_for_app <- penguinDash::penguins_clean %>%
  mutate(species = as.character(species))

# Species colors (work in both modes)
species_colors <- c(
  "Adelie" = "#FF6B35",
  "Chinstrap" = "#4A90E2",
  "Gentoo" = "#50C878"
)

# UI
ui <- page_sidebar(
  title = "Palmer Penguins Explorer",
  theme = bs_theme(
    version = 5,
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  fillable = TRUE,

  # Advanced CSS with Dark Mode Support
  tags$head(
    tags$style(HTML("
    /* Light mode colors (default) */
    :root {
      --bg-main: #F8FAFC;
      --bg-card: #FFFFFF;
      --bg-sidebar: #FFFFFF;
      --text-primary: #0F172A;
      --text-secondary: #64748B;
      --text-label: #334155;
      --border-color: #E2E8F0;
      --shadow-color: rgba(0, 0, 0, 0.08);
      --input-bg: #FFFFFF;
      --input-text: #0F172A;
      --kpi-avg-bg: #E9EDFC;
      --kpi-peak-bg: #FEF3E2;
      --kpi-n-bg: #E9F7EF;
    }

    /* Dark mode colors */
    body[data-theme='dark'] {
      --bg-main: #0F172A;
      --bg-card: #1E293B;
      --bg-sidebar: #1E293B;
      --text-primary: #F1F5F9;
      --text-secondary: #CBD5E1;
      --text-label: #E2E8F0;
      --border-color: #334155;
      --shadow-color: rgba(0, 0, 0, 0.3);
      --input-bg: #334155;
      --input-text: #F1F5F9;
      --kpi-avg-bg: #1E3A5F;
      --kpi-peak-bg: #3D2E1E;
      --kpi-n-bg: #1E3D2E;
    }

    body {
      background: var(--bg-main);
      color: var(--text-primary);
      transition: background-color 0.3s ease, color 0.3s ease;
    }

    /* Dark mode toggle button */
    .theme-toggle {
      position: fixed;
      top: 20px;
      right: 20px;
      z-index: 1000;
      background: var(--bg-card);
      border: 2px solid var(--border-color);
      border-radius: 50px;
      padding: 8px 16px;
      cursor: pointer;
      box-shadow: 0 4px 12px var(--shadow-color);
      transition: all 0.3s ease;
      font-size: 20px;
      color: var(--text-primary);
    }

    .theme-toggle:hover {
      transform: scale(1.05);
      box-shadow: 0 6px 16px var(--shadow-color);
    }

    /* KPI Cards */
    .kpi-row {
      display: flex;
      justify-content: center;
      align-items: stretch;
      gap: 1.5em;
      margin: 0.5rem 0 1.25rem 0;
      flex-wrap: nowrap;
    }

    .kpi-card {
      min-height: 120px;
      max-width: 320px;
      width: 100%;
      display: flex;
      flex-direction: column;
      justify-content: center;
      align-items: center;
      border-radius: 16px;
      border: 1px solid var(--border-color);
      box-shadow: 0 4px 12px var(--shadow-color);
      padding: 18px;
      background: var(--bg-card) !important;
      transition: all 0.3s ease;
    }

    .kpi-card:hover {
      transform: translateY(-3px);
      box-shadow: 0 6px 16px var(--shadow-color);
    }

    .kpi-title {
      font-weight: 600;
      font-size: 1.2rem;
      color: var(--text-primary);
      margin-bottom: 4px;
      text-align: center;
    }

    .kpi-num {
      font-size: 2.2rem;
      font-weight: 900;
      color: var(--text-primary);
      text-align: center;
      line-height: 1.1;
    }

    .kpi-bg-avg  { background: var(--kpi-avg-bg) !important; }
    .kpi-bg-peak { background: var(--kpi-peak-bg) !important; }
    .kpi-bg-n    { background: var(--kpi-n-bg) !important; }

    /* Download Button */
    .btn-download-3d {
      background: linear-gradient(145deg, #0066ff, #004db3);
      border: none;
      color: white;
      padding: 12px 24px;
      border-radius: 8px;
      font-weight: 600;
      font-size: 15px;
      box-shadow:
        0 4px 6px rgba(0, 102, 255, 0.3),
        0 1px 3px rgba(0, 0, 0, 0.2),
        inset 0 1px 0 rgba(255, 255, 255, 0.3);
      transition: all 0.2s ease;
      width: 100%;
    }

    .btn-download-3d:hover {
      background: linear-gradient(145deg, #0052cc, #003d8f);
      transform: translateY(-2px);
      box-shadow:
        0 6px 12px rgba(0, 102, 255, 0.4),
        inset 0 1px 0 rgba(255, 255, 255, 0.3);
    }

    .btn-download-3d:active {
      transform: translateY(0px);
      box-shadow:
        0 2px 4px rgba(0, 102, 255, 0.3),
        inset 0 2px 4px rgba(0, 0, 0, 0.2);
    }

    /* Cards and panels */
    .card {
      background: var(--bg-card) !important;
      border: 1px solid var(--border-color) !important;
      color: var(--text-primary) !important;
      transition: all 0.3s ease;
    }

    .card-header {
      background: var(--bg-card) !important;
      border-bottom: 1px solid var(--border-color) !important;
      color: var(--text-primary) !important;
    }

    .card-body {
      color: var(--text-primary) !important;
    }

    /* Sidebar - ENHANCED for dark mode */
    .bslib-sidebar-layout > .sidebar {
      background: var(--bg-sidebar) !important;
      border-right: 1px solid var(--border-color) !important;
      color: var(--text-primary) !important;
    }

    /* Labels - CRITICAL FIX for dark mode readability */
    label, .control-label {
      color: var(--text-label) !important;
      font-weight: 500;
    }

    /* Form inputs - ENHANCED */
    .form-select, .form-control {
      background: var(--input-bg) !important;
      color: var(--input-text) !important;
      border: 1px solid var(--border-color) !important;
    }

    .form-select option {
      background: var(--input-bg) !important;
      color: var(--input-text) !important;
    }

    /* Checkbox label */
    .form-check-label {
      color: var(--text-label) !important;
    }

    /* Slider text */
    .irs-from, .irs-to, .irs-single {
      background: var(--input-bg) !important;
      color: var(--input-text) !important;
    }

    /* Summary text */
    .summary-text {
      color: var(--text-secondary) !important;
    }

    /* Tab navigation */
    .nav-link {
      color: var(--text-secondary) !important;
    }

    .nav-link.active {
      color: var(--text-primary) !important;
      background: var(--bg-card) !important;
      border-color: var(--border-color) !important;
    }

    /* Data table in dark mode */
    body[data-theme='dark'] .dataTables_wrapper {
      color: var(--text-primary) !important;
    }

    body[data-theme='dark'] table.dataTable {
      color: var(--text-primary) !important;
      background: var(--bg-card) !important;
    }

    body[data-theme='dark'] table.dataTable thead th {
      color: var(--text-primary) !important;
      background: var(--bg-sidebar) !important;
      border-bottom: 1px solid var(--border-color) !important;
    }

    body[data-theme='dark'] table.dataTable tbody td {
      border-color: var(--border-color) !important;
    }
    "))
  ),

  # Dark mode toggle button
  tags$button(
    class = "theme-toggle",
    onclick = "toggleTheme()",
    id = "theme-button",
    "☀️"
  ),

  # JavaScript for theme switching
  tags$script(HTML("
    // Check for saved theme preference or default to light
    const currentTheme = localStorage.getItem('theme') || 'light';
    document.body.setAttribute('data-theme', currentTheme);
    updateThemeButton(currentTheme);

    function toggleTheme() {
      const body = document.body;
      const currentTheme = body.getAttribute('data-theme');
      const newTheme = currentTheme === 'light' ? 'dark' : 'light';

      body.setAttribute('data-theme', newTheme);
      localStorage.setItem('theme', newTheme);
      updateThemeButton(newTheme);
    }

    function updateThemeButton(theme) {
      const button = document.getElementById('theme-button');
      button.textContent = theme === 'light' ? '🌙' : '☀️';
    }
  ")),

  # Sidebar controls
  sidebar = sidebar(
    selectInput("species_filter", "Species:",
                choices = c("All", "Adelie", "Chinstrap", "Gentoo"),
                selected = "All"),

    selectInput("x_axis_var", "X-Axis Variable:",
                choices = c("Bill Length (mm)" = "bill_length_mm",
                            "Bill Depth (mm)" = "bill_depth_mm",
                            "Flipper Length (mm)" = "flipper_length_mm",
                            "Body Mass (g)" = "body_mass_g"),
                selected = "bill_length_mm"),

    selectInput("y_axis_var", "Y-Axis Variable:",
                choices = c("Bill Depth (mm)" = "bill_depth_mm",
                            "Flipper Length (mm)" = "flipper_length_mm",
                            "Body Mass (g)" = "body_mass_g",
                            "Bill Length (mm)" = "bill_length_mm"),
                selected = "bill_depth_mm"),

    checkboxInput("show_color", "Color by Species", value = TRUE),

    sliderInput("bill_length_range", "Bill Length Filter (mm):",
                min = 30,
                max = 60,
                value = c(30, 60),
                step = 1),

    div(class = "mt-3",
        downloadButton("download_csv", "Download Filtered CSV", class = "btn-download-3d"))
  ),

  # KPI Row
  div(class = "kpi-row",
      div(class = "kpi-card kpi-bg-avg",
          div(class = "kpi-title", "Avg. Body Mass"),
          div(class = "kpi-num", textOutput("kpi_avg_mass", inline = TRUE))
      ),
      div(class = "kpi-card kpi-bg-peak",
          div(class = "kpi-title", "Avg. Flipper Length"),
          div(class = "kpi-num", textOutput("kpi_avg_flipper", inline = TRUE))
      ),
      div(class = "kpi-card kpi-bg-n",
          div(class = "kpi-title", "Penguins Shown"),
          div(class = "kpi-num", textOutput("kpi_n", inline = TRUE))
      )
  ),

  # Summary text
  div(class = "summary-text", style = "text-align: center; margin: 1rem 0;",
      textOutput("summary_text")),

  # Main tabs
  navset_card_tab(
    nav_panel(
      "Plots",
      card(
        card_header("Penguin Measurements (Interactive)"),
        plotlyOutput("penguin_plotly", height = "480px")
      )
    ),
    nav_panel(
      "Data Table",
      card(
        card_header("Filtered Penguin Data"),
        DTOutput("penguin_table")
      )
    ),
    nav_panel(
      "Guide & Notes",
      card(
        card_header("How to Use This Dashboard"),
        div(style = "padding: 1.5rem;",
            tags$h4("Quick Start", style = "margin-top: 0;"),
            tags$p("This dashboard allows you to explore physical measurements of three penguin species from the Palmer Archipelago in Antarctica."),

            tags$h4("Interactive Controls"),
            tags$ul(
              tags$li(tags$b("Dark Mode Toggle:"), " Click the sun/moon icon in the top-right corner to switch themes."),
              tags$li(tags$b("Species Filter:"), " Focus on one species or view all together."),
              tags$li(tags$b("X-Axis & Y-Axis Variables:"), " Choose any two measurements to compare."),
              tags$li(tags$b("Color by Species:"), " Toggle on/off to show or hide species distinction."),
              tags$li(tags$b("Bill Length Filter:"), " Narrow down the data by bill length range.")
            ),

            tags$h4("Key Insights to Explore"),
            tags$ul(
              tags$li("Gentoo penguins are generally larger with longer flippers"),
              tags$li("Body mass correlates strongly with flipper length across all species"),
              tags$li("Different species occupy distinct islands in the archipelago")
            ),

            tags$hr(),
            tags$p(
              tags$b("Data Source:"),
              " Palmer Station LTER, collected by Dr. Kristen Gorman. ",
              tags$a(href = "https://allisonhorst.github.io/palmerpenguins/",
                     "Learn more", target = "_blank"),
              style = "font-size: 0.9em;"
            )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  # Reactive filtered data
  filtered_data <- reactive({
    df <- data_for_app %>%
      filter(
        bill_length_mm >= input$bill_length_range[1],
        bill_length_mm <= input$bill_length_range[2]
      )

    if (input$species_filter != "All") {
      df <- df %>% filter(species == input$species_filter)
    }

    df
  })

  # KPI outputs
  output$kpi_avg_mass <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0) return("—")
    paste0(round(mean(df$body_mass_g, na.rm = TRUE)), " g")
  })

  output$kpi_avg_flipper <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0) return("—")
    paste0(round(mean(df$flipper_length_mm, na.rm = TRUE)), " mm")
  })

  output$kpi_n <- renderText({
    df <- filtered_data()
    format(nrow(df), big.mark = ",")
  })

  # Summary text
  output$summary_text <- renderText({
    df <- filtered_data()
    paste0(
      "Showing ", nrow(df), " penguins out of ", nrow(data_for_app), " total. ",
      "Selected species: ",
      if(input$species_filter == "All") "All" else input$species_filter, "."
    )
  })

  # Enhanced plot with X/Y axis selection and color toggle
  output$penguin_plotly <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0)

    var_labels <- c(
      "bill_length_mm" = "Bill Length (mm)",
      "bill_depth_mm" = "Bill Depth (mm)",
      "flipper_length_mm" = "Flipper Length (mm)",
      "body_mass_g" = "Body Mass (g)"
    )

    if (input$show_color) {
      p <- ggplot(df, aes(x = .data[[input$x_axis_var]],
                          y = .data[[input$y_axis_var]],
                          color = species,
                          text = paste0(
                            "<b>", species, "</b><br>",
                            var_labels[input$x_axis_var], ": ", .data[[input$x_axis_var]], "<br>",
                            var_labels[input$y_axis_var], ": ", .data[[input$y_axis_var]], "<br>",
                            "Sex: ", sex, "<br>",
                            "Island: ", island
                          ))) +
        geom_point(size = 2, alpha = 0.6) +
        scale_color_manual(values = species_colors) +
        labs(color = "Species")
    } else {
      p <- ggplot(df, aes(x = .data[[input$x_axis_var]],
                          y = .data[[input$y_axis_var]],
                          text = paste0(
                            var_labels[input$x_axis_var], ": ", .data[[input$x_axis_var]], "<br>",
                            var_labels[input$y_axis_var], ": ", .data[[input$y_axis_var]], "<br>",
                            "Sex: ", sex, "<br>",
                            "Island: ", island
                          ))) +
        geom_point(size = 2, alpha = 0.6, color = "#666666")
    }

    p <- p +
      labs(
        title = paste(var_labels[input$x_axis_var], "vs", var_labels[input$y_axis_var]),
        x = var_labels[input$x_axis_var],
        y = var_labels[input$y_axis_var]
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top")

    ggplotly(p, tooltip = "text") %>%
      layout(hovermode = "closest")
  })

  # Data Table
  output$penguin_table <- renderDT({
    datatable(filtered_data(), rownames = FALSE,
              options = list(pageLength = 10, scrollX = TRUE))
  })

  # CSV Download
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("penguins_filtered_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui, server)
