# inst/shiny-app/app.R
library(shiny)
library(ggplot2)
library(dplyr)

# Note: When the app is run from the package, it will have access to penguins_clean
# For testing, you might need to load it manually:
# load(here::here("data/penguins_clean.rda"))

ui <- fluidPage(
  # Apply a theme for better styling
  theme = shinythemes::shinytheme("lumen"),

  # App title
  titlePanel("Palmer Penguins Explorer"),

  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      h3("Controls"),
      p("Use the controls below to filter the data and customize the plot."),

      # Input: Slider for bill length
      sliderInput("billLength",
                  "Filter by Bill Length (mm):",
                  min = 30,
                  max = 60,
                  value = c(40, 50)),

      # Input: Checkbox group for species
      checkboxGroupInput("species",
                         "Select Species to Display:",
                         choices = c("Adelie", "Chinstrap", "Gentoo"),
                         selected = c("Adelie", "Chinstrap", "Gentoo")),

      # Input: Select variable for y-axis
      selectInput("y_var",
                  "Select Y-Axis Variable:",
                  choices = c("Bill Depth" = "bill_depth_mm",
                              "Flipper Length" = "flipper_length_mm",
                              "Body Mass" = "body_mass_g"),
                  selected = "bill_depth_mm")
    ),

    # Main panel for displaying outputs
    mainPanel(
      h3("Outputs"),
      p("The plot and table below will update based on your selections."),
      # Output: Scatterplot
      plotOutput("penguinPlot"),
      # Output: Data table
      dataTableOutput("penguinTable")
    )
  )
)

server <- function(input, output) {

  # Reactive expression to filter data based on inputs
  filtered_data <- reactive({
    penguinDash::penguins_clean %>%
      filter(
        bill_length_mm >= input$billLength[1],
        bill_length_mm <= input$billLength[2],
        species %in% input$species
      )
  })

  # Output: Render the scatterplot
  output$penguinPlot <- renderPlot({
    ggplot(filtered_data(), aes_string(x = "bill_length_mm", y = input$y_var, color = "species")) +
      geom_point(size = 3, alpha = 0.8) +
      labs(
        title = "Penguin Dimensions",
        x = "Bill Length (mm)",
        y = names(which(c("Bill Depth" = "bill_depth_mm",
                          "Flipper Length" = "flipper_length_mm",
                          "Body Mass" = "body_mass_g") == input$y_var)),
        color = "Species"
      ) +
      theme_minimal()
  })

  # Output: Render the data table
  output$penguinTable <- renderDataTable({
    filtered_data()
  })
}

shinyApp(ui, server)
