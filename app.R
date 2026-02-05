library(shiny)
library(tidyverse)
library(bslib)
library(thematic)
library(shinycssloaders)
library(shinyjs)
library(DT)
library(plotly)

if (!file.exists("data/sales.csv")) {
  source("processing.R")
}

sales <- read_csv("data/sales.csv")

light = bs_theme(
  bootswatch = "spacelab",
  primary = "#235ec4",
  base_font = font_google("Poppins")
)
dark = bs_theme(
  bootswatch = "spacelab",
  bg = "#313238",
  fg = "#FFF",
  primary = "#235ec4",
  base_font = font_google("Poppins")
)
options(spinner.type = 8, spinner.color = "#235ec4", spinner.caption = "Loading...")

ui <- navbarPage(
  # Customization
  theme = light,
  useShinyjs(),
  
  title = div(
    span(style = "font-size: 1.5em; position: relative; top: 15px;",
         "Superstore Profitability Analysis"),
    div(
      style = "font-size: 0.8em; position: absolute; right: 15px; top: 30px;",
      checkboxInput("dark_mode", "Dark Mode", value = FALSE, width = "auto")
    )
  ),
  
  
  # Visualization Tab
  tabPanel(
    title = "Profitability Visualization",
    titlePanel("Superstore Sales"),
    
    hr(),
    
    fluidRow(
      column(
        9,
        radioButtons(
          inputId = "subcategory", label = "Category",
          choices = sort(unique(sales$Subcategory)),
          inline = TRUE
        )
      ),
      column(
        3,
        selectInput(
          inputId = "state",
          label = "State",
          choices = sort(unique(sales$State))
        )
      )
    ),
    
    hr(),
    
    fluidRow(
      column(
        12,
        plotOutput("profit_plot", click = "plot_click") |> 
          withSpinner(),
        verbatimTextOutput("growth_info")
      )
    )
  ),
  
  # Table Ranking Tab
  tabPanel(
    title = "State Rankings",
    titlePanel("Best and worst states for each category"),
    fluidRow(
      column(8,
             radioButtons(
               inputId = "subcategory_rank", label = "Category",
               choices = sort(unique(sales$Subcategory)),
               inline = TRUE
             )
      ),
      column(4,
             sliderInput(
               inputId = "year",
               label = "Year",
               value = max(sales$Year),
               min = min(sales$Year),
               max = max(sales$Year)
             )
      )
    ),
    
    hr(),
    
    fluidRow(
      column(12,
             h4("Most Profitable States"),
             plotlyOutput("ranking_best") |> 
               withSpinner()
      ),
    ),
    
    br(),
    
    fluidRow(
      column(12,
             h4("Least Profitable States"),
             plotlyOutput("ranking_worst") |> 
               withSpinner()
      ),
    ),
    
    hr(),
    
    fluidRow(
      column(12,
             actionButton(inputId = "table_toggle",
                          label = "Show detailed table",
                          icon = icon("table"),
                          class = "btn-primary"),
             br(),
             hidden(
               div(
                 id = "detailed_table_container",
                 br(),
                 p("Note: States with no units sold are excluded"),
                 DTOutput("detailed_table") |> 
                   withSpinner()
               ))
      )
    )
  ),
  
  # Information Tab
  tabPanel(
    title = "About",
    includeMarkdown("about.Rmd")
  )
)

server <- function(input, output, session) {
  observeEvent(input$dark_mode, {
    session$setCurrentTheme(if (input$dark_mode) dark else light)
    
    thematic_on(
      bg = if (input$dark_mode) "#222222" else "#E3E3E3",
      fg = if (input$dark_mode) "#FFF" else "#111",
      font = "Poppins"
    )
  })
  
  cat_selection <- reactive({
    sales |>
      filter(Subcategory == input$subcategory)
  })
  
  # Update choices based on selected category
  observeEvent(
    eventExpr = input$subcategory,
    handlerExpr = {
      updateSelectInput(inputId = "state", choices = sort(unique(cat_selection()$State)))
    }
  )
  
  # Display Profitability Plot
  plot_data <- reactive({
    sales |>
      filter(Subcategory == input$subcategory) |>
      group_by(State, Year) |>
      summarize(Average_Margin = round(mean(Profit_Margin) * 100, 2),
                .groups = "drop") |>
      # Populates states with no data for given years with 0
      pivot_wider(names_from = Year, values_from = Average_Margin) |>
      mutate(across(-State, ~ coalesce(., 0))) |>
      pivot_longer(cols = -State, names_to = "Year", values_to = "Average_Margin") |>
      mutate(Year = as.double(Year)) |>
      filter(State == input$state) |>
      arrange(Year) |>
      mutate(Growth_Rate = growth_rate(Average_Margin))
  })
  
  output$profit_plot <- renderPlot({
    plot_data() |>
      ggplot(aes(Year, Average_Margin)) +
      geom_line(color = if (input$dark_mode) "#64B5F6" else "#E53935") +
      geom_point(color = if (input$dark_mode) "#FFF" else "#111") +
      labs(
        title = paste("Profitability of", input$subcategory, "in", input$state, sep = " "),
        y = "Average Profitability Margin (%)",
        caption = "0 values indicate missing data for the corresponding year."
      ) +
      theme(
        plot.title = element_text(size = 16),   
        axis.title.y = element_text(size = 14),   
        plot.caption = element_text(size = 12),
        panel.grid.major = element_line(
          color = if (input$dark_mode) "#444444" else "#ADADAD", 
          size = 0.5  
        ),
        panel.grid.minor = element_line(
          color = if (input$dark_mode) "#333333" else "#C4C4C4",
          size = 0.3
        )
      ) +
      expand_limits(y = 0)
  })
  
  output$growth_info <- renderPrint({
    req(input$plot_click)
    click <- input$plot_click
    near_point <- nearPoints(plot_data(), click, xvar = "Year", yvar = "Average_Margin")
    if (nrow(near_point) > 0) {
      if (near_point$Year == min(plot_data()$Year)) {
        cat("No prior year information for calculating growth rate.")
      } else if (!(is.na(near_point$Growth_Rate))) {
        cat("Profitability changed by ", near_point$Growth_Rate, "% from ",
            (near_point$Year - 1), " to ", near_point$Year, ".",
            sep = ""
        )
      } else {
        cat("Growth rate not available due to previous year's 0 margin.")
      }
    } else {
      cat("Click on a point.")
    }
  })
  
  ranking_data <- reactive({
    sales |>
      filter(Subcategory == input$subcategory_rank) |>
      group_by(State, Year) |>
      summarize(
        Average_Margin = round(mean(Profit_Margin) * 100, 2),
        Total_Units = as.integer(sum(Quantity)),
        .groups = "drop"
      ) |>
      arrange(State, Year) |> 
      mutate("Growth_Rate" = round(growth_rate(Average_Margin), 2)) |>
      arrange(desc(Average_Margin), State) |>
      filter(Year == input$year) |>
      select(State, Average_Margin, Growth_Rate, Total_Units)
  })
  
  # Display Rank Plots
  output$ranking_best = renderPlotly({
    plot_rank(ranking_data(), top = TRUE, input$dark_mode, input$subcategory_rank, input$year)
  })
  
  output$ranking_worst = renderPlotly({
    plot_rank(ranking_data(), top = FALSE, input$dark_mode, input$subcategory_rank, input$year)
  })
  
  output$detailed_table = renderDT({
    datatable(
      ranking_data(),
      extensions = c('Buttons'),
      options = list(
        dom = 'Bflrtip', 
        buttons = list(
          list(extend = 'copy', text = 'Copy', className = 'btn-primary'),
          list(extend = 'csv', text = 'CSV', className = 'btn-primary'),
          list(extend = 'excel', text = 'Excel', className = 'btn-primary'),
          list(extend = 'pdf', text = 'PDF', className = 'btn-primary'),
          list(extend = 'print', text = 'Print', className = 'btn-primary')
        ),
        pageLength = 10,
        lengthMenu = c(5, 10, 15, 20, 50),
        scrollX = TRUE
      ),
    )
  })
  
  observeEvent(input$table_toggle, {
    toggle("detailed_table_container")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
