library(shiny)
library(plotly)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Brazil's Structural Balance"),
  # helpText("Take the role of a CEO and navigate a market of innovation and imitation."),
  sidebarPanel(
    tabsetPanel(
      id = "settingsTabs",
      tabPanel(
        title = "Revenues",
        checkboxGroupInput("selectedRevenue", "Revenue: ", choices =
          list("TRT" = "trt", "TFP" = "tfp", "TRC" = "trc", "TI" = "ti",
            "TM" = "tm", "TRAN" = "tran", "ICMS" = "icms")),
        selectInput("selectedCurrency", "Currency: ", choices =
          list("Historic Real" = "nominal_revenues", "Constant Real" = "deflated_revenues"))
        )
      )
  ),
  mainPanel(
    plotlyOutput(outputId = "revenue_plot")
  )
)

shinyApp(ui, server, options = list(port = 3838))
