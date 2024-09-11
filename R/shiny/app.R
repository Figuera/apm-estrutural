library(shiny)
library(plotly)
library(ggplot2)

revenues <- list(
  nominal_revenues  = process_revenue(valor_historico),
  deflated_revenues = process_revenue(valor_atualizado)
)

server <- function(input, output, session) {
  output$revenue_plot <- renderPlotly({
    ggplotly(
      revenues[[input$selectedCurrency]] %>%
        gather("key", "value", -periodo) %>%
        filter(key %in% input$selectedRevenue) %>%
        ggplot(aes(x = periodo, y = value, color = key)) + geom_line()
    )
  })
}
