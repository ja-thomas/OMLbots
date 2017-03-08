library(ggplot2)
library(tidyr)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(DT)

server = function(input, output) {
  
  load(file = "./results.RData")
  load(file = "./hyperpars.RData")
  
  output$dataset = renderUI({
    selectInput('ds', 'Dataset', unique(results$data.name), selected = unique(results$data.name)[1], multiple = FALSE)
  })
  
  output$learner = renderUI({
    #Filter: df1 = filter(runs, data.name %in% input$ds)
    selectInput('lrn', 'Learner', unique(results[results$data.name %in% input$ds, ]$flow.name), selected = unique(results[results$data.name %in% input$ds, ]$flow.name)[1], multiple = FALSE)
  })
  
  hyperparTable = reactive({
    hyperpars[hyperpars$run.id %in% unique(results[results$flow.name == input$lrn & results$data.name == input$ds,]$run.id), ]
  })
  
  output$hyperpars = renderUI({
    selectInput('hyp', 'Hyperparameter', unique(hyperparTable()$hyperpar.name), selected = unique(hyperparTable()$hyperpar.name)[1], multiple = TRUE)
  })

  output$learnerTable = DT::renderDataTable({
    hyperparValues()
  })
  
  hyperparTable = reactive({
    hyperpars[hyperpars$run.id %in% unique(results[results$flow.name == input$lrn & results$data.name == input$ds,]$run.id), ]
  })
  
  hyperparValues = reactive({
    #tidyr::spread(hyperpars, hyperpar.name, hyperpar.value, fill = NA)
    df = reshape(hyperparTable(), idvar = "run.id", timevar = "hyperpar.name", direction = "wide")
    colnames(df) = gsub("hyperpar.value.", "", colnames(df))
    df
  })
  
  output$summary.vis.hist = renderUI({
    list(
      column(9,
        sliderInput("summary.vis.hist.nbins", "Number of bins", min = 1L, max = 100L,
          value = 30L, step = 1L, width = "95%")
      ),
      column(3,
        radioButtons("summary.vis.dens", "Show density?", choices = c("Yes", "No"),
          selected = "Yes", inline = TRUE)
      )
    )
  })
  
# Analog zu shinymlr  
  summary.vis.out = reactive({
    #reqAndAssign(summary.vis.var(), "feature")
    #reqAndAssign(input$summary.vis.dens, "density")
    #d = na.omit(data$data)
    barfill = "#3c8dbc"
    barlines = "#1d5a92"
    
    d = hyperparValues()
    feature = input$hyp

    summary.plot = ggplot(d, aes(x = as.numeric(as.character(d[, feature])))) + geom_histogram(colour = barlines, fill = barfill, stat = "bin", bins = input$summary.vis.hist.nbins)     
    #summary.plot = ggplot(data = d, aes(x = as.numeric(as.character(d[,feature])))) #+ 
      #geom_histogram(colour = barlines, fill = barfill, stat = "bin", bins = input$summary.vis.hist.nbins) + xlab(feature) #+
      #geom_vline(aes(xintercept = quantile(as.numeric(d[,feature]), 0.05)), color = "blue", size = 0.5, linetype = "dashed") +
      #geom_vline(aes(xintercept = quantile(as.numeric(d[,feature]), 0.95)), color = "blue", size = 0.5, linetype = "dashed") +
      #geom_vline(aes(xintercept = quantile(as.numeric(d[,feature]), 0.5)), color = "blue", size = 1, linetype = "dashed")
    #summary.plot = addPlotTheme(summary.plot)
    summary.plot
    #     if (density == "Yes")
    #      summary.plot = summary.plot + geom_density(fill = "blue", alpha = 0.1)
    #   summary.plot
    #} else {
    #  summary.plot = ggplot(data = d, aes(x = d[,feature])) + 
    #    geom_bar(aes(fill = d[,feature]), stat = "count") + xlab(feature) +
    #    guides(fill = FALSE)
    #  summary.plot = addPlotTheme(summary.plot)
    #  summary.plot
    #}
    #} else if (length(feature) > 1L) {
    #  summary.plot = ggpairs(data = d, columns = input$summary.datatable_rows_selected,
    #   upper = list(continuous = wrap("cor", size = 10)), 
    #    lower = list(continuous = "smooth"))
    # summary.plot
    #}
  })
  
  output$summary.vis = renderPlot({
    summary.vis.out()
  })
}

ui = fluidPage(
  titlePanel("Coverage of hyperparameters"),
  
  uiOutput("dataset"),
  uiOutput("learner"),
  uiOutput("hyperpars"),
  
  mainPanel(
    DT::dataTableOutput("learnerTable")
  ),

  box(width = 12, title = "Variable Visualization", id = "summary.vis.box",
    fluidRow(
      column(12,
        uiOutput("summary.vis.hist")),
      column(12,
        plotOutput("summary.vis")
      )
    )
  )
  )

shinyApp(ui = ui, server = server)