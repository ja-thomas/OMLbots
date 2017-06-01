library(ggplot2)
library(tidyr)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(DT)
library(plotly)

server = function(input, output) {
  
  load(file = "./results.RData")
  load(file = "./hyperpars.RData")
  
  output$count.learners = renderPlotly({
    p = ggplot(results[results$measure.name == "area.under.roc.curve",]) + theme_light()
    p + geom_bar(aes(flow.name)) + coord_flip() + ggtitle("Count of learners")
  })
  
  output$count.datasets = renderPlotly({
    p = ggplot(results[results$measure.name == "area.under.roc.curve",]) + theme_light()
    p + geom_bar(aes(data.name)) + coord_flip() + ggtitle("Count of datasets")
  })
  
  output$dataset = renderUI({
    selectInput('ds', 'Dataset', unique(tbl.results$data.name), selected = unique(tbl.results$data.name)[1], multiple = FALSE)
  })
  
  output$learner = renderUI({
    #Filter: df1 = filter(runs, data.name %in% input$ds)
    selectInput('lrn', 'Learner', unique(tbl.results[tbl.results$data.name %in% input$ds, ]$flow.name), selected = unique(tbl.results[tbl.results$data.name %in% input$ds, ]$flow.name)[1], multiple = FALSE)
  })
  
  hyperparTable = reactive({
    tbl.hypPars[tbl.hypPars$run.id %in% unique(tbl.results[tbl.results$flow.name == input$lrn & tbl.results$data.name == input$ds,]$run.id), ]
  })
  
  output$hyperpars = renderUI({
    selectInput('hyp', 'Hyperparameter', unique(hyperparTable()$hyperpar.name), selected = unique(hyperparTable()$hyperpar.name)[1], multiple = TRUE)
  })

  output$learnerTable = DT::renderDataTable({
    hyperparValues()
  })
  
  hyperparValues = reactive({
    tidyr::spread(hyperparTable(), hyperpar.name, hyperpar.value, fill = NA)
    # df = reshape(hyperparTable(), idvar = "run.id", timevar = "hyperpar.name", direction = "wide")
    # colnames(df) = gsub("hyperpar.value.", "", colnames(df))
    # df
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
    
    summary.plot = ggplot(d, aes(x = as.numeric(d %>% collect %>% .[[feature]]))) + geom_histogram(colour = barlines, fill = barfill, stat = "bin", bins = input$summary.vis.hist.nbins)
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

  output$summary.vis = renderPlotly({
    summary.vis.out()
  })
}

ui = fluidPage(
  
  titlePanel("Summary of the runs of the OMLBot"),
  
  tabsetPanel(
    tabPanel("Count of learners",
      plotlyOutput("count.learners", width = "auto", height = "auto")
    ),
    tabPanel("Count of datasets",
      plotlyOutput("count.datasets", width = "auto" ,height = "600px")
    ),
    
    tabPanel("Coverage of hyperparameters",
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
            plotlyOutput("summary.vis")
          )
        )
      )
    )
    
  )
)

shinyApp(ui = ui, server = server)