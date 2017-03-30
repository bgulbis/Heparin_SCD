# Shiny app to explore vancomycin data for CS&E project

library(shiny)
library(tidyverse)
library(lazyeval)
library(plotly)

x <- dirr::get_rds("data/final")
rm(x)


hvi <- c("HH CVICU", "HH CVIMU", "HH HFIC", "HH HFIM", "HH 5HVI", "HH CCU", "HVI CIMU")
# hvi <- "HH CVICU"

times <- c("hep.time.wt.avg", "ptt.time.wt.avg", "temp.time.wt.avg")
group_by <- c("group")
plots <- c(Scatter = "scatter", Histogram = "histogram", `Box Plot` = "box")
cat_x <- c("group")

ui <- fluidPage(

    headerPanel("Data Exploration"),
    sidebarPanel(
        # sliderInput('sampleSize', 'Sample Size', min = 1, max = nrow(diamonds), value = 1000, step = 500, round = 0),
        selectInput("plot", "Plot Type", choices = plots, selected = "scatter"),
        selectInput('x', 'X', choices = times, selected = "hep.time.wt.avg"),
        selectInput('y', 'Y', choices = times, selected = "ptt.time.wt.avg"),
        sliderInput("bins", "Bins", min = 1, max = 50, value = 20),
        selectInput('color', 'Color', choices = c(None = ".", group_by)),
        selectInput('split', 'Split', choices = c(None = ".", group_by)),
        selectInput('filter', 'Location Filter', choices = c(HVI = ".", hvi))
        # selectInput('facet_row', 'Facet Row', c(None = '.', group_by)),
        # selectInput('facet_col', 'Facet Column', c(None = '.', group_by))
        # sliderInput('plotHeight', 'Height of plot (in pixels)', min = 100, max = 2000, value = 1000)
    ),
    mainPanel(
        plotlyOutput("trendPlot")
        # textOutput("xaxis")
    )
)

server <- function(input, output, session) {

    observe({
        if (input$plot == "box") {
            updateSelectInput(session, "x", choices = cat_x)
        } else {
            updateSelectInput(session, "x", choices = times, selected = "hep.time.wt.avg")
        }

        if (input$plot == "histogram") {
            updateSelectInput(session, "y", choices = character(0))
        } else {
            updateSelectInput(session, "y", choices = times, select = "ptt.time.wt.avg")
        }
    })

    #add reactive data information. Dataset = built in diamonds data
    dataset <- reactive({
        if (input$filter != ".") {
            dplyr::filter_(data_mbo, .dots = list(~location == input$filter))
        } else {
            data_mbo
        }
    })

    # output$xaxis <- renderText(length(dataset()[[input$x]]))
    # output$xaxis <- renderText(input$bins)
    output$trendPlot <- renderPlotly({

        if (input$color == ".") {
            color = NULL
        } else {
            color = interp(~factor(color), color = as.name(input$color))
        }

        if (input$split == ".") {
            split = NULL
        } else {
            split = interp(~factor(split), split = as.name(input$split))
        }

        p <- plot_ly(dataset(),
                color = color,
                split = split)

        if (input$plot == "histogram") {
            xval <- dataset()[[input$x]]
            xbins <- list(
                start = min(xval),
                end = max(xval),
                size = (max(xval) - min(xval)) / input$bins
            )

            add_histogram(p,
                          x = interp(~x, x = as.name(input$x)),
                          nbinsx = input$bins
                          # autobinx = FALSE,
                          # xbins = xbins
                          # xbins = list(size = interp(~bins, bins = as.name(input$bins)))
            )
        } else if (input$plot == "box") {
            add_boxplot(p,
                        x = interp(~x, x = as.name(input$x)),
                        y = interp(~y, y = as.name(input$y))
            )
        } else {
            add_markers(p,
                        x = interp(~x, x = as.name(input$x)),
                        y = interp(~y, y = as.name(input$y)),
                        marker = list(symbol = "circle-open"))
        }
            # layout(xaxis = list(range = c(-3, 48)),
                   # yaxis = list(range = c(-3, 48)))


        # build graph with ggplot syntax
        # p <- ggplot(dataset(), aes_string(x = input$x, y = input$y, color = input$color)) +
        #     geom_point(shape = 1) +
        #     scale_color_brewer(palette = "Set1") +
        #     theme_bg()
        #
        # # if at least one facet column/row is specified, add it
        # facets <- paste(input$facet_row, '~', input$facet_col)
        # if (facets != '. ~ .') p <- p + facet_grid(facets)
        # #
        # ggplotly(p) %>%
        #     layout(xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE)) # height = input$plotHeight,

    })

}

shinyApp(ui, server)
