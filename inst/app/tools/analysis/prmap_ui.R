#########################################
# Perceptual map using factor analysis
#########################################
pm_nr_dim <- c("2 dimensions" = 2, "3 dimensions" = 3)

## list of function arguments
pm_args <- as.list(formals(prmap))

## list of function inputs selected by user
pm_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  pm_args$data_filter <- if (input$show_filter) input$data_filter else ""
  pm_args$dataset <- input$dataset
  for (i in r_drop(names(pm_args)))
    pm_args[[i]] <- input[[paste0("pm_", i)]]
  pm_args
})

pm_plot_args <- as.list(if (exists("plot.prmap")) {
  formals(plot.prmap)
} else {
  formals(radiant.multivariate:::plot.prmap)
} )

## list of function inputs selected by user
pm_plot_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(pm_plot_args))
    pm_plot_args[[i]] <- input[[paste0("pm_", i)]]
  pm_plot_args
})

output$ui_pm_brand <- renderUI({
  isLabel <- "character" == .get_class() | "factor" == .get_class()
  vars <- varnames()[isLabel]
  selectInput(
    inputId = "pm_brand", label = "Brand:", choices = vars,
    selected = state_single("pm_brand", vars), multiple = FALSE
  )
})

output$ui_pm_attr <- renderUI({
  isNum <- "numeric" == .get_class() | "integer" == .get_class()
  vars <- varnames()[isNum]
  selectInput(
    inputId = "pm_attr", label = "Attributes:", choices = vars,
    selected = state_multiple("pm_attr", vars), multiple = TRUE,
    size = min(10, length(vars)), selectize = FALSE
  )
})

output$ui_pm_pref <- renderUI({
  if (not_available(input$pm_attr)) return()
  isNum <- "numeric" == .get_class() | "integer" == .get_class()
  vars <- varnames()[isNum]
  if (length(vars) > 0) vars <- vars[-which(vars %in% input$pm_attr)]
  selectInput(
    inputId = "pm_pref", label = "Preferences:", choices = vars,
    selected = state_multiple("pm_pref", vars), multiple = TRUE,
    size = max(1, min(5, length(vars))), selectize = FALSE
  )
})

output$ui_pm_plots <- renderUI({
  plot_list <- c("Brands" = "brand", "Attributes" = "attr")
  if (!is.null(input$pm_pref)) plot_list <- c(plot_list, c("Preferences" = "pref"))
  checkboxGroupInput(
    "pm_plots", NULL, plot_list,
    selected = state_group("pm_plots"),
    inline = TRUE
  )
})

output$ui_pm_store_name <- renderUI({
  req(input$dataset)
  textInput("pm_store_name", "Store factor scores:", "", placeholder = "Provide single variable name")
})

observe({
  ## dep on most inputs
  input$data_filter
  input$show_filter
  # sapply(r_drop(names(pm_args)), function(x) input[[paste0("pm_", x)]])
  sapply(names(pm_args), function(x) input[[paste0("pm_", x)]])

  ## notify user when the model needs to be updated
  ## based on https://stackoverflow.com/questions/45478521/listen-to-reactive-invalidation-in-shiny
  if (pressed(input$pm_run)) {
    if (is.null(input$pm_brand) && is.null(input$pm_attr)) {
      updateTabsetPanel(session, "tabs_prmap", selected = "Summary")
      updateActionButton(session, "pm_run", "Estimate model", icon = icon("play"))
    } else if (isTRUE(attr(pm_inputs, "observable")$.invalidated)) {
      updateActionButton(session, "pm_run", "Re-estimate model", icon = icon("refresh", class = "fa-spin"))
    } else {
      updateActionButton(session, "pm_run", "Estimate model", icon = icon("play"))
    }
  }
})

output$ui_prmap <- renderUI({
  req(input$dataset)
  tagList(
    wellPanel(
      actionButton("pm_run", "Estimate model", width = "100%", icon = icon("play"), class = "btn-success")
    ),
    wellPanel(
      conditionalPanel(
        condition = "input.tabs_prmap == 'Summary'",
        uiOutput("ui_pm_brand"),
        uiOutput("ui_pm_attr"),
        uiOutput("ui_pm_pref"),
        radioButtons(
          inputId = "pm_nr_dim", label = NULL, pm_nr_dim,
          selected = state_init("pm_nr_dim", 2),
          inline = TRUE
        ),
        numericInput(
          "pm_cutoff", label = "Loadings cutoff:", min = 0,
          max = 1, state_init("pm_cutoff", 0), step = .05
        ),
        conditionalPanel(
          condition = "input.pm_attr != null",
          tags$table(
            tags$td(uiOutput("ui_pm_store_name")),
            tags$td(actionButton("pm_store", "Store", icon = icon("plus")), style = "padding-top:30px;")
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabs_prmap == 'Plot'",
        uiOutput("ui_pm_plots"),
        tags$table(
          tags$td(numericInput("pm_scaling", "Arrow scale:", state_init("pm_scaling", 2), .5, 4, .1, width = "117px")),
          tags$td(numericInput("pm_fontsz", "Font size:", state_init("pm_fontsz", 5), 1, 20, 1, width = "117px")),
          width = "100%"
        )
      )
    ),
    help_and_report(
      modal_title = "Attribute based brand maps",
      fun_name = "prmap",
      help_file = inclMD(file.path(getOption("radiant.path.multivariate"), "app/tools/help/prmap.md"))
    )
  )
})

pm_plot <- eventReactive(input$pm_run, {
  req(input$pm_nr_dim)
  nrDim <- as.numeric(input$pm_nr_dim)
  nrPlots <- (nrDim * (nrDim - 1)) / 2
  list(plot_width = 650, plot_height = 650 * nrPlots)
})

pm_plot_width <- function()
  pm_plot() %>% {if (is.list(.)) .$plot_width else 650}

pm_plot_height <- function()
  pm_plot() %>% {if (is.list(.)) .$plot_height else 650}

output$prmap <- renderUI({
  register_print_output("summary_prmap", ".summary_prmap")
  register_plot_output(
    "plot_prmap", ".plot_prmap",
    width_fun = "pm_plot_width",
    height_fun = "pm_plot_height"
  )

  pm_output_panels <- tabsetPanel(
    id = "tabs_prmap",
    tabPanel(
      "Summary",
      download_link("dl_pm_loadings"), br(),
      verbatimTextOutput("summary_prmap")
    ),
    tabPanel(
      "Plot",
      download_link("dlp_prmap"),
      plotOutput("plot_prmap", height = "100%")
    )
  )

  stat_tab_panel(
    menu = "Multivariate > Maps",
    tool = "Attributes",
    tool_ui = "ui_prmap",
    output_panels = pm_output_panels
  )
})

.prmap_available <- reactive({
  if (not_pressed(input$pm_run)) {
    "** Press the Estimate button to generate perceptual maps **"
  } else if (not_available(input$pm_brand) || not_available(input$pm_attr)) {
    "This analysis requires a brand variable of type factor or character and multiple attribute variables\nof type numeric or integer. If these variables are not available please select another dataset.\n\n" %>%
      suggest_data("retailers")
  } else if (length(input$pm_attr) < 2) {
    "Please select two or more attribute variables"
  } else {
    brand <- .get_data()[[input$pm_brand]]
    if (length(unique(brand)) < length(brand)) {
      "Number of observations and unique IDs for the brand variable do not match.\nPlease choose another brand variable or another dataset.\n\n" %>%
        suggest_data("retailers")
    } else {
      "available"
    }
  }
})

.prmap <- eventReactive(input$pm_run, {
  withProgress(
    message = "Generating perceptual map", value = 1,
    do.call(prmap, pm_inputs())
  )
})

.summary_prmap <- reactive({
  if (.prmap_available() != "available") return(.prmap_available())
  summary(.prmap(), cutoff = input$pm_cutoff)
})

.plot_prmap <- eventReactive({
  c(input$pm_run, pm_plot_inputs())
}, {
  if (.prmap_available() != "available") return(.prmap_available())
  req("pm_plots" %in% names(input))
  robj <- .prmap()
  if (is.character(robj)) return(robj)
  withProgress(message = "Generating brand maps", value = 1, {
    do.call(plot, c(list(x = robj), pm_plot_inputs(), shiny = TRUE))
  })
})

observeEvent(input$prmap_report, {
  outputs <- c("summary", "plot")
  inp_out <- list(list(cutoff = input$pm_cutoff, dec = 2), "")
  inp_out[[2]] <- clean_args(pm_plot_inputs(), pm_plot_args[-1])
  inp <- clean_args(pm_inputs(), pm_args)
  if (!is_empty(inp$nr_dim)) inp$nr_dim <- as_integer(inp$nr_dim)
  if (!is_empty(input$pm_store_name)) {
    xcmd <- paste0(input$dataset, " <- store(",
      input$dataset, ", result, name = \"", input$pm_store_name, "\")"
    )
  } else {
    xcmd <- ""
  }
  update_report(
    inp_main = inp,
    fun_name = "prmap",
    inp_out = inp_out,
    fig.width = pm_plot_width(),
    fig.height = pm_plot_height(),
    xcmd = xcmd
  )
})

## store factor scores
observeEvent(input$pm_store, {
  req(input$pm_store_name, input$pm_run)
  robj <- .prmap()
  if (!is.character(robj)) {
    withProgress(
      message = "Storing factor scores", value = 1,
      r_data[[input$dataset]] <- store(r_data[[input$dataset]], robj, name = input$pm_store_name)
    )
  }
})

dl_pm_loadings <- function(path) {
  if (pressed(input$pm_run)) {
    .prmap() %>%
      {if (is.list(.)) .$fres$loadings else return()} %>%
      clean_loadings(input$pm_cutoff, fsort = FALSE) %>%
      write.csv(file = path)
  } else {
    cat("No output available. Press the Estimate button to generate the factor analysis results", file = path)
  }
}

download_handler(
  id = "dl_pm_loadings",
  fun = dl_pm_loadings,
  fn = paste0(input$dataset, "_prmap_loadings.csv"),
  caption = "Download factor loadings"
)

download_handler(
  id = "dlp_prmap",
  fun = download_handler_plot,
  fn = paste0(input$dataset, "_prmap.png"),
  caption = "Download preceptual map plot",
  plot = .plot_prmap,
  width = pm_plot_width,
  height = pm_plot_height
)
