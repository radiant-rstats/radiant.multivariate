###############################
# Pre-factor analysis
###############################

pf_plots <- c("Scree" = "scree", "Change" = "change")

## list of function arguments
pf_args <- as.list(formals(pre_factor))

## list of function inputs selected by user
pf_inputs <- reactive({
  pf_args$data_filter <- if (input$show_filter) input$data_filter else ""
  pf_args$dataset <- input$dataset
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in r_drop(names(pf_args)))
    pf_args[[i]] <- input[[paste0("pf_", i)]]
  pf_args
})

output$ui_pf_vars <- renderUI({
  isNum <- "numeric" == .get_class() | "integer" == .get_class()
  vars <- varnames()[isNum]
  selectInput(
    inputId = "pf_vars", label = "Variables:", choices = vars,
    selected = state_multiple("pf_vars", vars),
    multiple = TRUE, size = min(15, length(vars)), selectize = FALSE
  )
})

observe({
  ## dep on most inputs
  input$data_filter
  input$show_filter
  sapply(r_drop(names(pf_args)), function(x) input[[paste0("pf_", x)]])

  ## notify user when the model needs to be updated
  ## based on https://stackoverflow.com/questions/45478521/listen-to-reactive-invalidation-in-shiny
  if (pressed(input$pf_run)) {
    if (is.null(input$pf_vars)) { 
      updateTabsetPanel(session, "tabs_pre_factor", selected = "Summary")
      updateActionButton(session, "pf_run", "Estimate model", icon = icon("play"))
    } else if (isTRUE(attr(pf_inputs, "observable")$.invalidated)) {
      updateActionButton(session, "pf_run", "Re-estimate model", icon = icon("refresh", class = "fa-spin"))
    } else {
      updateActionButton(session, "pf_run", "Estimate model", icon = icon("play"))
    }
  }
})

output$ui_pre_factor <- renderUI({
  req(input$dataset)
  tagList(
    wellPanel(
      actionButton("pf_run", "Estimate model", width = "100%", icon = icon("play"), class = "btn-success")
    ),
    wellPanel(
      conditionalPanel(
        condition = "input.tabs_pre_factor == 'Summary'",
        uiOutput("ui_pf_vars")
      ),
      conditionalPanel(
        condition = "input.tabs_pre_factor == 'Plot'",
        selectizeInput(
          "pf_plots", label = "Plot(s):", choices = pf_plots,
          selected = state_multiple("pf_plots", pf_plots, c("scree", "change")),
          multiple = TRUE,
          options = list(
            placeholder = "Select plot(s)",
            plugins = list("remove_button", "drag_drop")
          )
        ),
        numericInput("pf_cutoff", "Plot cutoff:", min = 0, max = 2, value = state_init("pf_cutoff", 0.1), step = .05)
      )
    ),
    help_and_report(
      modal_title = "Pre-factor analysis",
      fun_name = "pre_factor",
      help_file = inclMD(file.path(getOption("radiant.path.multivariate"), "app/tools/help/pre_factor.md"))
    )
  )
})

pf_plot <- reactive({
  list(plot_width = 600, plot_height = length(input$pf_plots) * 400)
})

pf_plot_width <- function()
  pf_plot() %>% {if (is.list(.)) .$plot_width else 600}

pf_plot_height <- function()
  pf_plot() %>% {if (is.list(.)) .$plot_height else 400}

output$pre_factor <- renderUI({
  register_print_output("summary_pre_factor", ".summary_pre_factor")
  register_plot_output(
    "plot_pre_factor", ".plot_pre_factor",
    width_fun = "pf_plot_width",
    height_fun = "pf_plot_height"
  )

  ## two outputs in a summary and plot tab
  pf_output_panels <- tabsetPanel(
    id = "tabs_pre_factor",
    tabPanel("Summary", verbatimTextOutput("summary_pre_factor")),
    tabPanel(
      "Plot",
      download_link("dlp_pre_factor"),
      plotOutput("plot_pre_factor", height = "100%")
    )
  )

  stat_tab_panel(
    menu = "Multivariate > Factor",
    tool = "Pre-factor",
    tool_ui = "ui_pre_factor",
    output_panels = pf_output_panels
  )
})

.pre_factor <- eventReactive(input$pf_run, {
  withProgress(
    message = "Estimating factor solution", value = 1,
    do.call(pre_factor, pf_inputs())
  )
})

.summary_pre_factor <- reactive({
  if (not_pressed(input$pf_run)) return("** Press the Estimate button to generate factor analysis diagnostics **")
  isolate({
    if (not_available(input$pf_vars)) {
      return("This analysis requires multiple variables of type numeric or integer.\nIf these variables are not available please select another dataset.\n\n" %>% suggest_data("toothpaste"))
    } else if (length(input$pf_vars) < 2) {
      return("Please select two or more numeric variables")
    }
  })
  summary(.pre_factor())
})

.plot_pre_factor <- reactive({
  if (not_pressed(input$pf_run)) return("** Press the Estimate button to generate factor analysis diagnostics **")
  isolate({
    if (not_available(input$pf_vars)) {
      return("This analysis requires multiple variables of type numeric or integer.\nIf these variables are not available please select another dataset.\n\n" %>% suggest_data("toothpaste"))
    } else if (length(input$pf_vars) < 2) {
      return("Please select two or more numeric variables\nin the Summary tab and re-estimate the model")
    }
  })
  withProgress(message = "Generating factor plots", value = 1, {
    plot(.pre_factor(), plots = input$pf_plots, cutoff = input$pf_cutoff, shiny = TRUE)
  })
})

observeEvent(input$pre_factor_report, {
  inp_out <- list(list(dec = 2), "")
  if (length(input$pf_plots) > 0) {
    figs <- TRUE
    outputs <- c("summary", "plot")
    inp_out[[2]] <- list(plots = input$pf_plots, custom = FALSE)
  } else {
    outputs <- c("summary")
    figs <- FALSE
  }
  update_report(
    inp_main = clean_args(pf_inputs(), pf_args),
    fun_name = "pre_factor", 
    inp_out = inp_out,
    outputs = outputs, 
    figs = figs,
    fig.width = pf_plot_width(),
    fig.height = pf_plot_height()
  )
})

download_handler(
  id = "dlp_pre_factor", 
  fun = download_handler_plot, 
  fn = function() paste0(input$dataset, "_pre_factor"),
  type = "png",
  caption = "Save pre-factor plot",
  plot = .plot_pre_factor,
  width = pf_plot_width,
  height = pf_plot_height
)
