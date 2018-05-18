hc_method <- list(
  "Ward's" = "ward.D", "Single" = "single", "Complete" = "complete", "Average" = "average",
  "McQuitty" = "mcquitty", "Median" = "median", "Centroid" = "centroid"
)

hc_distance <- c(
  "Squared euclidean" = "sq.euclidian", "Euclidian" = "euclidean",
  "Maximum" = "maximum", "Manhattan" = "manhattan", "Canberra" = "canberra", "Binary" = "binary", "Minkowski" = "minkowski"
)

hc_plots <- c("Scree" = "scree", "Change" = "change", "Dendrogram" = "dendro")

## list of function arguments
hc_args <- as.list(formals(hclus))

hc_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  hc_args$data_filter <- if (input$show_filter) input$data_filter else ""
  hc_args$dataset <- input$dataset
  for (i in r_drop(names(hc_args)))
    hc_args[[i]] <- input[[paste0("hc_", i)]]
  hc_args
})


###############################################################
# Hierarchical clustering
###############################################################
output$ui_hc_vars <- renderUI({
  isNum <- "numeric" == .get_class() | "integer" == .get_class()
  vars <- varnames()[isNum]
  selectInput(
    inputId = "hc_vars", label = "Variables:", choices = vars,
    selected = state_multiple("hc_vars", vars),
    multiple = TRUE, size = min(8, length(vars)), selectize = FALSE
  )
})

observe({
  ## dep on most inputs
  input$data_filter
  input$show_filter
  sapply(r_drop(names(hc_args)), function(x) input[[paste0("hc_", x)]])

  ## notify user when the model needs to be updated
  ## based on https://stackoverflow.com/questions/45478521/listen-to-reactive-invalidation-in-shiny
  if (pressed(input$hc_run)) {
    if (isTRUE(attr(hc_inputs, "observable")$.invalidated)) {
      updateActionButton(session, "hc_run", "Re-estimate model", icon = icon("refresh", class = "fa-spin"))
    } else {
      updateActionButton(session, "hc_run", "Estimate model", icon = icon("play"))
    }
  }
})

output$ui_hclus <- renderUI({
  req(input$dataset)
  tagList(
    wellPanel(
      actionButton("hc_run", "Estimate model", width = "100%", icon = icon("play"), class = "btn-success")
    ),
    wellPanel(
      uiOutput("ui_hc_vars"),
      selectInput(
        "hc_distance", label = "Distance measure:", choices = hc_distance,
        selected = state_single("hc_distance", hc_distance, "sq.euclidean"),
        multiple = FALSE
      ),
      selectInput(
        "hc_method", label = "Method:", choices = hc_method,
        selected = state_single("hc_method", hc_method, "ward.D"), multiple = FALSE
      ),
      selectizeInput(
        "hc_plots", label = "Plot(s):", choices = hc_plots,
        selected = state_multiple("hc_plots", hc_plots, c("scree", "change")),
        multiple = TRUE,
        options = list(
          placeholder = "Select plot(s)",
          plugins = list("remove_button", "drag_drop")
        )
      ),

      with(tags, table(
        tr(
          td(numericInput(
            "hc_cutoff", "Plot cutoff:", min = 0, max = 1,
            value = state_init("hc_cutoff", 0.05), step = .02, width = "117px"
          )),
          td(numericInput(
            "hc_max_cases", "Max cases:", min = 100, max = 100000, step = 100,
            value = state_init("hc_max_cases", 5000) 
          ))
        )
      ))
    ),
    help_and_report(
      modal_title = "Hierarchical cluster analysis",
      fun_name = "hclus",
      help_file = inclMD(file.path(getOption("radiant.path.multivariate"), "app/tools/help/hclus.md"))
    )
  )
})

## reset
observeEvent(input$hc_plots, {
  if (length(input$hc_plots) > 1 && "dendro" %in% input$hc_plots) {
    updateSelectInput(session = session, inputId = "hc_plots", selected = "dendro")
  }
})

hc_plot <- reactive({
  plots <- input$hc_plots
  req(plots)
  ph <- plots %>% 
    {if (length(.) == 1 && . == "dendro") 800 else 400}
  pw <- if (!is_empty(plots) && plots == "dendro") 900 else 650
  list(plot_width = pw, plot_height = ph * length(plots))
})

hc_plot_width <- function()
  hc_plot() %>% {if (is.list(.)) .$plot_width else 650}

hc_plot_height <- function()
  hc_plot() %>% {if (is.list(.)) .$plot_height else 400}

## output is called from the main radiant ui.R
output$hclus <- renderUI({
  register_print_output("summary_hclus", ".summary_hclus")
  register_plot_output(
    "plot_hclus", ".plot_hclus",
    width_fun = "hc_plot_width",
    height_fun = "hc_plot_height"
  )

  ## one output with components stacked
  hc_output_panels <- tagList(
    tabPanel("Summary", verbatimTextOutput("summary_hclus")),
    tabPanel(
      "Plot",
      download_link("dlp_hclus"),
      plotOutput("plot_hclus", height = "100%")
    )
  )

  stat_tab_panel(
    menu = "Multivariate > Cluster",
    tool = "Hierarchical",
    tool_ui = "ui_hclus",
    output_panels = hc_output_panels
  )
})

.hclus <- eventReactive(input$hc_run, {
  withProgress(
    message = "Estimating cluster solution", value = 1,
    do.call(hclus, hc_inputs())
  )
})

.summary_hclus <- reactive({
  if (not_available(input$hc_vars)) {
    "This analysis requires one or more variables of type integer or numeric.\nIf these variable types are not available please select another dataset.\n\n" %>% 
      suggest_data("toothpaste")
  } else if (not_pressed(input$hc_run))  {
    "** Press the Estimate button to generate cluster solution **"
  } else {
    summary(.hclus())
  }
})

.plot_hclus <- eventReactive({
  c(input$hc_run, input$hc_plots, input$hc_cutoff)
}, {
  if (length(input$hc_plots) > 1 && "dendro" %in% input$hc_plots) {
    invisible()
  } else {
    withProgress(
      message = "Generating cluster plot", value = 1,
      capture_plot(plot(.hclus(), plots = input$hc_plots, cutoff = input$hc_cutoff))
    )
  }
})

observeEvent(input$hclus_report, {
  if (length(input$hc_plots) > 0) {
    if (input$hc_cutoff != 0.05) {
      inp_out <- list("", list(plots = input$hc_plots, cutoff = input$hc_cutoff, custom = FALSE))
    } else {
      inp_out <- list("", list(plots = input$hc_plots, custom = FALSE))
    }
    outputs <- c("summary", "plot")
    figs <- TRUE
  } else {
    outputs <- c("summary")
    inp_out <- list("", "")
    figs <- FALSE
  }
  update_report(
    inp_main = clean_args(hc_inputs(), hc_args),
    fun_name = "hclus",
    inp_out = inp_out,
    outputs = outputs,
    figs = figs,
    fig.width = hc_plot_width(),
    fig.height = hc_plot_height()
  )
})

download_handler(
  id = "dlp_hclus", 
  fun = download_handler_plot, 
  fn = paste0(input$dataset, "_hclustering.png"),
  caption = "Download hierarchical cluster plots",
  plot = .plot_hclus,
  width = hc_plot_width,
  height = hc_plot_height
)
