###############################################################
# K-clustering
###############################################################

km_plots <- c("Density" = "density", "Bar" = "bar", "Scatter" = "scatter")
km_algorithm <- c("K-means" = "mean", "K-medians" = "median")

# list of function arguments
km_args <- as.list(formals(kclus))

km_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  km_args$data_filter <- if (input$show_filter) input$data_filter else ""
  km_args$dataset <- input$dataset
  for (i in r_drop(names(km_args)))
    km_args[[i]] <- input[[paste0("km_",i)]]
  km_args
})

output$ui_km_vars <- renderUI({

  ## are there any two-level vars
  dum <- two_level_vars()
  if (length(dum) > 0) {
    isVars <- .getclass() %in% c("integer","numeric","factor")
    isFct <- {.getclass() == "factor"} %>% {names(.[.])} %>% setdiff(., dum)
    vars <- varnames()[isVars] %>% .[!. %in% isFct]
  } else {
    isVars <- .getclass() %in% c("integer","numeric")
    vars <- varnames()[isVars]
  }

  selectInput(inputId = "km_vars", label = "Variables:", choices = vars,
	  selected = state_multiple("km_vars", vars, input$hc_vars),
	  multiple = TRUE, size = min(8, length(vars)), selectize = FALSE)
})

output$ui_kclus <- renderUI({
  req(input$dataset)
  tagList(
    wellPanel(
      actionButton("km_run", "Estimate", width = "100%")
    ),

    conditionalPanel(condition = "input.tabs_kclus == 'Plot'",
      wellPanel(
       selectInput("km_plots", label = "Plot(s):", choices = km_plots,
                   selected = state_multiple("km_plots", km_plots, "density"),
                   multiple = FALSE)
      )
    ),
  	wellPanel(
      selectInput("km_fun", label = "Algorithm:", choices = km_algorithm,
        selected = state_single("km_fun", km_algorithm, "mean"), multiple = FALSE),
	    uiOutput("ui_km_vars"),
		  checkboxInput(inputId = "km_hc_init", label = "Initial centers from HC",
      	value = state_init('km_hc_init',FALSE)),
	  	conditionalPanel(condition = "input.km_hc_init == true",
	  		wellPanel(
		  		selectInput("km_distance", label = "Distance measure:", choices = hc_distance,
			     	selected = state_single("km_distance", hc_distance, "sq.euclidian"), multiple = FALSE),
	  			selectInput("km_method", label = "Method:", choices = hc_method,
			     	selected = state_single("km_method", hc_method, "ward.D"), multiple = FALSE)
	  		)
	  	),
	  	conditionalPanel(condition = "input.km_hc_init == false",
		    numericInput("km_seed", "Set random seed:", min = 0,
		    	value = state_init('km_seed',1234))
		  ),
	    numericInput("km_nr_clus", "Number of clusters:", min = 2,
	    	value = state_init('km_nr_clus',2)),
      conditionalPanel(condition = "input.km_vars != null",
        tags$table(
          tags$td(textInput("km_store_name", "Store membership:", state_init("km_store_name","kclus"))),
          tags$td(actionButton("km_store", "Store"), style="padding-top:30px;")
        )
		  )
  	),
  	help_and_report(modal_title = "K-clustering",
  	                fun_name = "kclus",
  	                help_file = inclMD(file.path(getOption("radiant.path.multivariate"),"app/tools/help/kclus.md")))
	)
})

km_plot <- reactive({
  list(plot_width = 750, plot_height = 300 * length(input$km_vars) / 2)
})

km_plot_width <- function()
  km_plot() %>% { if (is.list(.)) .$plot_width else 650 }

km_plot_height <- function()
  km_plot() %>% { if (is.list(.)) .$plot_height else 400 }

# output is called from the main radiant ui.R
output$kclus <- renderUI({

		register_print_output("summary_kclus", ".summary_kclus")
		register_plot_output("plot_kclus", ".plot_kclus",
                         	width_fun = "km_plot_width",
                         	height_fun = "km_plot_height")

	  km_output_panels <- tabsetPanel(
	    id = "tabs_kclus",
	    tabPanel("Summary",
        downloadLink("dl_km_means", "", class = "fa fa-download alignright"), br(),
        verbatimTextOutput("summary_kclus")),
	    tabPanel("Plot",
        plot_downloader("kclus", height = km_plot_height()),
        plotOutput("plot_kclus", height = "100%"))
	  )

		stat_tab_panel(menu = "Multivariate > Cluster",
		               tool = "K-clustering",
		               tool_ui = "ui_kclus",
		             	 output_panels = km_output_panels)
})

.km_available <- reactive({
  if (not_available(input$km_vars))
    return("This analysis requires one or more variables of type numeric or integer.\nIf these variable types are not available please select another dataset.\n\n" %>% suggest_data("toothpaste"))
  if (not_pressed(input$km_run)) return("** Press the Estimate button to generate the cluster solution **")

  "available"
})

.kclus <- eventReactive(input$km_run, {
  withProgress(message = "Estimating cluster solution", value = 1,
    do.call(kclus, km_inputs())
  )
})

.summary_kclus <- reactive({
  if (.km_available() != "available") return(.km_available())
  summary(.kclus())
})

.plot_kclus <- reactive({
  if (.km_available() != "available") return(.km_available())
  plot(.kclus(), plots = input$km_plots, shiny = TRUE)
})

observeEvent(input$kclus_report, {
  inp_out <- list(list(dec = 2), "")
  if (length(input$km_plots) > 0) {
    figs <- TRUE
    outputs <- c("summary","plot")
    inp_out[[2]] <- list(plots = input$km_plots, custom = FALSE)
  } else {
    outputs <- c("summary")
    figs <- FALSE
  }
  update_report(inp_main = clean_args(km_inputs(), km_args),
                fun_name = "kclus", inp_out = inp_out,
                outputs = outputs, figs = figs,
                fig.width = km_plot_width(),
                fig.height = km_plot_height(),
                xcmd = paste0("# store(result, name = \"", input$km_store_name,"\")\n# write.csv(result$clus_means, file = \"~/kclus.csv\")"))
})

output$dl_km_means <- downloadHandler(
  filename = function() { "kclus.csv" },
  content = function(file) {
    if (pressed(input$km_run)) {
      .kclus() %>% { if (is.list(.)) write.csv(.$clus_means, file = file) }
    } else {
      cat("No output available. Press the Estimate button to generate the cluster solution", file = file)
    }
  }
)

## store cluster membership
observeEvent(input$km_store, {
  if (pressed(input$km_run)) {
    .kclus() %>% { if (is.list(.)) store(., name = input$km_store_name) }
  }
})
