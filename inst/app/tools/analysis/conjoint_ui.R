################################################################
# Conjoint regression - UI
################################################################

ca_show_interactions <- c("None" = "", "2-way" = 2, "3-way" = 3)
ca_predict <- c("None" = "none", "Data" = "data","Command" = "cmd", "Data & Command" = "datacmd")
ca_plots <- list("Part-worths" = "pw", "Importance-weights" = "iw")

# list of function arguments
ca_args <- as.list(formals(conjoint))

# list of function inputs selected by user
ca_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  ca_args$data_filter <- if (input$show_filter) input$data_filter else ""
  ca_args$dataset <- input$dataset
  for (i in r_drop(names(ca_args)))
    ca_args[[i]] <- input[[paste0("ca_",i)]]
  ca_args
})

ca_sum_args <- as.list(if (exists("summary.conjoint")) formals(summary.conjoint)
                        else formals(radiant.multivariate:::summary.conjoint))

## list of function inputs selected by user
ca_sum_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(ca_sum_args))
    ca_sum_args[[i]] <- input[[paste0("ca_",i)]]
  ca_sum_args
})

ca_plot_args <- as.list(if (exists("plot.conjoint")) formals(plot.conjoint)
                         else formals(radiant.multivariate:::plot.conjoint))

## list of function inputs selected by user
ca_plot_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(ca_plot_args))
    ca_plot_args[[i]] <- input[[paste0("ca_",i)]]
  ca_plot_args
})

ca_pred_args <- as.list(if (exists("predict.conjoint")) formals(predict.conjoint)
                         else formals(radiant.multivariate:::predict.conjoint))

## list of function inputs selected by user
ca_pred_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(ca_pred_args))
    ca_pred_args[[i]] <- input[[paste0("ca_",i)]]

  ca_pred_args$pred_cmd <- ca_pred_args$pred_data <- ""
  if (input$ca_predict == "cmd") {
    ca_pred_args$pred_cmd <- gsub("\\s", "", input$ca_pred_cmd) %>% gsub("\"","\'",.)
  } else if (input$ca_predict == "data") {
    ca_pred_args$pred_data <- input$ca_pred_data
  } else if (input$ca_predict == "datacmd") {
    ca_pred_args$pred_cmd <- gsub("\\s", "", input$ca_pred_cmd) %>% gsub("\"","\'",.)
    ca_pred_args$pred_data <- input$ca_pred_data
  }
  ca_pred_args
})

ca_pred_plot_args <- as.list(if (exists("plot.model.predict")) formals(plot.model.predict)
                         else formals(radiant.model:::plot.model.predict))

## list of function inputs selected by user
ca_pred_plot_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(ca_pred_plot_args))
    ca_pred_plot_args[[i]] <- input[[paste0("ca_",i)]]
  ca_pred_plot_args
})

output$ui_ca_rvar <- renderUI({
	isNum <- "numeric" == .getclass() | "integer" == .getclass()
 	vars <- varnames()[isNum]
  selectInput(inputId = "ca_rvar", label = "Profile evaluations:", choices = vars,
   	selected = state_single("ca_rvar",vars), multiple = FALSE)
})

output$ui_ca_evar <- renderUI({
	isFct <- "factor" == .getclass()
 	vars <- varnames()[isFct]
  selectInput(inputId = "ca_evar", label = "Attributes:", choices = vars,
  	selected = state_multiple("ca_evar", vars), multiple = TRUE,
  	size = min(10, length(vars)), selectize = FALSE)
})

output$ui_ca_show_interactions <- renderUI({
  choices <- ca_show_interactions[1:max(min(3,length(input$ca_evar)),1)]
  radioButtons(inputId = "ca_show_interactions", label = "Interactions:",
    choices = choices, selected = state_init("ca_show_interactions"),
    inline = TRUE)
})

output$ui_ca_int <- renderUI({

  if (isolate("ca_show_interactions" %in% names(input)) &&
      is_empty(input$ca_show_interactions)) {
    choices <- character(0)
  } else if (is_empty(input$ca_show_interactions)) {
    return()
  } else {
    vars <- input$ca_evar
    if (not_available(vars) || length(vars) < 2) return()
    ## list of interaction terms to show
    choices <- iterms(vars, input$ca_show_interactions)
  }

  selectInput("ca_int", label = NULL, choices = choices,
    selected = state_init("ca_int"),
    multiple = TRUE, size = min(4,length(choices)), selectize = FALSE)
})


output$ui_ca_by <- renderUI({
 	vars <- c("None" = "none", varnames())
  selectInput(inputId = "ca_by", label = "By:", choices = vars,
  	selected = state_single("ca_by", vars, "none"), multiple = FALSE)
})

output$ui_ca_show <- renderUI({
  levs <- c()
  if (available(input$ca_by))
    levs <- .getdata()[[input$ca_by]] %>% as_factor %>% levels
  selectInput(inputId = "ca_show", label = "Show:", choices = levs,
  	selected = state_single("ca_show", levs, levs[1]), multiple = FALSE)
})

## reset ca_show if needed
observeEvent(input$ca_by == "none" && !is_empty(input$ca_show), {
  updateSelectInput(session = session, inputId = "ca_show", selected = NULL)
})

output$ui_ca_store <- renderUI({
	req(input$ca_by != "none")
	tagList(
    HTML("<label>Store all PWs in a new dataset:</label>"),
    tags$table(
	    tags$td(textInput("ca_store_pw_name", NULL, state_init("ca_store_pw_name","PWs"))),
	    tags$td(actionButton("ca_store_pw", "Store"), style="padding-top:5px;")
	  ),
	  tags$br(),
    HTML("<label>Store all IWs in a new dataset:</label>"),
	  tags$table(
	    tags$td(textInput("ca_store_iw_name", NULL, state_init("ca_store_iw_name","IWs"))),
	    tags$td(actionButton("ca_store_iw", "Store"), style="padding-top:5px;")
	  )
	)
})

output$ui_ca_store_pred <- renderUI({
	req(input$ca_predict != "none")
	req(input$ca_by)
	lab <- "<label>Store predictions:</label>"
	name <- "predict_ca"
	if (input$ca_by != "none") {
		lab <- sub(":", " in new dataset:", lab)
		name <- "predict_by"
	}

  tags$table(
    if (!input$ca_pred_plot) tags$br(),
    HTML(lab),
    tags$td(textInput("ca_store_pred_name", NULL, state_init("ca_store_pred_name", name))),
    tags$td(actionButton("ca_store_pred", "Store"), style="padding-top:5px;")
  )
})

## reset prediction settings when the dataset changes
observeEvent(input$dataset, {
  if (input$nav_radiant != "Conjoint")
    updateSelectInput(session = session, inputId = "ca_predict", selected = "none")
})

output$ui_ca_predict_plot <- renderUI({
	req(input$ca_by)
	if (input$ca_by != "none")
    predict_plot_controls("ca", vars_color = input$ca_by, init_color = input$ca_by)
  else
  	predict_plot_controls("ca")
})

output$ui_ca_pred_data <- renderUI({
  selectizeInput(inputId = "ca_pred_data", label = "Predict for profiles:",
                 choices = c("None" = "", r_data$datasetlist),
                 selected = state_single("ca_pred_data", c("None" = "", r_data$datasetlist)), multiple = FALSE)
})

output$ui_conjoint <- renderUI({
  req(input$dataset)
  tagList(
    wellPanel(
      actionButton("ca_run", "Estimate", width = "100%")
    ),
    conditionalPanel(condition = "input.tabs_conjoint == 'Predict'",
      wellPanel(
        selectInput("ca_predict", label = "Prediction input:", ca_predict,
          selected = state_single("ca_predict", ca_predict, "none")),
        conditionalPanel("input.ca_predict == 'data' | input.ca_predict == 'datacmd'",
          uiOutput("ui_ca_pred_data")
        ),
        conditionalPanel("input.ca_predict == 'cmd' | input.ca_predict == 'datacmd'",
          returnTextAreaInput("ca_pred_cmd", "Prediction command:",
            value = state_init("ca_pred_cmd", ""))
        ),
        conditionalPanel(condition = "input.ca_predict != 'none'",
          checkboxInput("ca_pred_plot", "Plot predictions", state_init("ca_pred_plot", FALSE)),
          conditionalPanel("input.ca_pred_plot == true",
            uiOutput("ui_ca_predict_plot")
          )
        ),
        ## only show if a dataset is used for prediction or storing predictions 'by'
        conditionalPanel("input.ca_predict == 'data' | input.ca_predict == 'datacmd' | input.ca_by != 'none'",
          uiOutput("ui_ca_store_pred")
        )
      )
    ),
	  conditionalPanel(condition = "input.tabs_conjoint == 'Plot'",
  		wellPanel(
	      selectInput("ca_plots", "Conjoint plots:", choices = ca_plots,
	  	  	selected = state_single("ca_plots", ca_plots, "pw")),
	  		conditionalPanel(condition = "input.ca_plots == 'pw'",
			    checkboxInput(inputId = "ca_scale_plot", label = "Scale PW plots",
				  	value = state_init('ca_scale_plot',FALSE)))
  		)
	  ),
  	wellPanel(
	    uiOutput("ui_ca_rvar"),
	    uiOutput("ui_ca_evar"),
      # uiOutput("ui_ca_show_interactions"),
      # conditionalPanel(condition = "input.ca_show_interactions != ''",
      #   uiOutput("ui_ca_int")
      # ),
	    uiOutput("ui_ca_by"),
		  conditionalPanel(condition = "input.tabs_conjoint != 'Predict' & input.ca_by != 'none'",
	      uiOutput("ui_ca_show")
	    ),
		  conditionalPanel(condition = "input.tabs_conjoint == 'Summary'",
	    	uiOutput("ui_ca_store")
	    ),
      conditionalPanel(condition = "input.ca_evar != null",
			  checkboxInput("ca_reverse", label = "Reverse evaluation scores",
			  	value = state_init('ca_reverse',FALSE)),
		    conditionalPanel(condition = "input.tabs_conjoint == 'Summary'",
			    checkboxInput(inputId = "ca_additional", label = "Additional regression output",
				  	value = state_init('ca_additional',FALSE)),
			    checkboxInput(inputId = "ca_mc_diag", label = "VIF",
				  	value = state_init('ca_mc_diag',FALSE))
		  	)
		  )
	  ),
  	help_and_report(modal_title = "Conjoint",
  	                fun_name = "conjoint",
  	                help_file = inclMD(file.path(getOption("radiant.path.multivariate"),"app/tools/help/conjoint.md")))
	)
})

ca_available <- reactive({

  if (not_available(input$ca_rvar))
    return("This analysis requires a response variable of type integer\nor numeric and one or more explanatory variables.\nIf these variables are not available please select another dataset.\n\n" %>% suggest_data("carpet"))

  if (not_available(input$ca_evar))
		return("Please select one or more explanatory variables of type factor.\nIf none are available please choose another dataset\n\n" %>% suggest_data("carpet"))

  "available"
})

ca_plot <- reactive({
	req(input$ca_plots)
	nrVars <- length(input$ca_evar)
	plot_height <- plot_width <- 500
	if (input$ca_plots == 'pw') {
		plot_height <- 325 * (1 + floor((nrVars - 1) / 2))
		plot_width <- 325 * min(nrVars,2)
	}

  list(plot_width = plot_width, plot_height = plot_height)
})

ca_plot_width <- function()
  ca_plot() %>% { if (is.list(.)) .$plot_width else 650 }

ca_plot_height <- function()
  ca_plot() %>% { if (is.list(.)) .$plot_height else 400 }

ca_pred_plot_height <- function()
  if (input$ca_pred_plot) 500 else 0

# output is called from the main radiant ui.R
output$conjoint <- renderUI({

		register_print_output("summary_conjoint", ".summary_conjoint")
    register_print_output("predict_conjoint", ".predict_print_conjoint")
    register_plot_output("predict_plot_conjoint", ".predict_plot_conjoint",
                          height_fun = "ca_pred_plot_height")
		register_plot_output("plot_conjoint", ".plot_conjoint",
                          height_fun = "ca_plot_height",
                          width_fun = "ca_plot_width")

		# two separate tabs
		ca_output_panels <- tabsetPanel(
	    id = "tabs_conjoint",
	    tabPanel("Summary",
        downloadLink("dl_ca_PWs", "", class = "fa fa-download alignright"), br(),
	      verbatimTextOutput("summary_conjoint")),
      tabPanel("Predict",
        conditionalPanel("input.ca_pred_plot == true",
          plot_downloader("conjoint", height = ca_pred_plot_height(), po = "dlp_", pre = ".predict_plot_"),
          plotOutput("predict_plot_conjoint", width = "100%", height = "100%")
        ),
        downloadLink("dl_ca_pred", "", class = "fa fa-download alignright"), br(),
        verbatimTextOutput("predict_conjoint")
      ),
	    tabPanel("Plot",
               plot_downloader("conjoint", height = ca_plot_height()),
	             plotOutput("plot_conjoint", width = "100%", height = "100%"))
	  )

		stat_tab_panel(menu = "Multivariate > Conjoint",
		               tool = "Conjoint",
		               tool_ui = "ui_conjoint",
		             	 output_panels = ca_output_panels)
})

.conjoint <- eventReactive(input$ca_run, {
  req(available(input$ca_rvar), available(input$ca_evar))
  withProgress(message = 'Estimating model', value = 1,
	  do.call(conjoint, ca_inputs())
	)
})

.summary_conjoint <- reactive({
  if (ca_available() != "available") return(ca_available())
  if (not_pressed(input$ca_run)) return("** Press the Estimate button to estimate the model **")
  do.call(summary, c(list(object = .conjoint()), ca_sum_inputs()))
})

.predict_conjoint <- reactive({
  if (ca_available() != "available") return(ca_available())
  if (not_pressed(input$ca_run)) return("** Press the Estimate button to estimate the model **")
  if (is_empty(input$ca_predict, "none")) return("** Select prediction input **")
  if((input$ca_predict == "data" || input$ca_predict == "datacmd") && is_empty(input$ca_pred_data))
    return("** Select data for prediction **")
  if(input$ca_predict == "cmd" && is_empty(input$ca_pred_cmd))
    return("** Enter prediction commands **")

  withProgress(message = "Generating predictions", value = 1, {
    do.call(predict, c(list(object = .conjoint()), ca_pred_inputs()))
  })
})

.predict_print_conjoint <- reactive({
  .predict_conjoint() %>% {if (is.character(.)) cat(.,"\n") else print(.)}
})

.predict_plot_conjoint <- reactive({
  if (ca_available() != "available") return(ca_available())
  req(input$ca_pred_plot, available(input$ca_xvar))
  if (not_pressed(input$ca_run)) return(invisible())
  if (is_empty(input$ca_predict, "none")) return(invisible())
  if((input$ca_predict == "data" || input$ca_predict == "datacmd") && is_empty(input$ca_pred_data))
    return(invisible())
  if(input$ca_predict == "cmd" && is_empty(input$ca_pred_cmd))
    return(invisible())
  do.call(plot, c(list(x = .predict_conjoint()), ca_pred_plot_inputs()))
})

.plot_conjoint <- reactive({
  if (ca_available() != "available") return(ca_available())
  if (not_pressed(input$ca_run)) return("** Press the Estimate button to estimate the model **")
  ca_plot_inputs() %>% { .$shiny <- TRUE; . } %>% { do.call(plot, c(list(x = .conjoint()), .)) }
 })

observeEvent(input$conjoint_report, {
  outputs <- c("summary")
  inp_out <- list("","")
  inp_out[[1]] <- clean_args(ca_sum_inputs(), ca_sum_args[-1])
  figs <- FALSE
  if (!is_empty(input$ca_plots)) {
    inp_out[[2]] <- clean_args(ca_plot_inputs(), ca_plot_args[-1])
    inp_out[[2]]$custom <- FALSE
    outputs <- c(outputs, "plot")
    figs <- TRUE
  }
  xcmd <- ""
  if (input$ca_by != "none")
    xcmd <- paste0("# store(result, name = \"", input$ca_store_pw_name, "\", type = \"PW\")\n")

  if (!is_empty(input$ca_predict, "none") &&
      (!is_empty(input$ca_pred_data) || !is_empty(input$ca_pred_cmd))) {
    pred_args <- clean_args(ca_pred_inputs(), ca_pred_args[-1])
    inp_out[[2 + figs]] <- pred_args
    outputs <- c(outputs, "pred <- predict")

    xcmd <- paste0(xcmd, "print(pred, n = 10)")
    if (input$ca_predict %in% c("data","datacmd") || input$ca_by != "none") {
      xcmd <- paste0(xcmd, "\n# store(pred, data = \"", input$ca_pred_data, "\", name = \"", input$ca_store_pred_name,"\")")
      xcmd <- paste0(xcmd, "\n# write.csv(pred, file = \"~/", input$ca_store_pred_name, ".csv\", row.names = FALSE)")
    } else {
      xcmd <- paste0(xcmd, "\n# write.csv(pred, file = \"~/ca_predictions.csv\", row.names = FALSE)")
    }

    if (input$ca_pred_plot && !is_empty(input$ca_xvar)) {
      inp_out[[3 + figs]] <- clean_args(ca_pred_plot_inputs(), ca_pred_plot_args[-1])
      inp_out[[3 + figs]]$result <- "pred"
      outputs <- c(outputs, "plot")
      figs <- TRUE
    }
  }
  update_report(inp_main = clean_args(ca_inputs(), ca_args),
                fun_name = "conjoint", inp_out = inp_out,
                outputs = outputs, figs = figs,
                fig.width = ca_plot_width(),
                fig.height = ca_plot_height(),
                xcmd = xcmd)
})

output$dl_ca_PWs <- downloadHandler(
	filename = function() { paste(input$dataset, '_PWs.csv', sep='') },
  content = function(file) {
	  if (pressed(input$ca_run)) {
	    .conjoint()$the_table$PW %>% write.csv(file = file, row.names = FALSE)
	  } else {
	    cat("No output available. Press the Estimate button to generate results", file = file)
	  }
  }
)

observeEvent(input$ca_store_pw, {
  req(pressed(input$ca_run))
  robj <- .conjoint()
  if (!is.list(robj)) return()
  withProgress(message = "Storing PWs", value = 1,
    store(robj, name = input$ca_store_pw_name, type = "PW")
  )
})

observeEvent(input$ca_store_iw, {
  req(pressed(input$ca_run))
  robj <- .conjoint()
  if (!is.list(robj)) return()
  withProgress(message = "Storing IWs", value = 1,
    store(robj, name = input$ca_store_iw_name, type = "IW")
  )
})

observeEvent(input$ca_store_pred, {
  req(!is_empty(input$ca_pred_data), pressed(input$ca_run))
  pred <- .predict_conjoint()
  if (is.null(pred)) return()
  withProgress(message = 'Storing predictions', value = 1,
    store(pred, data = input$ca_pred_data, name = input$ca_store_pred_name)
  )
})

output$dl_ca_pred <- downloadHandler(
  filename = function() { "conjoint_predictions.csv" },
  content = function(file) {
    if (pressed(input$ca_run)) {
      .predict_conjoint() %>%
        write.csv(file = file, row.names = FALSE)
    } else {
      cat("No output available. Press the Estimate button to generate results", file = file)
    }
  }
)

