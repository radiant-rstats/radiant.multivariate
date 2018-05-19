################################################################
# Conjoint 
################################################################
ca_show_interactions <- c("None" = "", "2-way" = 2, "3-way" = 3)
ca_predict <- c("None" = "none", "Data" = "data", "Command" = "cmd", "Data & Command" = "datacmd")
ca_plots <- list("None" = "none", "Part-worths" = "pw", "Importance-weights" = "iw")

# list of function arguments
ca_args <- as.list(formals(conjoint))

# list of function inputs selected by user
ca_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  ca_args$data_filter <- if (input$show_filter) input$data_filter else ""
  ca_args$dataset <- input$dataset
  for (i in r_drop(names(ca_args)))
    ca_args[[i]] <- input[[paste0("ca_", i)]]
  ca_args
})

ca_sum_args <- as.list(if (exists("summary.conjoint")) {
  formals(summary.conjoint)
} else {
  formals(radiant.multivariate:::summary.conjoint)
} )

## list of function inputs selected by user
ca_sum_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(ca_sum_args))
    ca_sum_args[[i]] <- input[[paste0("ca_", i)]]
  ca_sum_args
})

ca_plot_args <- as.list(if (exists("plot.conjoint")) {
  formals(plot.conjoint)
} else {
  formals(radiant.multivariate:::plot.conjoint)
} )

## list of function inputs selected by user
ca_plot_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(ca_plot_args))
    ca_plot_args[[i]] <- input[[paste0("ca_", i)]]
  ca_plot_args
})

ca_pred_args <- as.list(if (exists("predict.conjoint")) {
  formals(predict.conjoint)
} else {
  formals(radiant.multivariate:::predict.conjoint)
} )

## list of function inputs selected by user
ca_pred_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(ca_pred_args))
    ca_pred_args[[i]] <- input[[paste0("ca_", i)]]

  ca_pred_args$pred_cmd <- ca_pred_args$pred_data <- ""
  if (input$ca_predict == "cmd") {
    ca_pred_args$pred_cmd <- gsub("\\s{2,}", " ", input$ca_pred_cmd) %>% 
      gsub(";\\s+", ";", .) %>%
      gsub("\"", "\'", .)
  } else if (input$ca_predict == "data") {
    ca_pred_args$pred_data <- input$ca_pred_data
  } else if (input$ca_predict == "datacmd") {
    ca_pred_args$pred_cmd <- gsub("\\s{2,}", " ", input$ca_pred_cmd) %>% 
      gsub(";\\s+", ";", .) %>%
      gsub("\"", "\'", .)
    ca_pred_args$pred_data <- input$ca_pred_data
  }
  ca_pred_args
})

ca_pred_plot_args <- as.list(if (exists("plot.model.predict")) {
  formals(plot.model.predict)
} else {
  formals(radiant.model:::plot.model.predict)
} )

## list of function inputs selected by user
ca_pred_plot_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(ca_pred_plot_args))
    ca_pred_plot_args[[i]] <- input[[paste0("ca_", i)]]
  ca_pred_plot_args
})

output$ui_ca_rvar <- renderUI({
  isNum <- "numeric" == .get_class() | "integer" == .get_class()
  vars <- varnames()[isNum]
  selectInput(
    inputId = "ca_rvar", label = "Profile evaluations:", choices = vars,
    selected = state_single("ca_rvar", vars), multiple = FALSE
  )
})

output$ui_ca_evar <- renderUI({
  hasLevs <- .get_class() %in% c("factor", "logical", "character")
  vars <- varnames()[hasLevs]
  selectInput(
    inputId = "ca_evar", label = "Attributes:", choices = vars,
    selected = state_multiple("ca_evar", vars), multiple = TRUE,
    size = min(10, length(vars)), selectize = FALSE
  )
})

output$ui_ca_show_interactions <- renderUI({
  choices <- ca_show_interactions[1:max(min(3, length(input$ca_evar)), 1)]
  radioButtons(
    inputId = "ca_show_interactions", label = "Interactions:",
    choices = choices, selected = state_init("ca_show_interactions"),
    inline = TRUE
  )
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

  selectInput(
    "ca_int", label = NULL, choices = choices,
    selected = state_init("ca_int"),
    multiple = TRUE, size = min(4, length(choices)), selectize = FALSE
  )
})


output$ui_ca_by <- renderUI({
  vars <- c("None" = "none", varnames())
  selectInput(
    inputId = "ca_by", label = "By:", choices = vars,
    selected = state_single("ca_by", vars, "none"), multiple = FALSE
  )
})

output$ui_ca_show <- renderUI({
  levs <- c()
  if (available(input$ca_by)) {
    levs <- .get_data()[[input$ca_by]] %>% as_factor() %>% levels()
  }
  selectInput(
    inputId = "ca_show", label = "Show:", choices = levs,
    selected = state_single("ca_show", levs, levs[1]), multiple = FALSE
  )
})

## reset ca_show if needed
observeEvent(input$ca_by == "none" && !is_empty(input$ca_show), {
  updateSelectInput(session = session, inputId = "ca_show", selected = NULL)
})

## reset prediction and plot settings when the dataset changes
observeEvent(input$dataset, {
  updateSelectInput(session = session, inputId = "ca_predict", selected = "none")
  updateSelectInput(session = session, inputId = "ca_plots", selected = "none")
})

observe({
  ## dep on most inputs
  input$data_filter
  input$show_filter
  sapply(r_drop(names(ca_args)), function(x) input[[paste0("ca_", x)]])
  ## notify user when the model needs to be updated
  ## based on https://stackoverflow.com/questions/45478521/listen-to-reactive-invalidation-in-shiny
  if (pressed(input$ca_run)) {
    if (is.null(input$ca_evar)) { 
      updateTabsetPanel(session, "tabs_conjoint ", selected = "Summary")
      updateActionButton(session, "ca_run", "Estimate model", icon = icon("play"))
    } else if (isTRUE(attr(ca_inputs, "observable")$.invalidated)) {
      updateActionButton(session, "ca_run", "Re-estimate model", icon = icon("refresh", class = "fa-spin"))
    } else {
      updateActionButton(session, "ca_run", "Estimate model", icon = icon("play"))
    }
  }
})

output$ui_ca_store <- renderUI({
  req(input$ca_by != "none")
  tagList(
    HTML("<label>Store all PWs in a new dataset:</label>"),
    tags$table(
      tags$td(textInput("ca_store_pw_name", NULL, "", placeholder = "Provide data name")),
      tags$td(actionButton("ca_store_pw", "Store", icon = icon("plus")), style = "padding-top:5px;")
    ),
    tags$br(),
    HTML("<label>Store all IWs in a new dataset:</label>"),
    tags$table(
      tags$td(textInput("ca_store_iw_name", NULL, "", placeholder = "Provide data name")),
      tags$td(actionButton("ca_store_iw", "Store", icon = icon("plus")), style = "padding-top:5px;")
    )
  )
})

output$ui_ca_store_pred <- renderUI({
  req(input$ca_predict != "none")
  req(input$ca_by)
  lab <- "<label>Store predictions:</label>"
  name <- "pred_ca"
  if (input$ca_by != "none") {
    lab <- sub(":", " in new dataset:", lab)
    name <- ""
  }

  tags$table(
    if (!input$ca_pred_plot) tags$br(),
    HTML(lab),
    # tags$td(textInput("ca_store_pred_name", NULL, state_init("ca_store_pred_name", name))),
    tags$td(textInput("ca_store_pred_name", NULL, name, placeholder = "Provide data name")),
    tags$td(actionButton("ca_store_pred", "Store", icon = icon("plus")), style = "padding-top:5px;")
  )
})

output$ui_ca_predict_plot <- renderUI({
  req(input$ca_by)
  if (input$ca_by != "none") {
    predict_plot_controls("ca", vars_color = input$ca_by, init_color = input$ca_by)
  } else {
    predict_plot_controls("ca")
  }
})

output$ui_ca_pred_data <- renderUI({
  selectizeInput(
    inputId = "ca_pred_data", label = "Prediction data:",
    choices = c("None" = "", r_info[["datasetlist"]]),
    selected = state_single("ca_pred_data", c("None" = "", r_info[["datasetlist"]])), 
    multiple = FALSE
  )
})

output$ui_conjoint <- renderUI({
  req(input$dataset)
  tagList(
    wellPanel(
      actionButton("ca_run", "Estimate model", width = "100%", icon = icon("play"), class = "btn-success")
    ),
    wellPanel(
      conditionalPanel(
        condition = "input.tabs_conjoint == 'Summary'",
        uiOutput("ui_ca_rvar"),
        uiOutput("ui_ca_evar"),
        # uiOutput("ui_ca_show_interactions"),
        # conditionalPanel(condition = "input.ca_show_interactions != ''",
        #   uiOutput("ui_ca_int")
        # ),
        uiOutput("ui_ca_by"),
        conditionalPanel(
          condition = "input.tabs_conjoint != 'Predict' & input.ca_by != 'none'",
          uiOutput("ui_ca_show")
        ),
        conditionalPanel(
          condition = "input.tabs_conjoint == 'Summary'",
          uiOutput("ui_ca_store")
        ),
        conditionalPanel(
          condition = "input.ca_evar != null",
          checkboxInput(
            "ca_reverse", label = "Reverse evaluation scores",
            value = state_init("ca_reverse", FALSE)
          ),
          conditionalPanel(
            condition = "input.tabs_conjoint == 'Summary'",
            checkboxInput(
              inputId = "ca_additional", label = "Additional regression output",
              value = state_init("ca_additional", FALSE)
            ),
            checkboxInput(
              inputId = "ca_mc_diag", label = "VIF",
              value = state_init("ca_mc_diag", FALSE)
            )
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabs_conjoint == 'Predict'",
        selectInput(
          "ca_predict", label = "Prediction input type:", ca_predict,
          selected = state_single("ca_predict", ca_predict, "none")
        ),
        conditionalPanel(
          "input.ca_predict == 'data' | input.ca_predict == 'datacmd'",
          uiOutput("ui_ca_pred_data")
        ),
        conditionalPanel(
          "input.ca_predict == 'cmd' | input.ca_predict == 'datacmd'",
          returnTextAreaInput(
            "ca_pred_cmd", "Prediction command:",
            value = state_init("ca_pred_cmd", "")
          )
        ),
        conditionalPanel(
          condition = "input.ca_predict != 'none'",
          checkboxInput("ca_pred_plot", "Plot predictions", state_init("ca_pred_plot", FALSE)),
          conditionalPanel(
            "input.ca_pred_plot == true",
            uiOutput("ui_ca_predict_plot")
          )
        ),
        ## only show if a dataset is used for prediction or storing predictions 'by'
        conditionalPanel(
          "input.ca_predict == 'data' | input.ca_predict == 'datacmd' | input.ca_by != 'none'",
          uiOutput("ui_ca_store_pred")
        )
      ),
      conditionalPanel(
        condition = "input.tabs_conjoint == 'Plot'",
        selectInput(
          "ca_plots", "Conjoint plots:", choices = ca_plots,
          selected = state_single("ca_plots", ca_plots, "none")
        ),
        conditionalPanel(
          condition = "input.ca_plots == 'pw'",
          checkboxInput(
            inputId = "ca_scale_plot", label = "Scale PW plots",
            value = state_init("ca_scale_plot", FALSE)
          )
        )
      )
    ),
    help_and_report(
      modal_title = "Conjoint",
      fun_name = "conjoint",
      help_file = inclMD(file.path(getOption("radiant.path.multivariate"), "app/tools/help/conjoint.md"))
    )
  )
})

ca_available <- reactive({
  if (not_pressed(input$ca_run)) {
    "** Press the Estimate button to run the conjoint analysis **"
  } else if (not_available(input$ca_rvar)) {
    "This analysis requires a response variable of type integer\nor numeric and one or more explanatory variables.\nIf these variables are not available please select another dataset.\n\n" %>% 
      suggest_data("carpet")
  } else if (not_available(input$ca_evar)) {
    "Please select one or more explanatory variables of type factor.\nIf none are available please choose another dataset\n\n" %>% 
      suggest_data("carpet")
  } else {
    "available"
  }
})

ca_plot <- reactive({
  req(pressed(input$ca_run))
  if (ca_available() != "available") return()
  req(input$ca_plots)

  nrVars <- length(input$ca_evar)
  plot_height <- plot_width <- 500
  if (input$ca_plots == "pw") {
    plot_height <- 325 * (1 + floor((nrVars - 1) / 2))
    plot_width <- 325 * min(nrVars, 2)
  }
  list(plot_width = plot_width, plot_height = plot_height)
})

ca_plot_width <- function()
  ca_plot() %>% {if (is.list(.)) .$plot_width else 650}

ca_plot_height <- function()
  ca_plot() %>% {if (is.list(.)) .$plot_height else 400}

ca_pred_plot_height <- function()
  if (input$ca_pred_plot) 500 else 0

output$conjoint <- renderUI({
  register_print_output("summary_conjoint", ".summary_conjoint")
  register_print_output("predict_conjoint", ".predict_print_conjoint")
  register_plot_output(
    "predict_plot_conjoint", ".predict_plot_conjoint",
    height_fun = "ca_pred_plot_height"
  )
  register_plot_output(
    "plot_conjoint", ".plot_conjoint",
    height_fun = "ca_plot_height",
    width_fun = "ca_plot_width"
  )

  ## three separate tabs
  ca_output_panels <- tabsetPanel(
    id = "tabs_conjoint",
    tabPanel(
      "Summary",
      download_link("dl_ca_PWs"), br(),
      verbatimTextOutput("summary_conjoint")
    ),
    tabPanel(
      "Predict",
      conditionalPanel(
        "input.ca_pred_plot == true",
        download_link("dlp_ca_pred"),
        plotOutput("predict_plot_conjoint", width = "100%", height = "100%")
      ),
      download_link("dl_ca_pred"), br(),
      verbatimTextOutput("predict_conjoint")
    ),
    tabPanel(
      "Plot",
      download_link("dlp_conjoint"),
      plotOutput("plot_conjoint", width = "100%", height = "100%")
    )
  )

  stat_tab_panel(
    menu = "Multivariate > Conjoint",
    tool = "Conjoint",
    tool_ui = "ui_conjoint",
    output_panels = ca_output_panels
  )
})

.conjoint <- eventReactive(input$ca_run, {
  req(available(input$ca_rvar), available(input$ca_evar))
  withProgress(
    message = "Estimating model", value = 1,
    do.call(conjoint, ca_inputs())
  )
})

.summary_conjoint <- reactive({
  if (not_pressed(input$ca_run)) return("** Press the Estimate button to estimate the model **")
  if (ca_available() != "available") return(ca_available())
  do.call(summary, c(list(object = .conjoint()), ca_sum_inputs()))
})

.predict_conjoint <- reactive({
  if (not_pressed(input$ca_run)) return("** Press the Estimate button to estimate the model **")
  if (ca_available() != "available") return(ca_available())
  if (is_empty(input$ca_predict, "none")) return("** Select prediction input **")
  if ((input$ca_predict == "data" || input$ca_predict == "datacmd") && is_empty(input$ca_pred_data)) {
    return("** Select data for prediction **")
  }
  if (input$ca_predict == "cmd" && is_empty(input$ca_pred_cmd)) {
    return("** Enter prediction commands **")
  }

  withProgress(message = "Generating predictions", value = 1, {
    do.call(predict, c(list(object = .conjoint()), ca_pred_inputs()))
  })
})

.predict_print_conjoint <- reactive({
  .predict_conjoint() %>% 
    {if (is.character(.)) cat(., "\n") else print(.)}
})

.predict_plot_conjoint <- reactive({
  if (not_pressed(input$ca_run)) return(invisible())
  if (ca_available() != "available") return(ca_available())
  req(input$ca_pred_plot, available(input$ca_xvar))
  if (is_empty(input$ca_predict, "none")) return(invisible())
  if ((input$ca_predict == "data" || input$ca_predict == "datacmd") && is_empty(input$ca_pred_data)) {
    return(invisible())
  }
  if (input$ca_predict == "cmd" && is_empty(input$ca_pred_cmd)) {
    return(invisible())
  }
  withProgress(message = "Generating prediction plot", value = 1, {
    do.call(plot, c(list(x = .predict_conjoint()), ca_pred_plot_inputs()))
  })
})

.plot_conjoint <- reactive({
  if (not_pressed(input$ca_run)) {
    return("** Press the Estimate button to estimate the model **")
  } else if (is_empty(input$ca_plots, "none")) {
    return("Please select a conjoint plot from the drop-down menu")
  } 
  input$ca_scale_plot
  input$ca_plots
  isolate({
    if (ca_available() != "available") return(ca_available())
    withProgress(message = "Generating plots", value = 1, {
      do.call(plot, c(list(x = .conjoint()), ca_plot_inputs(), shiny = TRUE))
    })
  })
})

observeEvent(input$conjoint_report, {
  outputs <- c("summary")
  inp_out <- list("", "")
  inp_out[[1]] <- clean_args(ca_sum_inputs(), ca_sum_args[-1])
  figs <- FALSE
  if (!is_empty(input$ca_plots, "none")) {
    inp_out[[2]] <- clean_args(ca_plot_inputs(), ca_plot_args[-1])
    inp_out[[2]]$custom <- FALSE
    outputs <- c(outputs, "plot")
    figs <- TRUE
  }
  xcmd <- ""

  if (input$ca_by != "none") {
    if (!is_empty(input$ca_store_pw_name)) {
      xcmd <- paste0(xcmd, input$ca_store_pw_name, " <- result$PW; register(\"", input$ca_store_pw_name, "\")\n")
    }
    if (!is_empty(input$ca_store_iw_name)) {
      xcmd <- paste0(xcmd, input$ca_store_iw_name, " <- result$IW; register(\"", input$ca_store_iw_name, "\")\n")
    }
  }

  if (!is_empty(input$ca_predict, "none") &&
     (!is_empty(input$ca_pred_data) || !is_empty(input$ca_pred_cmd))) {

    pred_args <- clean_args(ca_pred_inputs(), ca_pred_args[-1])
    if (!is_empty(pred_args[["pred_cmd"]])) {
      pred_args[["pred_cmd"]] <- strsplit(pred_args[["pred_cmd"]], ";")[[1]]
    }
    if (!is_empty(pred_args$pred_data)) {
      pred_args$pred_data <- as.symbol(pred_args$pred_data)
    } 

    inp_out[[2 + figs]] <- pred_args
    pred_name <- "pred"
    if (!is_empty(input$ca_by, "none") && !is_empty(input$ca_store_pred_name)) {
      pred_name <- input$ca_store_pred_name
      outputs <- c(outputs, paste0(pred_name, " <- predict"))
      xcmd <- paste0(xcmd, pred_name %>% paste0("register(\"", ., "\")\nprint(", . ,", n = 10)"))
    } else {
      outputs <- c(outputs, "pred <- predict")
      xcmd <- paste0(xcmd, "print(pred, n = 10)")
      if (input$ca_predict %in% c("data", "datacmd")) {
        if (is_empty(input$ca_by, "none")) {
          name <- unlist(strsplit(input$ca_store_pred_name, "(\\s*,\\s*|\\s*;\\s*|\\s+)")) %>%
            gsub("\\s", "", .) %>%
            deparse(., control = getOption("dctrl"), width.cutoff = 500L)
          xcmd <- paste0(xcmd, "\n", input$ca_pred_data , " <- store(", 
            input$ca_pred_data, ", pred, name = ", name, ")"
          )
        } 
      }
    } 

    if (input$ca_pred_plot && !is_empty(input$ca_xvar)) {
      inp_out[[3 + figs]] <- clean_args(ca_pred_plot_inputs(), ca_pred_plot_args[-1])
      inp_out[[3 + figs]]$result <- pred_name
      outputs <- c(outputs, "plot")
      figs <- TRUE
    }
  }
  update_report(
    inp_main = clean_args(ca_inputs(), ca_args),
    fun_name = "conjoint", 
    inp_out = inp_out,
    outputs = outputs, 
    figs = figs,
    fig.width = ca_plot_width(),
    fig.height = ca_plot_height(),
    xcmd = xcmd
  )
})

observeEvent(input$ca_store_pw, {
  name <- input$ca_store_pw_name
  req(pressed(input$ca_run), name)
  robj <- .conjoint()
  if (!is.list(robj)) return()
  withProgress(
    message = "Storing PWs", value = 1,
    r_data[[name]] <- robj$PW
  )
  register(name)
})

observeEvent(input$ca_store_iw, {
  name <- input$ca_store_iw_name
  req(pressed(input$ca_run), name)
  robj <- .conjoint()
  if (!is.list(robj)) return()
  withProgress(
    message = "Storing IWs", value = 1,
    r_data[[name]] <- robj$IW
  )
  register(name)
})

observeEvent(input$ca_store_pred, {
  req(!is_empty(input$ca_pred_data), pressed(input$ca_run))
  pred <- .predict_conjoint()
  if (is.null(pred)) return()
  if ("conjoint.predict.by" %in% class(pred)) {
    withProgress(
      message = "Storing predictions in new dataset", value = 1,
        r_data[[input$ca_store_pred_name]] <- pred, 
    )
    register(input$ca_store_pred_name)
  } else {
    withProgress(
      message = "Storing predictions", value = 1,
      r_data[[input$ca_pred_data]] <- radiant.model:::store.model.predict(
        r_data[[input$ca_pred_data]], pred, 
        name = input$ca_store_pred_name
      )
    )
  }
})

dl_ca_PWs <- function(path) {
  if (pressed(input$ca_run)) {
    store(.conjoint(), df = TRUE) %>% write.csv(file = path, row.names = FALSE)
  } else {
    cat("No output available. Press the Estimate button to generate results", file = path)
  }
}

download_handler(
  id = "dl_ca_PWs", 
  fun = dl_ca_PWs, 
  fn = paste0(input$dataset, "_PWs.csv"),
  caption = "Download part worths"
)

dl_ca_pred <- function(path) {
  if (pressed(input$ca_run)) {
    write.csv(.predict_conjoint(), file = path, row.names = FALSE)
  } else {
    cat("No output available. Press the Estimate button to generate results", file = path)
  }
}

download_handler(
  id = "dl_ca_pred", 
  fun = dl_ca_pred, 
  fn = paste0(input$dataset, "_ca_pred.csv"),
  caption = "Download predictions"
)

download_handler(
  id = "dlp_ca_pred", 
  fun = download_handler_plot, 
  fn = paste0(input$dataset, "_conjoint_pred.png"),
  caption = "Download conjoint prediction plot",
  plot = .predict_plot_conjoint,
  width = plot_width,
  height = ca_pred_plot_height
)

download_handler(
  id = "dlp_conjoint", 
  fun = download_handler_plot, 
  fn = paste0(input$dataset, "_conjoint.png"),
  caption = "Download conjoint plot",
  plot = .plot_conjoint,
  width = ca_plot_width,
  height = ca_plot_height
)
