help_multivariate <- c(
  "(Dis)similarity map" = "mds.md", "Attribute map" = "prmap.md",
  "Pre-factor" = "pre_factor.md", "Factor" = "full_factor.md",
  "Hierarchical clustering" = "hclus.md", "K-clustering" = "kclus.md",
  "Conjoint" = "conjoint.md"
)

output$help_multivariate <- reactive(append_help("help_multivariate", file.path(getOption("radiant.path.multivariate"), "app/tools/help/")))
observeEvent(input$help_multivariate_all, {
  help_switch(input$help_multivariate_all, "help_multivariate")
})
observeEvent(input$help_multivariate_none, {
  help_switch(input$help_multivariate_none, "help_multivariate", help_on = FALSE)
})

help_multivariate_panel <- tagList(
  wellPanel(
    HTML("<label>Multivariate menu: <i id='help_multivariate_all' title='Check all' href='#' class='action-button glyphicon glyphicon-ok'></i>
    <i id='help_multivariate_none' title='Uncheck all' href='#' class='action-button glyphicon glyphicon-remove'></i></label>"),
    checkboxGroupInput(
      "help_multivariate", NULL, help_multivariate,
      selected = state_group("help_multivariate"), inline = TRUE
    )
  )
)
