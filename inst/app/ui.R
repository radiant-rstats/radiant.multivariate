## ui for multivariate menu in radiant
navbar_proj(
  do.call(
    navbarPage,
    c(
      "Radiant for R",
      getOption("radiant.nav_ui"),
      getOption("radiant.multivariate_ui"),
      getOption("radiant.shared_ui"),
      help_menu("help_multivariate_ui")
    )
  )
)
