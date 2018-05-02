test_that("Impact Plots", {

  context("Impact Plots")

  data("modified_update_201710_summary")
  data_set$gavi73 <- !(is.na(data_set$gavi_cofin_status))

  plot_parameters <- list(year_first  = 2000,
                          year_last   = 2050,
                          outcome     = "deaths_averted",
                          touchstone  = "201210gavi-201303gavi",
                          n_countries = 10)

  plot_data_1 <- prep_impact_top_countries(data_set, plot_parameters)
  plot_1 <- graph_impact_top_countries(plot_data_1, plot_parameters)
print(plot_1)
  vdiffr::expect_doppelganger("Test Impact 1", plot_1)
})
