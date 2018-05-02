# phipps

A plotting package for use for the VIMC to produce impact plots use by the consortium

## Example usage

```
library(phipps)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

data("impact_data", package = "phipps")

plot_parameters <- list(year_first = 2000,
                        year_last  = 2050,
                        outcome     = "deaths_averted",
                        touchstone = "201210gavi-201303gavi",
                        n_countries = 10)
                        
plot_data <- prep_impact_top_countries(impact_data, plot_parameters)
graph_impact_top_countries(plot_data, plot_parameters)
################################################################################
plot_parameters <- list(touchstone_blue       = "201310gavi",
                        touchstone_red        = "201510gavi",
                        touchstone_red_dotted = "201210gavi-201710gavi",
                        label_blue            = "label_1",
                        label_red             = "label_2",
                        label_red_dotted      = "label_3",
                        year_first            = 2000,
                        year_last             = 2025,
                        year_current          = 2016,
                        metric                = "deaths_averted",
                        focal_model           = TRUE,
                        vaccine_type          = "campaign",
                        vaccine               = "Measles",
                        support_type          = "gavi",
                        country_set           = "gavi73",
                        continent             = NULL,
                        country               = NULL,
                        region                = NULL,
                        title                 = "Title")
                        
plotable <- graph_tracking_targets(dat = impact_data, params = plot_parameters)
plotable$graph
################################################################################
plot_parameters <- list(touchstone            = "201210gavi-201303gavi",
                        year_first            = 2000,
                        year_last             = 2025,
                        year_current          = 2016,
                        metric                = "deaths_averted",
                        focal_model           = TRUE,
                        vaccine_type          = NULL,
                        vaccine               = NULL,
                        support_type          = "gavi",
                        country_set           = "gavi73",
                        continent             = NULL,
                        country               = NULL,
                        region                = NULL,
                        title                 = "Comparison of impact between gavi status by activity type",
                        n_plot                = 10)

plot_generic_compare(impact_data, plot_parameters, "gavi_cofin_status", "activity_type")
```
