library(shiny)
source("example.R")

shinyServer(function(input, output) {

  observe_helpers(session = getDefaultReactiveDomain(), help_dir = "helpfiles")

  #comparison plot and data

  prepared_comparison_dat <- prepare_data(input, "comparison")

  comparison_data <- reactive({
    generate_comparison_data(prepared_comparison_dat,
                             theoretical = FALSE, #input
                             relative = TRUE) #input
  })

  comparison_plot_out <- reactive({
    generate_comparison_plot(prepared_comparison_dat,
                             theoretical = FALSE, #input
                             relative = TRUE) #input
  })

  #differential plot and data

  prepared_differential_dat <- prepare_data(input, "differential")

  differential_data <- reactive({
    generate_differential_data(prepared_differential_dat,
                               theoretical = FALSE, # input
                               relative = TRUE, # input
                               confidence_limit = 0.98, # input
                               confidence_limit_2 = 0.99) # input
  })

  differential_plot_out <- reactive({
    generate_differential_plot(prepared_differential_dat,
                               theoretical = FALSE, # input
                               relative = TRUE, # input
                               confidence_limit = 0.98, # input
                               confidence_limit_2 = 0.99) # input
  })


  #bar plot and data

  prepated_distribution_dat <- prepare_data(input, "bar")

  bar_data <- reactive({
    prepated_distribution_dat
  })
  bar_plot_out <- reactive({
    generate_overlap_distribution_plot(prepated_distribution_dat,
                                       start = 0, # input
                                       end = 100) # input
  })


  tabsetPanel_SERVER(id = "comparison",
                     plot_out = comparison_plot_out,
                     table_out = comparison_data,
                     plot_type = "comparison",
                     tt_content = list(row_text = c("%s",  "Position: %i - %i", "Value: %f", "State: %s"),
                                       chosen_cols = c("Sequence", "Start", "End", "y_plot", "State")))
  tabsetPanel_SERVER(id = "differential",
                     plot_out = differential_plot_out,
                     table_out = differential_data,
                     plot_type = "differential",
                     tt_content = list(row_text = c("%s",  "Position: %i - %i", "Value: %f"),
                                       chosen_cols = c("Sequence", "Start", "End", "y_plot")))
  tabsetPanel_SERVER(id = "geom_col",
                     plot_out = bar_plot_out,
                     table_out = bar_data,
                     plot_type = "geom_col",
                     tt_content = NULL)

})
