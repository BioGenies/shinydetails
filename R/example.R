#' @title Prepare HDX data
#' @description  Prepares data.
#' @param input List
#' @param plt Plot type. Accepts either \code{'comparison'} or \code{'bar'}. Default \code{'comparison'}
#' @export
#'

prepare_data <- function(input, plt = "comparison") {

  if(!(plt %in% c("bar", "comparison", "differential"))) stop("plot_type must be either bar, differential or comparison.")

  dat_raw <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))

  switch (plt,
          comparison = {
            states_from_file <- unique(dat_raw[["State"]]) #reactive
            chosen_protein <- unique(dat_raw[["Protein"]]) #input
            in_time <- 0.001 #input
            chosen_time <- 5 #input
            out_time <- 1440 #input
            deut_concentration <- 100 #input

            prepared_dat <- bind_rows(lapply(states_from_file,
                                             function(i) calculate_state_deuteration(dat_raw,
                                                                                     protein = chosen_protein,
                                                                                     state = i,
                                                                                     time_in = in_time,
                                                                                     time_chosen = chosen_time,
                                                                                     time_out = out_time,
                                                                                     deut_part = 0.01*deut_concentration)))
          },
          differential = {
            two_states <- unique(dat_raw[["State"]]) # input
            chosen_protein <- unique(dat_raw[["Protein"]]) # input
            in_time <- 0.001 #input
            chosen_time <- 5 #input
            out_time <- 1440 #input
            deut_concentration <- 100 #input

            prepared_dat <- generate_differential_data_set(dat = dat_raw,
                                                           states = two_states,
                                                           protein = chosen_protein,
                                                           time_in = in_time,
                                                           time_chosen = chosen_time,
                                                           time_out = out_time,
                                                           deut_part = 0.01*deut_concentration)
          },
          bar = {
            chosen_protein <- unique(dat_raw[["Protein"]]) #input
            chosen_state <- unique(dat_raw[["State"]])[[1]]
            start_seq <- min(dat_raw[["Start"]]) # input
            end_seq <- max(dat_raw[["End"]]) # input
            protein_sequence <- reconstruct_sequence(filter(dat_raw, Protein == chosen_protein))

            prepared_dat <- generate_overlap_distribution_data(dat_raw,
                                                               protein = chosen_protein,
                                                               state = chosen_state,
                                                               start = start_seq,
                                                               end = end_seq,
                                                               protein_sequence = protein_sequence)
          }
  )
  prepared_dat
}

#' @title An example component
#' @description  Run an example app using shiny component
#' @export
#'



example_app <- function() {
  ui <- fluidPage(mainPanel(
    tabsetPanel(tabPanel(title = "comparison plot",
                         column(width = 3),
                         column(width = 9,
                                tabsetPanel_UI("comparison"))),
                tabPanel(title = "differential plot",
                         column(width = 3),
                         column(width = 9,
                                tabsetPanel_UI("differential"))),
                tabPanel(title = "bar plot",
                         column(width = 3),
                         column(width = 9,
                                tabsetPanel_UI("bar"))),
                tabPanel("plots 3"),
                tabPanel("plots 4"),
                tabPanel("plots 5")
    )
  )
  )


  server <- function(input, output) {
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
    tabsetPanel_SERVER(id = "bar",
                       plot_out = bar_plot_out,
                       table_out = bar_data,
                       plot_type = "bar",
                       tt_content = NULL)
  }


  shinyApp(ui = ui, server = server)
}

