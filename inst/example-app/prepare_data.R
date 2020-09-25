#' @title Prepare HDX data
#' @description  Prepares data.
#' @param input List
#' @param plt Plot type. Accepts either \code{'comparison'} or \code{'bar'}. Default \code{'comparison'}
#' @export
#'

produce_HaDeX_data <- function(input, plt = "comparison") {

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
                                                                                     time_0 = in_time,
                                                                                     time_t = chosen_time,
                                                                                     time_100 = out_time,
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
                                                           time_0 = in_time,
                                                           time_t = chosen_time,
                                                           time_100 = out_time,
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


