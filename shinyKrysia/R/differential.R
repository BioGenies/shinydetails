#' generate_differential_data
#'
#' @description Generates differential data, based on the supplied
#' parameters.
#'
#' @param dat custom format, produced by
#' \code{\link{generate_differential_data_set}}
#' @param theoretical ...
#' @param relative ...
#' @param confidence_limit_1 ...
#' @param confidence_limit_2 ...
#'
#' @details This data is available in the GUI. The names of the parameters
#' and variables will be changed later after the glossary project.
#'
#' @return ...
#'
#' @seealso ...
#'
#' @export generate_differential_data

generate_differential_data <- function(dat,
                                       theoretical,
                                       relative,
                                       confidence_limit_1,
                                       confidence_limit_2){

  column_name_cl1 <- paste0("Valid At ", confidence_limit_1)
  column_name_cl2 <- paste0("Valid At ", confidence_limit_2)

  if(theoretical){

    if(relative){
      # theoretical & relative
      dat %>%
        add_stat_dependency(confidence_limit = confidence_limit_1,
                            theoretical = TRUE,
                            relative = TRUE) %>%
        add_stat_dependency(confidence_limit = confidence_limit_2,
                            theoretical = TRUE,
                            relative = TRUE) %>%
        select(Protein, Sequence, Start, End, diff_theo_frac_exch, err_diff_theo_frac_exch, paste0("valid_at_", confidence_limit_1), paste0("valid_at_", confidence_limit_2)) %>%
        mutate(diff_theo_frac_exch = round(diff_theo_frac_exch, 4),
               err_diff_theo_frac_exch = round(err_diff_theo_frac_exch, 4)) %>%
        arrange(Start, End) %>%
        rename("Theo Diff Frac Exch" = diff_theo_frac_exch,
               "Err Theo Diff Frac Exch" = err_diff_theo_frac_exch,
               "{column_name_cl1}" := paste0("valid_at_", confidence_limit_1),
               "{column_name_cl2}" := paste0("valid_at_", confidence_limit_2))

    } else {
      # theoretical & absolute
      dat %>%
        add_stat_dependency(confidence_limit = confidence_limit_1,
                            theoretical = TRUE,
                            relative = FALSE) %>%
        add_stat_dependency(confidence_limit = confidence_limit_2,
                            theoretical = TRUE,
                            relative = FALSE) %>%
        select(Protein, Sequence, Start, End, abs_diff_theo_frac_exch, err_abs_diff_theo_frac_exch, paste0("valid_at_", confidence_limit_1), paste0("valid_at_", confidence_limit_2)) %>%
        mutate(abs_diff_theo_frac_exch = round(abs_diff_theo_frac_exch, 4),
               err_abs_diff_theo_frac_exch = round(err_abs_diff_theo_frac_exch, 4)) %>%
        arrange(Start, End) %>%
        rename("Theo Abs Value Diff" = abs_diff_theo_frac_exch,
               "Err Theo Abs Value Diff" = err_abs_diff_theo_frac_exch,
               "{column_name_cl1}" := paste0("valid_at_", confidence_limit_1),
               "{column_name_cl2}" := paste0("valid_at_", confidence_limit_2))
    }

  } else {

    if(relative){
      # experimental &
      dat %>%
        add_stat_dependency(confidence_limit = confidence_limit_1,
                            theoretical = FALSE,
                            relative = TRUE) %>%
        add_stat_dependency(confidence_limit = confidence_limit_2,
                            theoretical = FALSE,
                            relative = TRUE) %>%
        select(Protein, Sequence, Start, End, diff_frac_exch, err_frac_exch, paste0("valid_at_", confidence_limit_1), paste0("valid_at_", confidence_limit_2)) %>%
        mutate(diff_frac_exch = round(diff_frac_exch, 4),
               err_frac_exch = round(err_frac_exch, 4)) %>%
        arrange(Start, End) %>%
        rename("Diff Frac Exch" = diff_frac_exch,
               "Err Diff Frac Exch" = err_frac_exch,
               "{column_name_cl1}" := paste0("valid_at_", confidence_limit_1),
               "{column_name_cl2}" := paste0("valid_at_", confidence_limit_2))

    } else {
      # experimental & absolute
      dat %>%
        add_stat_dependency(confidence_limit = confidence_limit_1,
                            theoretical = FALSE,
                            relative = FALSE) %>%
        add_stat_dependency(confidence_limit = confidence_limit_2,
                            theoretical = FALSE,
                            relative = FALSE) %>%
        select(Protein, Sequence, Start, End, abs_diff_frac_exch, err_abs_diff_frac_exch, paste0("valid_at_", confidence_limit_1), paste0("valid_at_", confidence_limit_2)) %>%
        mutate(abs_diff_frac_exch = round(abs_diff_frac_exch, 4),
               err_abs_diff_frac_exch = round(err_abs_diff_frac_exch, 4)) %>%
        arrange(Start, End) %>%
        rename("Diff Abs Value Exch" = abs_diff_frac_exch,
               "Err Diff Abs Value Exch" = err_abs_diff_frac_exch,
               "{column_name_cl1}" := paste0("valid_at_", confidence_limit_1),
               "{column_name_cl2}" := paste0("valid_at_", confidence_limit_2))
    }
  }
}

#' generate_differential_data_set
#'
#' @description Generate the data frame with differential data from two
#' provided states - experimental/relative calculations with the
#' uncertainty, based on supplied parameters.
#'
#' @param dat ...
#' @param states vector of two states to calculate difference between them,
#' the order is important
#' @param protein ...
#' @param time_in ...
#' @param time_chosen ...
#' @param time_out ...
#' @param deut_part ...
#'
#' @details The names of the parameters and variables will be changed
#' later after the glossary project.
#'
#' @return ...
#'
#' @seealso ...
#'
#' @export generate_differential_data_set

generate_differential_data_set <- function(dat,
                                           states,
                                           protein,
                                           time_in,
                                           time_chosen,
                                           time_out,
                                           deut_part){

  bind_rows(lapply(states, function(i) calculate_state_deuteration(dat,
                                                                   protein = protein,
                                                                   state = i,
                                                                   time_in = time_in,
                                                                   time_chosen = time_chosen,
                                                                   time_out = time_out,
                                                                   deut_part = deut_part))) %>%
    droplevels() %>%
    mutate(State = factor(State, levels = states, labels = c("1", "2"))) %>%
    gather(variable, value, -c(Protein:End, State, Med_Sequence)) %>%
    unite(tmp, variable, State) %>%
    spread(tmp, value)  %>%
    mutate(diff_frac_exch = frac_exch_state_1 - frac_exch_state_2,
           err_frac_exch = sqrt(err_frac_exch_state_1^2 + err_frac_exch_state_2^2),
           abs_diff_frac_exch = abs_frac_exch_state_1 - abs_frac_exch_state_2,
           err_abs_diff_frac_exch = sqrt(err_abs_frac_exch_state_1^2 + err_abs_frac_exch_state_2^2),
           diff_theo_frac_exch = avg_theo_in_time_1 - avg_theo_in_time_2,
           err_diff_theo_frac_exch = sqrt(err_avg_theo_in_time_1^2 + err_avg_theo_in_time_2^2),
           abs_diff_theo_frac_exch = abs_avg_theo_in_time_1 - abs_avg_theo_in_time_2,
           err_abs_diff_theo_frac_exch = sqrt(err_abs_avg_theo_in_time_1^2 + err_abs_avg_theo_in_time_2^2)) %>%
    select(Protein, Start, End, Med_Sequence, everything(), -contains("1"), -contains("2"))
}


#' generate_differential_plot
#'
#' @description Generates differential (Woods) plot with confidence values
#' based on supplied data and parameters.
#'
#' @param dat produced by \code{\link{generate_differential_data_set}} function
#' @param theoretical ...
#' @param relative ...
#' @param confidence_limit ...
#' @param confidence_limit_2 ...
#'
#' @details This plot is visible in GUI. The names of the parameters
#' and variables will be changed later after the glossary project.
#'
#' @return ...
#'
#' @seealso ...
#'
#' @export generate_differential_plot

generate_differential_plot <- function(dat,
                                       theoretical,
                                       relative,
                                       confidence_limit,
                                       confidence_limit_2){

  if(theoretical){

    if(relative){
      # theoretical & relative
      interval <- calculate_confidence_limit_values(calc_dat = dat,
                                                    confidence_limit = confidence_limit,
                                                    theoretical = TRUE,
                                                    relative = TRUE)

      interval_2 <- calculate_confidence_limit_values(calc_dat = dat,
                                                      confidence_limit = confidence_limit_2,
                                                      theoretical = TRUE,
                                                      relative = TRUE)

      mutate(dat, colour = case_when(
        dat[["diff_theo_frac_exch"]] < interval_2[1] ~ "deepskyblue3",
        dat[["diff_theo_frac_exch"]] < interval[1] ~ "deepskyblue1",
        dat[["diff_theo_frac_exch"]] > interval_2[2] ~ "firebrick3",
        dat[["diff_theo_frac_exch"]] > interval[2] ~ "firebrick1",
        TRUE ~ "azure3")) %>%
        ggplot() +
        geom_segment(aes(x = Start, y = diff_theo_frac_exch, xend = End, yend = diff_theo_frac_exch, color = colour)) +
        geom_errorbar(aes(x = Med_Sequence, ymin = diff_theo_frac_exch - err_diff_theo_frac_exch, ymax = diff_theo_frac_exch + err_diff_theo_frac_exch, color = colour)) +
        geom_hline(yintercept = 0, linetype = "dotted", color = "green", size = .7) +
        geom_hline(aes(yintercept = interval[1], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "deepskyblue1", size = .7, show.legend = TRUE) +
        geom_hline(aes(yintercept = interval[2], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "firebrick1", size = .7, show.legend = FALSE) +
        geom_hline(aes(yintercept = interval_2[1], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "deepskyblue3", size = .7, show.legend = TRUE) +
        geom_hline(aes(yintercept = interval_2[2], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "firebrick3", size = .7, show.legend = FALSE) +
        scale_linetype_manual(values = c("dashed", "dotdash")) +
        scale_colour_identity() +
        scale_y_continuous(expand = c(0, 0), limits = c(-100, 100)) +
        theme(legend.title = element_blank(),
              legend.position = "bottom",
              legend.direction = "vertical")

    } else {
      # theoretical & absolute
      interval <- calculate_confidence_limit_values(calc_dat = dat,
                                                    confidence_limit = confidence_limit,
                                                    theoretical = TRUE,
                                                    relative = FALSE)

      interval_2 <- calculate_confidence_limit_values(calc_dat = dat,
                                                      confidence_limit = confidence_limit_2,
                                                      theoretical = TRUE,
                                                      relative = FALSE)

      mutate(dat, colour = case_when(
        dat[["abs_diff_theo_frac_exch"]] < interval_2[1] ~ "deepskyblue3",
        dat[["abs_diff_theo_frac_exch"]] < interval[1] ~ "deepskyblue1",
        dat[["abs_diff_theo_frac_exch"]] > interval_2[2] ~ "firebrick3",
        dat[["abs_diff_theo_frac_exch"]] > interval[2] ~ "firebrick1",
        TRUE ~ "azure3")) %>%
        ggplot() +
        geom_segment(aes(x = Start, y = abs_diff_theo_frac_exch, xend = End, yend = abs_diff_theo_frac_exch, color = colour)) +
        geom_errorbar(aes(x = Med_Sequence, ymin = abs_diff_theo_frac_exch - err_abs_diff_theo_frac_exch, ymax = abs_diff_theo_frac_exch + err_abs_diff_theo_frac_exch, color = colour)) +
        geom_hline(yintercept = 0, linetype = "dotted", color = "green", size = .7) +
        geom_hline(aes(yintercept = interval[1], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "deepskyblue1", size = .7, show.legend = TRUE) +
        geom_hline(aes(yintercept = interval[2], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "firebrick1", size = .7, show.legend = FALSE) +
        geom_hline(aes(yintercept = interval_2[1], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "deepskyblue3", size = .7, show.legend = TRUE) +
        geom_hline(aes(yintercept = interval_2[2], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "firebrick3", size = .7, show.legend = FALSE) +
        scale_linetype_manual(values = c("dashed", "dotdash")) +
        scale_colour_identity() +
        scale_y_continuous(expand = c(0, 0), limits = c(-1, 1)) +
        theme(legend.title = element_blank(),
              legend.position = "bottom",
              legend.direction = "vertical")
    }

  } else {

    if(relative){
      # experimental & relative
      interval <- calculate_confidence_limit_values(calc_dat = dat,
                                                    confidence_limit = confidence_limit,
                                                    theoretical = FALSE,
                                                    relative = TRUE)

      interval_2 <- calculate_confidence_limit_values(calc_dat = dat,
                                                      confidence_limit = confidence_limit_2,
                                                      theoretical = FALSE,
                                                      relative = TRUE)

      mutate(dat, colour = case_when(
        dat[["diff_frac_exch"]] < interval_2[1] ~ "deepskyblue3",
        dat[["diff_frac_exch"]] < interval[1] ~ "deepskyblue1",
        dat[["diff_frac_exch"]] > interval_2[2] ~ "firebrick3",
        dat[["diff_frac_exch"]] > interval[2] ~ "firebrick1",
        TRUE ~ "azure3")) %>%
        ggplot() +
        geom_segment(aes(x = Start, y = diff_frac_exch, xend = End, yend = diff_frac_exch, color = colour)) +
        geom_errorbar(aes(x = Med_Sequence, ymin = diff_frac_exch - err_frac_exch, ymax = diff_frac_exch + err_frac_exch, color = colour)) +
        geom_hline(yintercept = 0, linetype = "dotted", color = "green", size = .7) +
        geom_hline(aes(yintercept = interval[1], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "deepskyblue1", size = .7, show.legend = TRUE) +
        geom_hline(aes(yintercept = interval[2], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "firebrick1", size = .7, show.legend = FALSE) +
        geom_hline(aes(yintercept = interval_2[1], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "deepskyblue3", size = .7, show.legend = TRUE) +
        geom_hline(aes(yintercept = interval_2[2], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "firebrick3", size = .7, show.legend = FALSE) +
        scale_linetype_manual(values = c("dashed", "dotdash")) +
        scale_colour_identity() +
        scale_y_continuous(expand = c(0, 0), limits = c(-100, 100)) +
        theme(legend.title = element_blank(),
              legend.position = "bottom",
              legend.direction = "vertical")

    } else {
      # experimental & absolute
      interval <- calculate_confidence_limit_values(calc_dat = dat,
                                                    confidence_limit = confidence_limit,
                                                    theoretical = FALSE,
                                                    relative = FALSE)

      interval_2 <- calculate_confidence_limit_values(calc_dat = dat,
                                                      confidence_limit = confidence_limit_2,
                                                      theoretical = FALSE,
                                                      relative = FALSE)

      mutate(dat, colour = case_when(
        dat[["abs_diff_frac_exch"]] < interval_2[1] ~ "deepskyblue3",
        dat[["abs_diff_frac_exch"]] < interval[1] ~ "deepskyblue1",
        dat[["abs_diff_frac_exch"]] > interval_2[2] ~ "firebrick3",
        dat[["abs_diff_frac_exch"]] > interval[2] ~ "firebrick1",
        TRUE ~ "azure3")) %>%
        ggplot() +
        geom_segment(aes(x = Start, y = abs_diff_frac_exch, xend = End, yend = abs_diff_frac_exch, color = colour)) +
        geom_errorbar(aes(x = Med_Sequence, ymin = abs_diff_frac_exch - err_abs_diff_frac_exch, ymax = abs_diff_frac_exch + err_abs_diff_frac_exch, color = colour)) +
        geom_hline(yintercept = 0, linetype = "dotted", color = "green", size = .7) +
        geom_hline(aes(yintercept = interval[1], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "deepskyblue1", size = .7, show.legend = TRUE) +
        geom_hline(aes(yintercept = interval[2], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "firebrick1", size = .7, show.legend = FALSE) +
        geom_hline(aes(yintercept = interval_2[1], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "deepskyblue3", size = .7, show.legend = TRUE) +
        geom_hline(aes(yintercept = interval_2[2], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "firebrick3", size = .7, show.legend = FALSE) +
        scale_linetype_manual(values = c("dashed", "dotdash")) +
        scale_colour_identity() +
        scale_y_continuous(expand = c(0, 0), limits = c(-1, 1)) +
        theme(legend.title = element_blank(),
              legend.position = "bottom",
              legend.direction = "vertical")

    }
  }

}
