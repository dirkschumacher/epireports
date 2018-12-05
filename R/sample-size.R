#' Compute a sample size for a proportion
#'
#'
#'
#' @references
#' This method is based on the work of Kevin M. Sullivan and Andrew G. Dean
#' of OpenEpi.
#'
#' @export
sample_size <- function(population_size, expected_prevalence,
                        precision, design_effect, alpha = 0.05) {
  num <- (design_effect * population_size * expected_prevalence *
            (1 - expected_prevalence))
  denom <- precision^2 / stats::qnorm(1 - alpha / 2)^2 * (population_size - 1) +
              expected_prevalence * (1 - expected_prevalence)
  as.integer(round(num / denom))
}

#' Take a sample size of children and convert it to households
#'
#' @param sample_size The sample size in terms of children
#' @param avg_hh average household size
#' @param prop_under_5 proportion of children under 5 in the population
#' @param frac_6_59 fraction of `prop_under_5` between 6 and 59 months of age
#' @param @non_response_rate the expected rate of non-response
#' @references
#' Sampling Methods and Sample Size Calculation for the SMART Methodology
#' <https://www.humanitarianresponse.info/sites/www.humanitarianresponse.info/files/documents/files/Sampling_Paper_June_2012.pdf>
#'
#' @export
#' @examples
#' # Assuming 0% non-response ------------------
#' sample_size_households(500,                 # 500 children
#'                        avg_hh = 6.2,        # avg household size is 6.2
#'                        prop_under_5 = 0.15, # 15% of children under 5
#'                        frac_6_59 = 0.9)     # 90% of under 5 between 6-59 months
#' # Result should be 598 households
#'
#' # Assuming 8% non-response ------------------
#' sample_size_households(500,                 # 500 children
#'                        avg_hh = 6.2,        # avg household size is 6.2
#'                        prop_under_5 = 0.15, # 15% of children under 5
#'                        frac_6_59 = 0.9,     # 90% of under 5 between 6-59 months
#'                        non_response_rate = 0.08)
#' # Result should be 650 households
sample_size_households <- function(sample_size, avg_hh,
                                   prop_under_5,
                                   frac_6_59, non_response_rate = 0) {
  hh <- sample_size / (avg_hh * prop_under_5 * frac_6_59)
  hh <- hh / (1 - non_response_rate)
  as.integer(ceiling(hh))
}
