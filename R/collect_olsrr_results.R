#' ---
#' title: "Collection of olsrr Result Across Samples"
#' date:  "2020-04-27"
#' ---
#'
#'
#' @title Collect Results from olsrr Modelselection Across Sample Datasets
#'
#' @description
#' For very large datasets model selection is done on smaller subsamples of the
#' complete dataset. Given the subsamples, model selection is run and a set
#' of results is collected. This function is not suitable because the package olsrr
#' can only be used for regression models (models with covariables only).
#' Therefore it does not make sense to use fixed effects (factors) in olsrr model selection.
#'
#' @details
#' Currently model selection is done using \code{olsrr::ols_step_backward_aic()}
#'
#' @return tbl_result tibble with results of the model selection for each sample.
#'
#' @examples
#' \dontrun{
#' n_nr_sample <- 10
#' n_sample_size <- 30
#' # samples from mtcars and write samples to files in tmpdir
#' tmpdir <- tempdir()
#' cat(" * Sample directory: ", tmpdir, "\n")
#' vec_sample_files <- sapply(1:n_nr_sample,
#'                            function(x) paste0("mtcars_sample_", x, ".csv", collapse = ""),
#'                            USE.NAMES = FALSE)
#' vec_sample_path <- file.path(tmpdir, vec_sample_files)
#' set.seed(321)
#' for (i in 1:n_nr_sample){
#'   cat(" * Generate sample ", i, " ...\n")
#'   cur_sample <- dplyr::sample_n(mtcars, n_sample_size, replace = TRUE)
#'   readr::write_csv(cur_sample, path = vec_sample_path[i])
#' }
#' collect_ols_results(pvec_sample = vec_sample_path, ps_response = "mpg")
#' }
#'
#' @param pvec_sample vector of paths to data samples
#' @param ps_response name of the response variable
#' @param pvec_ignoring_columns vector of columns to ignore
#' @return tbl_result tibble with model selection results
#'
#' @importFrom magrittr `%>%`
#'
#' @export collect_ols_results
collect_ols_results <- function(pvec_sample, ps_response, pvec_ignoring_columns = NULL){

  # read the first sample
  tbl_first_sample <- read_sample(ps_sample_path = pvec_sample[1])
  # ignore some columns
  if (!is.null(pvec_ignoring_columns)){
    tbl_first_sample <- tbl_first_sample %>% dplyr::select(-c(pvec_ignoring_columns))
  }
  # check that ps_response is in colnames(tbl_first_sample)
  if (!ps_response %in% colnames(tbl_first_sample))
    stop(" * ERROR: Cannot find response variable: ", ps_response, " in columnnames of first sample.")
  # check that no column is of type factor
  if (any(sapply(colnames(tbl_first_sample), function(x) is.factor(tbl_first_sample[[x]]), USE.NAMES = FALSE)))
    stop(" * ERROR in collect_ols_results: Cannot handle fixed effects - use function collect_stepAIC_results")
  # determine the predictors from column of first sample
  vec_pred <- setdiff(names(tbl_first_sample), ps_response)
  # the vector of column names of the result tibble
  vec_col <- c("SampleID", "(Intercept)", vec_pred, "AIC")

  # determine the number of samples
  n_nr_sample <- length(pvec_sample)
  # initialise the result tibble
  tbl_result <- tibble::as_tibble(matrix(0, nrow = n_nr_sample, ncol = length(vec_col)))
  colnames(tbl_result) <- vec_col

  # specify the formula for the full model
  formula.full <- as.formula(paste( ps_response, paste0(vec_pred, collapse = " + "), sep = " ~ "))

  # loop over the samples and do the model selection
  for (sidx in seq_along(pvec_sample)){
    tbl_cur_sample <- read_sample(ps_sample_path = pvec_sample[sidx])
    # check whether response variable is in colnames of tbl_cur_sample
    if (!ps_response %in% colnames(tbl_cur_sample))
      stop(" * ERROR: Cannot find response variable: ", ps_response, " in columnnames of current sample.")
    # check that no column is of type factor
    if (any(sapply(colnames(tbl_cur_sample), function(x) is.factor(tbl_cur_sample[[x]]), USE.NAMES = FALSE)))
      stop(" * ERROR in collect_ols_results: Cannot handle fixed effects - use function collect_stepAIC_results")

    # fit full model to current sample data
    lm.full.cur <- lm(formula = formula.full, data = tbl_cur_sample)
    ols_result <- olsrr::ols_step_backward_aic(model = lm.full.cur)
    tbl_result[sidx, names(coefficients(ols_result$model))] <- 1
    tbl_result[sidx, "AIC"] <- ols_result$aics[length(ols_result$aics)]
    tbl_result[sidx, "SampleID"] <- sidx
  }

  # return the resulting tibble
  return(tbl_result)
}
