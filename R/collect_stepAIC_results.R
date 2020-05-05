#' ---
#' title: "Collection of MASS::stepAIC Results"
#' date:  "2020-04-29"
#' #' ---
#'
#'
#' @title Collect Results From MASS::stepAIC Across Sample Datasets
#'
#' @description
#' Model selection is done with \code{MASS::stepAIC()} for a set of sample
#' datasets. The results are collected into a tibble and returned to the
#' caller.
#'
#'#' @examples
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
#' collect_stepAIC_results(pvec_sample = vec_sample_path, ps_response = "mpg")
#' }
#'
#' @param pvec_sample vector of paths to sample files
#' @param ps_response response variable
#' @param pvec_ignoring_columns columns to be ignored in sample data
#' @param pvec_fixeffect_columns columns to be treated as fixed effects
#' @param pb_trace flag to indicate whether stepAIC produced output
#' @return tbl_result tibble with model selection results
#'
#' @importFrom magrittr `%>%`
#'
#' @export collect_stepAIC_results
collect_stepAIC_results <- function(pvec_sample, ps_response,
                                    pvec_ignoring_columns = NULL,
                                    pvec_fixeffect_columns = NULL,
                                    pb_trace = FALSE){
  # read first sample file
  tbl_first_sample <- read_sample(ps_sample_path = pvec_sample[1])
  # ignore some columns
  if (!is.null(pvec_ignoring_columns)){
    tbl_first_sample <- tbl_first_sample %>% dplyr::select(-c(pvec_ignoring_columns))
  }
  # check that ps_response is in colnames(tbl_first_sample)
  if (!ps_response %in% colnames(tbl_first_sample))
    stop(" * ERROR in collect_stepAIC_results: Cannot find response variable: ", ps_response, " in columnnames of first sample.")

  # determine the predictors from column of first sample
  vec_pred <- setdiff(names(tbl_first_sample), ps_response)
  # the vector of column names of the result tibble
  vec_col <- c("SampleID", vec_pred, "AIC")

  # determine the number of samples
  n_nr_sample <- length(pvec_sample)
  # initialise the result tibble
  tbl_result <- tibble::as_tibble(matrix(0, nrow = n_nr_sample, ncol = length(vec_col)))
  colnames(tbl_result) <- vec_col

  # specify the formula for the full model
  formula.full <- as.formula(paste( ps_response, paste0(vec_pred, collapse = " + "), sep = " ~ "))
  # loop over the samples and do the model selection
  for (sidx in seq_along(pvec_sample)){
    # reading the current dataset
    tbl_cur_sample <- read_sample(ps_sample_path =  pvec_sample[sidx])
    # check that ps_response is in colnames(tbl_cur_sample)
    if (!ps_response %in% colnames(tbl_cur_sample))
      stop(" * ERROR in collect_stepAIC_results: Cannot find response variable: ", ps_response, " in columnnames of current sample.")
    # ps_response assure that have a record
    tbl_cur_sample <- tbl_cur_sample[tbl_cur_sample[[ps_response]] != 0,]
    # Distinguish between fix effects (as.factor) and covariables (still numeric)
    if (!is.null(pvec_fixeffect_columns)){
      for(fx in pvec_fixeffect_columns){
        tbl_cur_sample[[fx]] <- as.factor(tbl_cur_sample[[fx]])
      }
    }
    lm.full.cur <- lm(formula = formula.full, data = tbl_cur_sample)
    step_result <- MASS::stepAIC(lm.full.cur, direction = 'backward', trace = pb_trace)
    tbl_result[sidx, labels(terms(step_result))] <- 1
    tbl_result[sidx, "AIC"] <- step_result$anova$AIC[length(step_result$anova$AIC)]
    tbl_result[sidx, "SampleID"] <- sidx
  }

  # return the resulting tibble
  return(tbl_result)
}


#' --- Reading Sample files ---
#'
#' @title Read a Sample File
#'
#' @description
#' The function distinguishes between files with extension "csv" and "txt" or "out".
#' Sample files with extension "csv" are read with readr::read_csv and sample files
#' with extension "txt" or "out" are read with readr::read_delim. Sample files with
#' extensions other than "csv", "txt" or "out" are not read and produce an error.
#'
#' @param ps_sample_path path to sample file
#' @return tbl_sample tibble read from sample file
read_sample <- function(ps_sample_path){
  #depending of the file-extension, different reading functions for the first sample
  if(assertthat::has_extension(path = ps_sample_path, ext = "csv")){
    tbl_sample <- readr::read_csv(file = ps_sample_path, col_types = readr::cols())
  } else if(assertthat::has_extension(path = ps_sample_path, ext = "txt") ||
            assertthat::has_extension(path = ps_sample_path, ext = "out")){
    tbl_sample <- readr::read_delim(file = ps_sample_path, delim = " ", col_types = readr::cols())
  } else{
    stop(" * ERROR in read_sample: Cannot find a defined file-extension of the first file: ", ps_sample_path)
  }
  return(tbl_sample)
}
