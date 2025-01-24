#' Initialize hdp object
#' Allocate process index for hdp initialization.
#' Prepare for \code{\link[hdpx]{hdp_init}}
#'
#' @param input.catalog Input spectra catalog as a matrix or
#' in \code{\link[ICAMS]{ICAMS}} format.
#'
#' @param K.guess Suggested initial value of the number of
#' signatures, passed to \code{\link[hdpx]{dp_activate}} as
#' \code{initcc}.
#'
#' @param multi.types A logical scalar or
#' a character vector.
#' If \code{FALSE}, The HDP analysis
#'   will regard all input spectra as one tumor type.
#'
#' If \code{TRUE}, the HDP analysis
#'   will infer tumor types based on the string before "::" in their names.
#' e.g. tumor type for "SA.Syn.Ovary-AdenoCA::S.500" would be "SA.Syn.Ovary-AdenoCA"
#'
#' If \code{multi.types} is a character vector, then it should be of the same length
#' as the number of columns in \code{input.catalog}, and each value is the
#' name of the tumor type of the corresponding column in \code{input.catalog}.
#'
#' e.g. \code{c("SA.Syn.Ovary-AdenoCA", "SA.Syn.Kidney-RCC")}.
#'
#' @param verbose If \code{TRUE} then \code{message} progress information.
#'
#' @param gamma.alpha Shape parameter of gamma distribution from which
#'   the Dirichlet process concentration parameters are drawn; in this
#'   function the gamma distributions for all Dirichlet processes are the same.
#'
#' @param gamma.beta Inverse scale parameter (rate parameter) of gamma distribution
#'   from which the Dirichlet process concentration parameters are drawn; in this
#'   function the gamma distributions for all Dirichlet processes are the same.
#'
#' @param one.parent.hack IF TRUE, the vector of parents for the Dirichlet
#'   processes looks like c(0, 1, 1, 1, 1, ...), not c(0, 1, 2, 2, 2, ....).

PrepInit <- function(multi.types,
                     input.catalog,
                     verbose,
                     K.guess,
                     gamma.alpha=1,
                     gamma.beta=1,
                     one.parent.hack = FALSE){

  if (mode(input.catalog) == "character") {
    if (verbose) message("Reading input catalog file ", input.catalog)
    input.catalog <- ICAMS::ReadCatalog(input.catalog, strict = FALSE)
  } else {
    input.catalog <- input.catalog
  }

  # hdp gets confused if the class of its input is not matrix.
  convSpectra <- t(input.catalog)
  # class(convSpectra) <- "matrix"
  # convSpectra <- t(convSpectra)

  number.channels <- nrow(input.catalog)
  number.samples  <- ncol(input.catalog)

  if (verbose) {
    message("Guessed number of Dirichlet process raw data clusters) = ",
            K.guess)
  }

  if (multi.types == FALSE) { # All tumors belong to one tumor type
    num.tumor.types <- 1
    ppindex <- c(0,1,rep(2,number.samples)) # Parent Dirichlet process index
    if (one.parent.hack) ppindex <- c(0, rep(1, number.samples))
  } else {
    if (multi.types == TRUE) {
      sample.names <- colnames(input.catalog)
      if (!all(grepl("::", sample.names)))
        stop("Every sample name needs to be of",
             " the form <sample_type>::<sample_id>")

      tumor.types <- sapply(
        sample.names,
        function(x) {strsplit(x, split = "::", fixed = T)[[1]][1]})

      num.tumor.types <- length(unique(tumor.types))
    } else if (is.character(multi.types)) {
      num.tumor.types <- length(unique(multi.types))
      tumor.types <- multi.types
    } else {
      stop("multi.types should be TRUE, FALSE, or a character vector of tumor types")
    }
    # 0 refers to the grandparent Dirichlet process node.
    # There is a level-one node for each tumor type, indicated by a 1.
    ppindex <- c(0, rep(1, num.tumor.types))

    # Each tumor type gets its own number.
    ppindex <- c(ppindex, 1 + as.numeric(as.factor(tumor.types))) # To do, update this with the more transparent code
    # cat(ppindex, "\n")
    # ppindex is now something like
    # c(0, 1, 1, 2, 2, 2, 3, 3)
    # 0 is grandparent
    # 1 is a parent of one type (there are 2 types)
    # 2 indicates tumors of the first type
    # 3 indicates tumors of second type
  }

  cpindex <- 1 + ppindex

  # Calculate the number of levels in the DP node tree.
  dp.levels <- length(unique(ppindex))

  if (verbose) {
    message("Gamma distribution was set to shape = ", gamma.alpha,
            " rate (inverse scale) = ", gamma.beta)
  }

  alphaa <- rep(gamma.alpha, dp.levels)
  alphab <- rep(gamma.beta,  dp.levels)
  invisible(list(num.tumor.types = num.tumor.types,
                 number.channels = number.channels,
                 convSpectra     = convSpectra,
                 ppindex         = ppindex,
                 cpindex         = cpindex,
                 alphaa          = alphaa,
                 alphab          = alphab))
}
