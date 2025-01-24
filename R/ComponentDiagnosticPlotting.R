#' Generate diagnostic plots from the return value of \code{\link{CombineChainsAndExtractSigs}}
#'
#' @param retval Return from \code{\link{CombineChainsAndExtractSigs}.}
#'
#' @param input.catalog Input spectra catalog as a matrix.
#'
#' @inheritParams SaveAnalysis
#'
#' @importFrom grDevices dev.off pdf
#'
#' @importFrom graphics par
#'
#' @details Generates the plots
#'
#'  - \code{diagnostics.signature.exposure.each.sample.pdf}
#'  - \code{diagnostics.components.in.which.gibbs.samples.pdf}
#'  - \code{diagnostics.likelihood.pdf}
#'  - \code{diagnostics.numcluster.pdf}
#'  - \code{diagnostics.signatures.pdf}
#'
#' @keywords internal

ComponentDiagnosticPlotting <- function(retval,
                                        input.catalog,
                                        out.dir,
                                        verbose){

  IS.ICAMS <- ICAMS::IsICAMSCatalog(input.catalog)

  multi <- retval$extracted.retval[["multi.chains"]] # class hdpSampleMulti
  chains <- hdpx::chains(multi)      # list of hdpSampleChain

  if (verbose) message("Writing HDP diagnostics")

  pdf(file = file.path(out.dir,"diagnostics.likelihood.pdf"))
  par(mfrow=c(2,2), mar=c(4, 4, 2, 1))
  lapply(chains, hdpx::plot_lik, bty = "L")
  dev.off()

  grDevices::pdf(file = file.path(out.dir,"diagnostics.numcluster.pdf"))
  # This is the number of raw clusters sampled along each chain
  par(mfrow=c(2,2), mar=c(4, 4, 2, 1))
  lapply(chains, hdpx::plot_numcluster, bty = "L")
  grDevices::dev.off()

  if (IS.ICAMS) {
    grDevices::pdf(
      file = file.path(out.dir,
                       "diagnostics.signature.exposure.each.sample.pdf"),
      paper = "a4")
    myCol <- grDevices::rainbow(ncol(retval$signature), alpha = 1)
    graphics::par(mfrow=c(1,1), mar=c(1, 1, 2, 1))

    PlotSamplesHighSigExp(retval           = retval,
                          hdpsample        = multi,
                          input.catalog    = input.catalog)

    grDevices::dev.off()
  }

  # hdpx::extract_components extracts information across chains
  # and does not have the chain information.
  # Therefore, we use hdpx::extract_ccc_from_hdp to find which
  # raw clusters are highly similar to components (most likely these clusters
  # contributed to the components during extract_components).

  ccc_0 <- lapply(chains, function(ch){
    lapply(hdpx::clust_categ_counts(ch), function(x){
      ans <- cbind(x)
      return(ans[, -ncol(ans)])
    })
  })

  cdc_0 <- lapply(chains, function(ch){
    lapply(hdpx::clust_dp_counts(ch), function(x){
      ans <- cbind(x)
      return(ans[, -ncol(ans)])
    })
  })
  sigmatchretval <- apply(retval$signature,2,function(x){
    hdpx::extract_ccc_from_hdp(x,
                               ccc_0 = ccc_0,
                               cos.merge = 0.90)})

  grDevices::pdf(file = file.path(out.dir,"diagnostics.signatures.pdf"))
  graphics::par(mfrow=c(8, 1), mar = c(1, 1, 1, 1))
  # This plots the component (signature) profiles with
  # 95% credibility intervals
  hdpx::plot_component_with_credint(retval =sigmatchretval)
  grDevices::dev.off()

  grDevices::pdf(
    file.path(out.dir,
              "diagnostics.components.in.which.gibbs.samples.pdf"))
  graphics::par(mfrow=c(1,1), mar=c(5, 4, 4, 2))
  hdpx::plot_component_posterior_samples(components = retval$signature,
                                         retval = sigmatchretval)
  grDevices::dev.off()
}



