GetPossibleICAMSCatalog <- function(input.catalog) {
  if (mode(input.catalog) == "character") {
      input.catalog <- ICAMS::ReadCatalog(input.catalog, stop.on.error = FALSE)
      if (is.na(input.catalog[1,1])) {
        input.catalog <- data.table::fread(input.catalog)
      }
  }

  if(is.null(ncol(input.catalog))) {
    input.catalog <- matrix(input.catalog, nrow = length(input.catalog))
  }

  input.catalog <- input.catalog[,colSums(input.catalog)>0]
  return(input.catalog)
}
