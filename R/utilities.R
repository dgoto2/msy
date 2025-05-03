##############################################
# utility functions
##############################################

fGetValsScan <- function(Nums, RPs) {
  # Nums - character vector with values or the name of refererence points to be substituted
  # RPs - named vector of reference points

  if (length(Nums) == 1) {
    if (is.na(Nums)) {
      return(NA)
    }
  }

  # extract the numeric ones
  ret <- as.numeric(Nums[!is.na(suppressWarnings(as.numeric(Nums)))])

  # now the specified reference points
  if (sum(is.na(suppressWarnings(as.numeric(Nums)))) > 0) {
    ret <- c(ret, as.numeric(RPs[Nums[is.na(suppressWarnings(as.numeric(Nums)))]]))
  }

  # add a warning if there are unmatched RPs

  sort(ret)
}