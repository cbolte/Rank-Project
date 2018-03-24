add_rank <- function(df) {
  det <- df
  det <- det[order(det$total_rkm, decreasing = TRUE), ]
  det$index <- c(1:nrow(motility))
  return(det)
}