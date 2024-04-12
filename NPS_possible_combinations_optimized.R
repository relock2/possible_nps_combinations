
# This version of the R function is designed to work more quickly than the
# original version by reducing the time complexity created by a for loop.
# In this function, all combinations are generated, then the results are
# filtered to only those that meet the required NPS.

find_nps_combinations_grid <- function(n, target_nps) {
  # Generate all combinations of promoters and detractors
  grid <- expand.grid(prom = 0:n, det = 0:n)
  grid <- grid[grid$prom + grid$det <= n, ]
  
  # Calculate passives based on promoters and detractors
  grid$pas <- n - grid$prom - grid$det
  
  # Calculate NPS
  grid$nps <- (grid$prom - grid$det) / n
  
  # Filter rows where NPS equals the target NPS
  valid_combinations <- grid[grid$nps == target_nps, ]
  
  # Generate sequences for each valid combination
  sequences <- lapply(seq_len(nrow(valid_combinations)), function(i) {
    c(rep(1, valid_combinations$prom[i]),
      rep(0, valid_combinations$pas[i]),
      rep(-1, valid_combinations$det[i]))
  })
  
  return(sequences)
}