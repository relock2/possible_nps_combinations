#' Calculate all possible combinations of promoter, detractor, and passive that
#' can result in a given Net Promoter Score with a certain population.
#'
#' Using assumptions and constraints provided by the user, this function calculates
#' all possible combinations of Promoters (+1), Passives (0), and Detractors (-1)
#' that can result in the specified Net Promoter Score and returns them as a list
#' of vectors. If just_number = TRUE, the function returns the number of possible
#' combinations that can result in that Net Promoter Score.
#'
#' @param n: The desired population
#' @param target_nps: A number between -1 and 1, representing the desired Net Promoter Score
#' @param just_number: If set to TRUE, the function will return the number of possible
#'                     combinations. If set to FALSE, it will return all possible
#'                     combinations as a list of vectors
#'
#' @return A list of vectors or the length of that list
#'
#' @examples
#' nps_combinations <- find_nps_combinations(10, target_nps = 0)
#' 
#' @export

find_nps_combinations <- function(n=100, target_nps = 0.5, just_number = TRUE) {
  
  # Check data types
  if (!is.numeric(n)) stop("Argument `n` must be an integer")
  if (!is.numeric(target_nps)) stop("Argument `target_nps` must be numeric")
  if (!is.logical(just_number)) stop("Argument `just_number` must be either TRUE or FALSE")
  
  # Check for invalid inputs
  if (n < 0) stop("n must be a positive integer.")
  if (target_nps < -1 | target_nps > 1) stop("target_nps must be between -1 and 1.")
  
  # Ensure n is an integer
  n <- round(n)
  
  combinations <- list()  # Initialize a list to store valid combinations
  
  # Iterate through prom and det up to n
  for (prom in 0:n) {
    for (det in 0:(n - prom)) {  # Adjust the range of det based on prom
      pas <- n - prom - det  # Calculate pas based on prom and det
      
      nps <- (prom * 1 + pas * 0 + det * -1) / n  # Calculate NPS
      
      if (nps == target_nps) {  # Check if NPS equals the target value
        sequence <- c(rep(1, prom), rep(0, pas), rep(-1, det))
        combinations <- c(combinations, list(sequence))
      }
    }
  }
  
  if (just_number) {
    
    return(length(combinations)) # Just return the number of combinations
  } else {
    return(combinations)  # Return the list of valid combinations
  }
}
