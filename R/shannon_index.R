#' Calculate Shannon Index for diversity
#' 
#' This function calculates the Index of Shannon (1948)
#'  for a tibble with different species.
#'  
#' The formula that is used is 
#' \deqn{SI = -\sum_{S=1}^i (p_i * ln(p_i))}
#' where \eqn{p_i} is the relative abundance of each species (\eqn{S}).
#' 
#' @md
#' 
#' @references
#'  \insertRef{shannon1948}{SoilManageR}
#'  
#'  \insertRef{spellerberg2003tribute}{SoilManageR}
#' 
#' @seealso
#' [plant_diversity()] to calculate the [shannon_index()] 
#'  (and other diversity indices) for a management_df
#' 
#' @param var_tibble a tibble with two columns (name of species, count per species), 
#'  the second column must be called "count"
#' 
#' @return double of the Shannon Index 
#' 
#' @examples 
#' #create tibble
#' tibble_example <- tibble::tibble(Plant = c("A","B","C","D","E"), count = c(10,5,8,20,10))
#' 
#' #calculate Shannon Index
#' shannon_index(tibble_example)  # = 1.505...
#' 
#' @export

shannon_index <- function(var_tibble) {
 
  total_count <- sum(var_tibble$count)
  
  var_tibble <- var_tibble %>%
    dplyr::mutate(p_i = count/total_count) %>%
    dplyr::mutate(p_index = p_i * log(p_i))
  
  shannon_index <- -sum(var_tibble$p_index)
  
  return(shannon_index)
   
}
