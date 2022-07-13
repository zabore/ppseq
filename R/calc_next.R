#' Calculate response probability for the next patient
#'
#' @description This function is meant to be used in the context of a
#' clinical trial with a binary endpoint. For the two-sample case, the total
#' number of events in the standard-of-care arm is y0 and the total number of
#' events in the experimental arm is y1. The function samples from the posterior
#' beta distribution based on the data and the prior beta hyperparameters, and
#' returns the empiric mean and highest posterior density interval for the next
#' patient. The empiric mean represents the probability of the binary outcome
#' occurring in the next patient. 
#' The one-sample case is also available, in which a target p0
#' must be specified..
#'
#' @param y number of events observed so far. Vector of length two c(y0, y1) 
#' for the two-sample case; integer y for the one-sample case.
#' @param n sample size observed so far. Vector of length two c(n0, n1)
#' for the two-sample case; integer n for the one-sample case.
#' @param p0 the target value to compare to in the one-sample case. Set to NULL
#' for the two-sample case.
#' @param direction "greater" (default) if interest is in P(p1 > p0) in the 
#' two-sample case or P(p > p0) in the one-sample case; "less"
#' if interest is in P(p1 < p0) for the two-sample case or P(p < p0) for the
#' one-sample case.
#' @param delta clinically meaningful difference between groups.
#' Typically 0 for the two-sample case. NULL for one-sample case (default).
#' @param prior vector of length two containing hyperparameters of the prior 
#' beta distribution. c(0.5, 0.5) is default, for the Beta(0.5, 0.5) 
#' distribution.
#' @param S number of samples, default is 5000
#' @param interval a value between 0 and 1 indicating the width of the desired
#' interval, default is 0.95
#'
#' @return Returns a tibble with the empiric mean and 
#' highest posterior density interval
#'
#' @examples
#'
#' set.seed(123)
#' 
#' # One-sample case
#' calc_next(
#'   y = 27, 
#'   n = 100, 
#'   p0 = 0.2
#'   )
#'
#' # Two-sample case
#' calc_next(
#'   y = c(14, 23), 
#'   n = c(100, 100), 
#'   p0 = NULL, 
#'   delta = 0
#'   )
#' 
#' @importFrom stats rbeta
#' @importFrom tibble tibble
#' @export
#' 

calc_next <- function(y, n, p0, 
                      direction = "greater", delta = NULL, 
                      prior = c(0.5, 0.5), S = 5000,
                      interval = 0.95) {
  if (length(y) != length(n))
    stop("y and n must be the same length")
  
  if ((is.null(p0) & is.null(delta)) | (!is.null(p0) & !is.null(delta)))
    stop("Exactly one of delta or p0 must be specified for the two-sample and
         one-sample case, respectively")
  
  if (!direction %in% c("greater", "less"))
    stop('direction must be either "greater" or "less"')
  
  if (length(y) == 1 & is.null(p0))
    stop("p0 must be specified for the one-sample case")
  
  if (length(y) == 2 & is.null(delta))
    stop("delta must be specified for the two-sample case")
  
  if (interval <= 0 | interval >= 1)
    stop("interval must be a value between 0 and 1")
  
  if (length(y) == 2) {
    rb0 <- rbeta(S, prior[1] + y[1], prior[2] + n[1] - y[1])
    rb1 <- rbeta(S, prior[1] + y[2], prior[2] + n[2] - y[2])
    
    if (n[1] < N[1]) {
      Y0 <- y[1] + map_dbl(rb0, rbinom, n = 1, size = 1)
    } else {
      Y0 <- rep(y[1], S)
      N[1] <- n[1]
    }
    
    if (n[2] < N[2]) {
      Y1 <- y[2] + map_dbl(rb1, rbinom, n = 1, size = 1)
    } else {
      Y1 <- rep(y[2], S)
      N[2] <- n[2]
    }
    
    post <- map2_dbl(Y0, Y1, ~ calc_posterior(
      y = c(.x, .y), n = N,  direction = direction, p0 = p0, delta = delta,
      prior = prior, S = S
    ))
  } else if (length(y) == 1) {
    rb1 <- rbeta(S, prior[1] + y, prior[2] + n - y)
    
    if (n < N) {
      Y <- y + map_dbl(rb1, rbinom, n = 1, size = 1)
    } else {
      Y <- rep(y, S)
      N <- n
    }
    
    post <- map_dbl(
      Y, 
      calc_posterior,
      n = N,
      direction = direction, 
      p0 = p0,
      delta = delta, 
      prior = prior, 
      S = S
    )
  }
  
  return(mean(post > theta))
}


y <- 14
n <- 50
p0 <- 0.2
direction <- "greater"
delta <- NULL 
prior <- c(0.5, 0.5) 
S <- 5000
interval <- 0.95

library(purrr)
library(ppseq)

rb1 <- rbeta(S, prior[1] + y, prior[2] + n - y)

Y <- y + map_dbl(rb1, rbinom, n = 1, size = 1)

post <- map_dbl(
  Y, 
  calc_posterior,
  n = N,
  direction = direction, 
  p0 = p0,
  delta = delta, 
  prior = prior, 
  S = S
)

# plot shows this is bimodal - makes sense since only two values possible for the counts
df <- as_tibble(post_order)
ggplot(df, aes(x = value)) + 
  geom_density() + 
  geom_bar()

# ordered list of samples
post_order <- sort(post)

# total number of samples
interval_n <- floor(interval * S)

# Get width (in units of data) of all intervals with n_samples samples
int_width <- post_order[interval_n:S] - post_order[1:(S - interval_n + 1)]

min_int <- which.min(int_width)

post_order[min_int]
post_order[min_int + interval_n]
post_order[min_int + interval_n] - post_order[min_int]

df <- as_tibble(post_order)
ggplot(df, aes(x = value)) + 
  geom_density() + 
  geom_bar() + 
  geom_vline(xintercept = post_order[min_int]) + 
  geom_vline(xintercept = post_order[min_int + interval_n])


y <- c(14, 23)
n <- c(100, 100)
p0 <- NULL
delta <- 0

rb0 <- rbeta(S, prior[1] + y[1], prior[2] + n[1] - y[1])
rb1 <- rbeta(S, prior[1] + y[2], prior[2] + n[2] - y[2])

Y0 <- y[1] + map_dbl(rb0, rbinom, n = 1, size = 1)
Y1 <- y[2] + map_dbl(rb1, rbinom, n = 1, size = 1)

post <- map2_dbl(Y0, Y1, ~ calc_posterior(
  y = c(.x, .y), n = N,  direction = direction, p0 = p0, delta = delta,
  prior = prior, S = S
))


















calc_next <- function(y, n, p0, direction = "greater", delta = NULL, 
                      prior = c(0.5, 0.5), S = 5000,
                      interval = 0.95) {
  if (length(y) != length(n))
    stop("y and n must be the same length")
  
  if ((is.null(p0) & is.null(delta)) | (!is.null(p0) & !is.null(delta)))
    stop("Exactly one of delta or p0 must be specified for the two-sample and
         one-sample case, respectively")
  
  if (!direction %in% c("greater", "less"))
    stop('direction must be either "greater" or "less"')
  
  if (length(y) == 1 & is.null(p0))
    stop("p0 must be specified for the one-sample case")
  
  if (length(y) == 2 & is.null(delta))
    stop("delta must be specified for the two-sample case")
  
  if (interval <= 0 | interval >= 1)
    stop("interval must be a value between 0 and 1")
  
  if (length(y) == 2) {
    rb0 <- rbeta(S, prior[1] + y[1], prior[2] + n[1] - y[1])
    rb1 <- rbeta(S, prior[1] + y[2], prior[2] + n[2] - y[2])
    
    hdi0 <- hdi(rb0, credMass = interval)
    hdi1 <- hdi(rb1, credMass = interval)
    
    out <- tibble(
      arm = c(0, 1),
      mean = c(mean(rb0), mean(rb1)),
      lower = c(hdi0["lower"], hdi1["lower"]),
      upper = c(hdi0["upper"], hdi1["upper"])
    )
  } else if (length(y) == 1) {
    rb1 <- rbeta(S, prior[1] + y, prior[2] + n - y)
    
    hdi1 <- hdi(rb1, credMass = interval)
    
    out <- tibble(
      arm = 1,
      mean = mean(rb1),
      lower = hdi1["lower"],
      upper = hdi1["upper"]
    )
  }
  
  return(out)
}