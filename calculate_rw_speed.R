# Function to extract real-world walking speed from its frequency distribution

### Input variable:
# x: gait speed data from all recorded strides, in a long format

## Output variable: outcome; dataframe containing: 
# 1) Extracted real-world walking speed, according to Boekesteijn et al., 2026, Journal of Biomechanics. DOI: 10.1016/j.jbiomech.2026.113581 
# 2) Mean walking speeds of component 1 and 2
# 3) Ashman's D
# 4) number of fitted components


# import mclust library for GMM fitting
library(mclust)

calculate_rw_speed <- function(x) {
  # Check the number of strides required for GMM; here set to 100 strides as a minimum
  if (length(x) < 100) {
    warning("Less than 100 strides available.")
    return(NA)
  }
  
  # Fit GMM (restricted to maximally 2 components based on assumption of bimodality)
  fit <- Mclust(x, G = 1:2, modelNames = "V", verbose = FALSE)
  num_components <- fit$G
  means <- fit$parameters$mean
  sds <- sqrt(fit$parameters$variance$sigmasq)
  proportions <- fit$parameters$pro
  
  # Reorder components such that component 1 is always the slowest
  if (num_components == 2) {
    ord <- order(means)
    means <- means[ord]
    sds <- sds[ord]
    proportions <- proportions[ord]
  }
  
  # fit a density function based on the derived GMM characteristics to extract the mode
  gmm_mode <- function(fit) {
    dens_func <- function(x) {
      sum(
        fit$parameters$pro *
          dnorm(
            x,
            mean = fit$parameters$mean,
            sd = sqrt(fit$parameters$variance$sigmasq)
          )
      )
    }
    # extract mode
    opt <- optimize(
      f = function(x) -dens_func(x),
      interval = range(fit$data)
    )
    opt$minimum
  }
  
  # Calculate Ashman's D (only possible when 2 components are detected)
  # Ashman, K.M., Bird, C.M., Zepf, S.E., 1994. Detecting Bimodality in Astronomical Datasets.
  # The Astronomical Journal 108, 2348.
    ashmans_d <- if (num_components == 2) {
    sqrt(2) * abs(means[1] - means[2]) / sqrt(sds[1]^2 + sds[2]^2)
  } else {
    NA
  }
  
  # Compute real-world walking speed according to proposed method
  rw_speed <- if (num_components == 1) {
    means[1] 
  } else if (ashmans_d > 2) {
    means[2]
  } else {
    gmm_mode(fit)
  }
  
  # write output variables to dataframe
  return(data.frame(
    real_world_speed = rw_speed,
    mean_comp1 = means[1],
    mean_comp2 = ifelse(num_components == 2, means[2], NA),
    ashmans_d = ashmans_d,
    n_components = num_components
  ))
}