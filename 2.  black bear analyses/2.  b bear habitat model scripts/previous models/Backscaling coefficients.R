
# Back scaling the coefficients:
to_scale <- 1:10
using_scale <- scale(to_scale, center = TRUE, scale = TRUE)
by_hand <- (to_scale - mean(to_scale))/sd(to_scale)
identical(as.numeric(using_scale), by_hand)

get_real <- function(coef, scaled_covariate){
  
  # collect mean and standard deviation from scaled covariate
  mean_sd <- unlist(attributes(scaled_covariate)[-1])
  
  # reverse the z-transformation
  answer <- (coef * mean_sd[2]) + mean_sd[1]
  
  # this value will have a name, remove it
  names(answer) <- NULL
  
  # return unscaled coef
  return(answer)
}

# Loosen et al:
get_real(-0.1, using_scale) # 5.1972
get_real(-0.75, using_scale) # 3.2293
get_real(0.2, using_scale) # 6.1055
get_real(0.1, using_scale) # 5.803
get_real(0, using_scale) # 5.1972
get_real(-0.25, using_scale) # 4.743
get_real(-1.2, using_scale) # 1.8668

# Fleishman et al., 2017:
get_real(0.72, using_scale) # 7.6799
get_real(0.26, using_scale) # 6.287
get_real(0.23, using_scale) # 6.196
get_real(-0.03, using_scale) # 5.409
get_real(-0.1, using_scale) # 5.1972
get_real(-0.18, using_scale) # 4.955
get_real(-0.06, using_scale) # 5.318
get_real(0, using_scale) # 5.5
get_real(-0.09, using_scale) # 5.228

get_real(0.3, using_scale) # 6.408
get_real(0.47, using_scale) # 6.9229
get_real(0.48, using_scale) # 6.9533


