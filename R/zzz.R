.stats_info <- data.table(
  name = c("variable",
            "pct_NA",
            "nb_valid",
            "pct_zero",
            "mean",
            "median",
            "sd",
            "min",
            "max",
            "var",
            "interquartile_range",
            "mode_max",
            "kurtosis",
            "skewness",
            "boxplot",
            "density",
            "nb_modalities"
  ), 
  info = c("variable name",
           "percentage of unknown values",
           "number of known values",
           "percentage of zero",
           "mean value",
           "median value",
           "amount of variation",
           "minimum value",
           "maximum value",
           "variance: expected value of the squared deviation from the mean",
           "first quartile subtracted from the third quartile",
           "value which maximizes the density of the variable",
           "descriptor of the shape of a probability distribution",
           "measure of the asymmetry of the probability distribution",
           "visualization of the distribution",
           "detailed visualization of the distribution",
           "number of different values"
  )
)
