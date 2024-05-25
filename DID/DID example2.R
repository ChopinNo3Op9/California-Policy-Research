# Install did package if not already installed
# install.packages("did")

# Load the necessary packages
library(did)
library(dplyr)

# Set seed for reproducibility
set.seed(1814)

# Define simulation parameters (example parameters)
sp <- list(
  n = 2000,               # number of units
  t = 4,                  # number of time periods
  te.e = 1:4              # dynamic effects
)

# Function to simulate dataset with a never-treated group
build_sim_dataset <- function(sp) {
  n <- sp$n
  t <- sp$t
  te.e <- sp$te.e
  
  # Create a sample dataset
  df <- expand.grid(id = 1:n, period = 1:t)
  df <- df %>%
    mutate(G = sample(1:5, n * t, replace = TRUE), # include group 5 as never-treated
           X = rnorm(n * t),
           cluster = sample(1:100, n * t, replace = TRUE),
           treat = ifelse(period >= G & G < 5, 1, 0), # group 5 will never be treated
           Y = rnorm(n * t) + treat * rep(te.e, each = n))
  
  # Drop units treated in the first period (if any)
  df <- df %>% filter(!(G == 1 & treat == 1))
  
  return(df)
}

# Generate the dataset
dta <- build_sim_dataset(sp)

# Check the number of observations after dropping "always-treated" units
nrow(dta)
#> [1] 15916

# Display the first few rows of the dataset
head(dta)

# Estimate group-time average treatment effects using att_gt method
example_attgt <- att_gt(
  yname = "Y",
  tname = "period",
  idname = "id",
  gname = "G",
  xformla = ~X,
  data = dta
)

# Summarize the results
summary(example_attgt)
