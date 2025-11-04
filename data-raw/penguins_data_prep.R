# data-raw/penguins_data_prep.R

# Load the palmerpenguins data and remove missing values for a clean dataset
penguins_clean <- palmerpenguins::penguins |>
  na.omit()
# Save the cleaned data to the data/ directory of the package
usethis::use_data(penguins_clean, overwrite = TRUE)

