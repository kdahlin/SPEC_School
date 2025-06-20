# plsr time i guess
library(pls)

spectra_with_traits_clean <- spectra_with_traits %>%
  filter(!is.na(lyr.1))

### chunk stolen/adapted from erb github repo
# Identify the spectral column names (starting with "lyr")
spectral_cols <- grep("^lyr", names(spectra_with_traits_clean), value = TRUE)

# Combine response and predictors into one data frame
plsr_data <- spectra_with_traits_clean %>%
  select(nitrogenPercent, starts_with("lyr"))

## if you want to drop columns with low variance?
library(caret)

# Identify columns with near-zero variance
nzv <- nearZeroVar(plsr_data[ , -1])

# Drop these columns
plsr_data_filtered <- plsr_data[ , -c(1, nzv + 1)]  # +1 to offset exclusion of response
plsr_data_filtered$nitrogenPercent <- plsr_data$nitrogenPercent  # Add response back

plsr_model <- plsr(nitrogenPercent ~ ., 
                   data = plsr_data_filtered, # if dropping near zero variance change to plsr_data_filtered
                   ncomp = 1,
                   validation = "none", 
                   scale = TRUE)


summary(plsr_model)
