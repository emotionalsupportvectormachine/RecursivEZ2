
library(EZ2)
library(readxl)
library(writexl)
library(purrr)

# Starting parameters
magic_numbers <- c(v=.4159,z=.0634,a=.1268,Ter=.226)

# Read the excel file.
input_file <- as.data.frame(read_excel("/Users/cloud/Desktop/EZ2 Thesis Input.xlsx"))

# Rename some columns since R doesn't like anything other than regular latin characters.
colnames(input_file)[1] <- "SN"
colnames(input_file)[2] <- "Mrt_go"
colnames(input_file)[3] <- "Vrt_go"

# Modified EZ2 from https://rdrr.io/rforge/EZ2/src/R/EZ2batch.R
# Omits some unused params and implements the fit$value condition.
apply_magic <- function(pstart, data, ObsValPair, ...) {
  mdl = c(ObsValPair, ...)
  result <- t(apply(data, 1, function(row) {
    attach(as.list(row), warn.conflicts = FALSE)

    # Do the EZ2 model fitting.
    fit <- EZ2(pstart, mdl)

    # Fit the model again using the found parameters
    # if the value is too high.
    # Stops after 100 tries to prevent infinite loop.
    for (i in 1:100) {
      if (fit$value > .00000001) {
        fit <- EZ2(fit$par, mdl)
      } else {
        break
      }
    }

    # Add the participant number to the results.
    fit <- unlist(fit)
    prepend(fit, SN)
  }))

  # Add the column name
  colnames(result)[1] <- "SN"

  # Only return the relevant columns as a dataframe.
  result <- as.data.frame(subset(result, select=c("SN", "par.v", "par.z", "par.a", "par.Ter", "value")))
  result
}


results <- apply_magic(magic_numbers,
                       input_file,
                       Mrt_go ~ EZ2.mrt(v,z,a,Ter=Ter),
                       Vrt_go ~ EZ2.vrt(v,z,a),
                       Pe_go ~ EZ2.pe(v,z,a),
                       Pe_nogo ~ EZ2.pe(v,a-z,a))

# Write the results back to an excel file.
write_xlsx(results, "/Users/cloud/Desktop/EZ2 Thesis Output.xlsx")
