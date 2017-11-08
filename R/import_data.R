library(data.table)
library(ggplot2)
library(readxl)
filenames <- list.files("data")
crime_idx <- grep("crime-statistics", filenames)
crime_files <- filenames[crime_idx]
number_of_files <- length(crime_files)
crime <- vector(mode = "list", length = number_of_files)
for (i in seq_along(crime_files)) {
  f <- crime_files[i]
  crime[[i]] <- setDT(read_excel(file.path("data", f)))
  setnames(crime[[i]], c("date", "suburb", "postcode", "offence_level_1",
                    "offence_level_2", "offence_level_3", "offence_count"))
}
CrimePlot(crime[[1]], "OFFENCES AGAINST PROPERTY", c("ABERFOYLE PARK", "ADELAIDE"))
