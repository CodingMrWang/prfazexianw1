## QUESTION ----

#' plot total offence according to specific area and type
#'
#' \code{CrimePlot} < it will accept any of the offence_descriptions found in Offence Level 3
#' and will accept a 2-element vector of suburbs, then plot the total offence of the specificed
#' description in the two suburbs in each month>
#' @param crime_data A data.table object with the following columns:
#'     "date" (POSIXct), "suburb" (chr), "postcode" (chr), "offence_level_1" (chr),
#'     "offence_level_2" (chr), "offence_level_3" (chr), "offence_count" (num).
#' @param offence_description A character string of one description found in Offence level 3.
#' @param suburbs A two-element character vector. Each element is the name (UPPERCASE)
#'     of an SA suburb.
#' @export
#' @return  A ggplot object showing the correlation in offence count between the two input suburbs.
#' @examples
#' <one or two examples showing how to use the function>
#' crime_data = crime[[1]]
#' offence_description = "OFFENCES AGAINST PROPERTY"
#' suburbs = c("ABERFOYLE PARK", "ADELAIDE")
#' CrimePlot(crime_data, offence_description, suburbs)
 CrimePlot <- function(crime_data, offence_description, suburbs) {
  require(data.table)
  require(ggplot2)

  # Error catching
  print(9999)
  if (length(suburbs) != 2) {
    stop("Please enter two suburbs")
  }

  print(8888888)
  expected_colnames <- c("date", "suburb", "postcode", "offence_level_1", "offence_level_2",
                         "offence_level_3", "offence_count")
  print(0000000)
  if (!all.equal(colnames(crime_data), expected_colnames)) {
    stop(paste("Input table columns need to match: ",
               paste(expected_colnames, collapse = ", ")))
  }
  print(1111111111)
  # Check that the input suburbs and offence description exist in crime_data
  if (any(!suburbs %in% crime_data$suburb) |
      !offence_description %in% crime_data$offence_level_3) {
    stop("input suburbs or offence descriptoin doesn't exist in crime_data")
  }
 print(5555555555)
  # Make a data table for plotting using data.table transformations
  # You will need to filter, summarise and group by
  # Expect cols: "date", "suburb", "total_offence_count"
  offence_count = crime_data$offence_count
  #(crime_data$suburb %in% suburbs & crime_data$offence_level_3 %in% offence_description)
  plot_data <- crime_data[,
                          list("total_offence_count" = sum(offence_count)),by = list(month(date), suburb)]
  #filter the unique tuples
  print(1)
  plot_data <- unique(plot_data)
  print(2)
  # These lines will transform the plot_data structure to allow us to plot
  # correlations. Try them out
  plot_data[, suburb := plyr::mapvalues(suburb, suburbs, c("x", "y"))]
  plot_data <- dcast(plot_data, month ~ suburb, fun = sum,
                     fill = 0, value.var = "total_offence_count")
  print(3)
  suburb1 = rbind(plot_data$month,plot_data$x,suburbs[1])
  print(4)
  suburb2 = rbind(plot_data$month,plot_data$y,suburbs[2])
  print(5)
  plot_data = data.frame(t(cbind(suburb1, suburb2)))
  colnames(plot_data) = c("month", "total_offence_count", "suburb")
  print(6)
  plot_data$month = factor(plot_data$month ,level = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))
  plot_data$total_offence_count = as.numeric(plot_data$total_offence_count)
  print(7)
  # Generate the plot

  ggplot(plot_data, aes(x = month, y = total_offence_count, color = factor(suburb)))+
       geom_count() +
    labs(title = "Offence count summary",
      x = "Month",
         y = "Total offence count in the two suburbs");
}
