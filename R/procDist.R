#' Extract the statistics from the result given by findNNs
#' @param distResult A distance result file from findNNs
#' @param thred a threshold distance as cut-off
#' @return The statistics extracted from the distace result file
#' @export procDist
#' @examples
#' Cell.X.Position=sample(1:1000, 20, replace=TRUE)
#' Cell.Y.Position=sample(1:1000, 20, replace=TRUE)
#' distance = runif(20,min=0,max=100)
#' distResult = data.frame (Cell.X.Position,Cell.Y.Position,distance)
#' thred=30
#' procDist(distResult,thred)



procDist = function(distResult, thred=30 ) {
  duration = distResult$distance
  result = data.frame(
    Percentage = sum(duration<thred) * 100 / length(duration),
    Mean = mean(duration),
    Std.Dev = sd(duration),
    Median = median(duration)
  )
  return(result)
}