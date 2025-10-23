#' Score the RAND 36-Item Short Form Health Survey (SF-36, v1.0)
#' 
#' This function recodes and scores the eight scales of the RAND SF-36 
#' health survey from raw item responses in a dataframe. Items are recoded to 
#' a 0-100 scale and then averaged to create the 8 scales.
#' 
#' The function expects columns for 36 items, starting at column `item1`. 
#' Missing values are...
#' 
#' @param data A dataframe containing the raw SF-36 item responses as 
#'   integers. Columns must be in the standard SF-36 order.
#'   
#' @param item1 Integer; the starting column index for the first survey item. 
#'   Default is 3 (e.g., if columns 1-2 are participant ID and timepoint).
#' 
#' @return A data frame with the original data plus 8 new columns for the 
#'   scores (0-100 scale):
#'   \itemize{
#'     \item `Physical functioning`: Average of 10 items
#'     \item `Role limitations due to physical health`: Average of 4 items
#'     \item `Role limitations due to emotional problems`: Average of 3 items
#'     \item `Energy/fatigue`: Average of 4 items
#'     \item `Emotional well-being`: Average of 5 items
#'     \item `Social functioning`: Average of 2 items
#'     \item `Pain`: Average of 2 items
#'     \item `General health`: Average of 5 items
#'   }
#' 
#' @references 
#' \url{https://www.rand.org/health/surveys/mos/36-item-short-form/scoring.html}
#' @export
#' @importFrom dplyr mutate across recode
#' @importFrom purrr %||%
score_rand36sf <- function(data, item1 = 3) {
  offset <- item1 - 1
  
  recoded_data <- data |> 
    mutate(across(c(1, 2, 20, 22, 34, 36) + offset, ~
                    recode(., "1" = 100, "2" = 75, "3" = 50, "4" = 25, "5" = 0))) |> 
    mutate(across(c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12) + offset, ~
                    recode(., "1" = 0, "2" = 50, "3" = 100))) |> 
    mutate(across(c(13, 14, 15, 16, 17, 18, 19) + offset, ~
                    recode(., "1" = 0, "2" = 100))) |> 
    mutate(across(c(21, 23, 26, 27, 30) + offset, ~
                    recode(., "1" = 100, "2" = 80, "3" = 60, "4" = 40, "5" = 20,
                           "6" = 0))) |> 
    mutate(across(c(24, 25, 28, 29, 31) + offset, ~
                    recode(., "1" = 0, "2" = 20, "3" = 40, "4" = 60,
                           "5" = 80, "6" = 100))) |>
    mutate(across(c(32, 33, 35) + offset, ~
                    recode(., "1" = 0, "2" = 25, "3" = 50, "4" = 75, "5" = 100)))
  
  recoded_data |> mutate(
    `Physical functioning` = rowMeans(recoded_data[,c(3:12)+offset]),
    `Role limitations due to physical health` = rowMeans(recoded_data[,c(13:16)+offset]),
    `Role limitations due to emotional problems` = rowMeans(recoded_data[,c(17:19)+offset]),
    `Energy/fatigue` = rowMeans(recoded_data[,c(23,27,29,31)+offset]),
    `Emotional well-being` = rowMeans(recoded_data[,c(24,25,26,28,30)+offset]),
    `Social functioning` = rowMeans(recoded_data[,c(20,32)+offset]),
    `Pain` = rowMeans(recoded_data[,c(21,22)+offset]),
    `General health` = rowMeans(recoded_data[,c(1,33:36)+offset]))
}