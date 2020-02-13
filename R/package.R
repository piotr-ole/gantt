#' @import shiny
#' @importFrom rhandsontable rhandsontable rHandsontableOutput renderRHandsontable
#' @importFrom dplyr %>% group_by summarise mutate select first last filter lead lag if_else arrange desc
#' @importFrom scales hue_pal
#' @importFrom utils read.csv
#' @importFrom data.table rbindlist
#' @importFrom ggthemes theme_fivethirtyeight
#' @importFrom tibble deframe
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data :=
#' @import ggplot2
globalVariables(c('stage', 'control', 'start', 'end', 'value', 'type', 'point', 'task', 'people'))