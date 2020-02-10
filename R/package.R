#' @import shiny
#' @importFrom rhandsontable rhandsontable rHandsontableOutput renderRHandsontable
#' @importFrom dplyr %>% group_by summarise mutate select first last
#' @importFrom scales hue_pal
#' @importFrom utils read.csv
#' @importFrom data.table rbindlist
#' @import ggplot2
globalVariables(c('stage', 'control', 'start', 'end', 'value', 'type', 'point', 'id'))