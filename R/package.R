#' @import shiny
#' @importFrom rhandsontable rhandsontable rHandsontableOutput renderRHandsontable
#' @importFrom dplyr %>% group_by summarise 
#' @importFrom scales hue_pal
#' @importFrom utils read.csv
#' @importFrom data.table rbindlist
#' @import ggplot2
globalVariables(c('faze', 'control', 'start', 'end', 'value', 'type', 'point'))