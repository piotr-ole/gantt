library(ggplot2)
library(scales)
library(dplyr)

#' @name read_task
#' @title Read task
#' 
#' @description Creates task dataframe from csv file
#' @param task_file \code{string} path to csv file where tasks are specified
#' @param date_format \code{string} representing format of dates in task_file
#' @param delimiter \code{char} symbol which separates values in task_file
#'
#'
read_task <- function(task_file = 'task.csv', date_format = '%d/%m/%y', delimiter = ';') {
    task_df <- read.csv(file = task_file, sep = delimiter, header = TRUE, stringsAsFactors = FALSE)
    task_df$start <- as.Date(task_df$start, date_format)
    task_df$end <- as.Date(task_df$end, date_format)
    task_df$type <-  as.factor(task_df$type)
    task_df$value <- nrow(task_df):1
    task_df
}

#' @name mutate_task
#' @title Mutate task
#' 
#' @description Transforms task data frame into ggplot friendly format
#' @param task \code{data.frame} with tasks specified
#'
mutate_task <- function(task) { # to potencjalnie mozna ladniej napisac
    df <- data.frame(point = character(), task = character())
    for (i in seq(nrow(task))) {
        df <- rbind(df, data.frame(point = task[i, 'start'], task = task[i, 'task'], type = task[i, 'type'],
                                   value = task[i, 'value'], faze = task[i, 'faze'], control = task[i, 'control']))
        df <- rbind(df, data.frame(point = task[i, 'end'], task = task[i, 'task'], type = task[i, 'type'],
                                   value = task[i, 'value'], faze = task[i, 'faze'], control = task[i, 'control']))
    }
    df
}

#' @name middle_date
#' @title Get middle date
#' 
#' @description Calculates middle date between start and end.
#' @param start \code{date} 
#' @param end \code{date}
#'
middle_date <- function(start, end) {
    as.Date(start) + (as.Date(end) - as.Date(start)) / 2
}


create_summaries <- function(task) {
    tab <- task %>%
        group_by(faze) %>%
        summarise(start = min(start), end = max(end), value = max(value) + 0.3, type = 'summary')
    as.data.frame(tab[order(tab$value, decreasing = TRUE), ])
}

create_faze_task_list <- function(task) {
    split(task$task, task$faze)
}

create_color_dict <- function(task) {
    n <- length(unique(task$faze))
    split(hue_pal()(n), unique(task$faze))
}

assign_colors_to_fazes <- function(task, y_axis_color_by_fazes) {
    dict <- create_color_dict(task)
    if (y_axis_color_by_fazes == FALSE){
        return(rep('#000000', nrow(task)))
    }
    as.character(sapply(task$faze, function(x, dict) { dict[[x]] } ,dict))
}

adjust_path_data <- function(path_data) {
    dict <- path_data %>% group_by(faze) %>% summarise(max = max(control))
    fazes <- as.data.frame(dict[, 'faze'])[, 'faze']
    maxes <- cumsum(as.numeric(as.data.frame(dict[, 'max'])[, 1]))
    for (i in 2:length(fazes)) {
        path_data[path_data$faze == fazes[i], ]$control <- as.numeric(path_data[path_data$faze == fazes[i], ]$control) + maxes[i-1]
    }
    path_data
}


critical_path <- function(plot_task) {
    new_df <- plot_task[0, ]
    shadow <- plot_task[1, ]
    current <- plot_task[2, ]
    new_df <- rbind(new_df, shadow)
    for(i in 3:nrow(plot_task)) {
        if (shadow['task'] != current['task']) {
            if (shadow['point'] < current['point']) {
                r0 <- data.frame(point = shadow['point'], task = current['task'], 
                                 type = 'tmp',  value = current['value'])
                if (shadow['faze'] == current['faze']) {
                    r0 <- cbind(r0, faze = current['faze'])
                }
                else if (shadow['faze'] != current['faze']) {
                    r0 <- cbind(r0, faze = paste(c(char(shadow[1, 'faze']), '|', char(current[1, 'faze'])), collapse = ''))
                }
                if (shadow['control'] == current['control']) {
                    r0 <- cbind(r0, control = current['control'])
                }
                else if (shadow['control'] != current['control']) {
                    r0 <- cbind(r0, control = paste(c(char(shadow[1, 'control']), '|', char(current[1, 'control'])), collapse = ''))
                }
                new_df <- rbind(new_df, r0)
            } else if (shadow['point'] > current['point']) {
                r1 <- data.frame(point = shadow['point'], task = shadow['task'], # task dont really matters
                                 type = 'tmp', value = shadow['value'] - 0.5)
                r2 <- data.frame(point = current['point'], task = shadow['task'],
                                 type = 'tmp', value = shadow['value'] - 0.5)
                if (shadow['faze'] == current['faze']) {
                    r1 <- cbind(r1, faze = current['faze'])
                    r2 <- cbind(r2, faze = current['faze'])
                }
                else if (shadow['faze'] != current['faze']) {
                    r1 <- cbind(r1, faze = paste(c(char(shadow[1, 'faze']), '|', char(current[1, 'faze'])), collapse = ''))
                    r2 <- cbind(r2, faze = paste(c(char(shadow[1, 'faze']), '|', char(current[1, 'faze'])), collapse = ''))
                }
                if (shadow['control'] == current['control']) {
                    r1 <- cbind(r1, control = current['control'])
                    r2 <- cbind(r2, control = current['control'])
                }
                else if (shadow['control'] != current['control']) {
                    r1 <- cbind(r1, control = paste(c(char(shadow[1, 'control']), '|', char(current[1, 'control'])), collapse = ''))
                    r2 <- cbind(r2, control = paste(c(char(shadow[1, 'control']), '|', char(current[1, 'control'])), collapse = ''))
                }
                new_df <- rbind(new_df, r1)
                new_df <- rbind(new_df, r2)
            }
        }
        new_df <- rbind(new_df, current)
        shadow <- current
        current <- plot_task[i, ]
    }
    new_df
}

char <- function(x) {
    as.character(x)
}

#' @export
create_gantt_config <- function(x_axis_text_size = 10, y_axis_text_size = 10, 
                                x_axis_label_position = 'bottom', y_axis_label_position = 'left',
                                task_bar_color = 'type', task_bar_width = 5,
                                plot_title = 'Gantt chart', plot_title_size = 15,
                                faze_path = TRUE, control_path = TRUE, faze_arrows = TRUE,
                                y_axis_color_by_fazes = TRUE) {
    gantt_config <-  as.list(environment(), all=TRUE)
    gantt_config
}

#' @export
create_gantt_task <- function(task_file = 'big_task.csv', delimiter = ';', date_format = '%d/%m/%y') {
    read_task(task_file, date_format = date_format, delimiter = delimiter)
}

#' @export
gantt <- function(task, conf) {
    plot_task <- mutate_task(task)
    critical <- critical_path(plot_task)
    col = assign_colors_to_fazes(task, conf$y_axis_color_by_fazes)
    summaries <- create_summaries(task)
    p <- ggplot(data = plot_task, aes(x = point, y = task))
    if (conf$faze_path == TRUE && conf$control_path == FALSE)
    {
        path_data <- critical[ifelse(critical$faze %in% unique(task$faze), TRUE, FALSE), ]
        p <- p + geom_path(aes(x = point, y = value, group = faze), color = '#252525', 
                           data = path_data)
    }
    if (conf$faze_path == FALSE && conf$control_path == TRUE)
    {
        path_data <- critical[ifelse(critical$control %in% unique(task$control), TRUE, FALSE), ]
        p <- p + geom_path(aes(x = point, y = value, group = control), color = '#252525', 
                           data = path_data)
    }
    if (conf$faze_path == TRUE && conf$control_path == TRUE)
    {
        path_data <- critical[ifelse(critical$faze %in% unique(task$faze), TRUE, FALSE), ]
        path_data <- critical[ifelse(critical$control %in% unique(task$control), TRUE, FALSE), ]
        path_data <- adjust_path_data(path_data)
        p <- p + geom_path(aes(x = point, y = value, group = control), color = '#252525', 
                           data = path_data)
    }
    if (conf$task_bar_color == 'type') {
        p <- p + geom_line(aes(color = type), size = conf$task_bar_width, data = plot_task) 
    } else if (conf$task_bar_color == 'faze') {
        p <- p + geom_line(aes(color = faze), size = conf$task_bar_width, data = plot_task)
    }
    if (conf$faze_arrows == TRUE) {
    p <- p + geom_segment(aes(x = start, y = value, xend = end, yend = value), data = summaries, 
                          arrow = arrow(length = unit(0.2,'cm'), ends = 'both', type = 'closed'), 
                          lineend = 'butt', size = 2) 
    }
    p <- p +
        ggtitle(conf$plot_title) + 
        theme(plot.title = element_text(hjust = 0.5, size = conf$plot_title_size, face = 'bold'), 
              axis.text.y = element_text(size = conf$y_axis_text_size, color = rev(col), face ='bold'),
              axis.text.x = element_text(size = conf$x_axis_text_size),
              axis.title = element_blank()) +
        geom_text(aes(x = middle_date(start, end), y = value + 0.2, label = faze) , data = summaries, fontface = 'bold') +
        scale_y_discrete(limits = rev(task$task), position = conf$y_axis_label_position) + 
        scale_x_date(date_breaks = "1 week", date_labels = "%b %d", date_minor_breaks = "1 day",
                     position = conf$x_axis_label_position)
    p
}
