#' @name read_task
#' @title Read task
#' 
#' @description Creates task dataframe from csv file
#' @param task_file \code{string} path to csv file where tasks are specified
#' @param date_format \code{string} representing format of dates in task_file
#' @param delimiter \code{char} symbol which separates values in task_file
#' @details CSV file requires column described below. The order rows are given in the file,
#' will be also present in data.frame.
#' \itemize{
#' \item{\code{start}}{  date when task starts}
#' \item{\code{end}}{  date when task ends}
#' \item{\code{task}}{ name of a task}
#' \item{\code{type}}{ user specified type of task e.x. 'critical', 'regular'}
#' \item{\code{stage}}{ tasks can be divided by stages for example: 'Planing', 'Development', 'Tests'}
#' \item{\code{control}}{  each task can be assigned with control value, tasks with same control value
#' dependent on each other.}
#' }
#'@return \code{data.frame} with same columns as in csv file with value column added. Value column gives
#'unique value to each task.
#'@export
read_task <- function(task_file = 'task.csv', date_format = '%d/%m/%y', delimiter = ';') {
    task_df <- read.csv(file = task_file, sep = delimiter, header = TRUE, stringsAsFactors = TRUE)
    task_df$start <- as.Date(task_df$start, date_format)
    task_df$end <- as.Date(task_df$end, date_format)
    task_df$type <-  as.factor(task_df$type)
    task_df$stage <-  as.factor(task_df$stage)
    task_df$value <- nrow(task_df):1
    task_df
}

#' @name mutate_task
#' @title Mutate task
#' 
#' @description Transforms task data frame into ggplot friendly format
#' @param task \code{data.frame} with tasks specified
#' @details Transforms task \code{data.frame} in a way that each row from original \code{data.frame}
#' is doubled. Columns 'start' and 'end' are dropped and new column 'point' is added. Value of point
#' are missing value of 'start', 'end' columns.
#' @return Transformed \code{data.frame}
#' @seealso \code{\link[gantt]{read_task}}
mutate_task <- function(task) {
    df <- data.frame(point = character(), task = character())
    for (i in seq(nrow(task))) {
        df <- rbind(df, data.frame(point = task[i, 'start'], task = task[i, 'task'], type = task[i, 'type'],
                                   value = task[i, 'value'], stage = task[i, 'stage'], control = task[i, 'control']))
        df <- rbind(df, data.frame(point = task[i, 'end'], task = task[i, 'task'], type = task[i, 'type'],
                                   value = task[i, 'value'], stage = task[i, 'stage'], control = task[i, 'control']))
    }
    df
}

#' @name middle_date
#' @title Get middle date
#' 
#' @description Calculates middle date between start and end.
#' @param start \code{date} 
#' @param end \code{date}
#' @return \code{date} in between start and end dates
middle_date <- function(start, end) {
    start + (end - start) / 2
}

#' @name create_summaries
#' @title Create summary data frame
#' 
#' @description Creates data.frame which describes stages.
#' @param task \code{data.frame} 
#' @details  Each row has columns: 'stage', 'start', 'end', 'value' and 'type'. 
#' Type is always set as 'summary', the value is calculated as maximum value of task 
#' in a stage plus 0.3.
#' @return \code{data.frame} with stages summary
#' @seealso \code{\link[gantt]{read_task}}
create_summaries <- function(task) {
    tab <- task %>%
        group_by(stage) %>%
        summarise(start = min(start), end = max(end), value = max(value) + 0.3, type = 'summary')
    as.data.frame(tab[order(tab$value, decreasing = TRUE), ])
}

#' @name create_stage_task_list
#' @title Create list of tasks in each stages
#' @param task \code{data.frame} 
#' @details Creates a list in which each element is a \code{character} vector containing names of tasks
#' corresponding to every stage.
#' @return \code{list} of task with names of tasks, named by stages 
#' @seealso \code{\link[gantt]{read_task}}
create_stage_task_list <- function(task) {
    split(task$task, task$stage)
}

#' @name create_color_dict
#' @title stage color palette
#' @param task \code{data.frame}
#' @details Makes a named list of color codes, where names corresponds to unique stages in \code{task} 
#' @return \code{list} of task with names of stages 
#' @seealso \code{\link[gantt]{read_task}}
create_color_dict <- function(task) {
    n <- length(unique(task$stage))
    split(hue_pal()(n), unique(task$stage))
}

# zmienic nazwe funkcji na assign colors_to_tasks

#' @name assign_colors_to_stages
#' @title Assign color to tasks
#' @param task \code{data.frame}
#' @param color_by_stages \code{logical} if \code{TRUE} then color assigned to each task is same for same stages
#' , if \code{FALSE} then black color is assigned to every task.
#' @details Makes a named list of color codes, where names corresponds to unique stages in \code{task} 
#' @return \code{list} of task with names of stages 
#' @seealso \code{\link[gantt]{read_task}}
assign_colors_to_stages <- function(task, color_by_stages) {
    dict <- create_color_dict(task)
    if (color_by_stages == FALSE){
        return(rep('#000000', nrow(task)))
    }
    as.character(sapply(task$stage, function(x, dict) { dict[[x]] } ,dict))
}


#' @name adjust_path_data
#' @title Recalculate control value
#' @param path_data \code{data.frame}
#' 
adjust_path_data <- function(path_data) {
    dict <- path_data %>% group_by(stage) %>% summarise(max = max(control))
    stages <- as.data.frame(dict[, 'stage'])[, 'stage']
    maxes <- cumsum(as.numeric(as.data.frame(dict[, 'max'])[, 1]))
    for (i in 2:length(stages)) {
        path_data[path_data$stage == stages[i], ]$control <- as.numeric(path_data[path_data$stage == stages[i], ]$control) + maxes[i-1]
    }
    path_data
}

#' @name critical_path
#' @title Calculate paths for Gantt chart
#' @param task \code{data.frame}
#' 
critical_path <- function(task) {
    plot_task <- mutate_task(task)
    new_df <- plot_task[0, ]
    shadow <- plot_task[1, ]
    current <- plot_task[2, ]
    new_df <- rbind(new_df, shadow)
    for(i in 3:nrow(plot_task)) {
        if (shadow['task'] != current['task']) {
            if (shadow['point'] < current['point']) {
                r0 <- data.frame(point = shadow['point'], task = current['task'], 
                                 type = 'tmp',  value = current['value'])
                if (shadow['stage'] == current['stage']) {
                    r0 <- cbind(r0, stage = current['stage'])
                }
                else if (shadow['stage'] != current['stage']) {
                    r0 <- cbind(r0, stage = paste(c(char(shadow[1, 'stage']), '|', char(current[1, 'stage'])), collapse = ''))
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
                if (shadow['stage'] == current['stage']) {
                    r1 <- cbind(r1, stage = current['stage'])
                    r2 <- cbind(r2, stage = current['stage'])
                }
                else if (shadow['stage'] != current['stage']) {
                    r1 <- cbind(r1, stage = paste(c(char(shadow[1, 'stage']), '|', char(current[1, 'stage'])), collapse = ''))
                    r2 <- cbind(r2, stage = paste(c(char(shadow[1, 'stage']), '|', char(current[1, 'stage'])), collapse = ''))
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


#' @name char
#' @title as.character
#' @param x \code{object}
char <- function(x) {
    as.character(x)
}

#' @name create_gantt_config
#' @title Create configuration list
#' @param task_bar_color decides which column from \code{type} and
#' @param stage_path \code{logical}, whether path will be drawn between tasks with same stage
#' @param control_path \code{logical}, whether path will be drawn between tasks with same control value
#' @param stage_arrows \code{logical}, whether arrows denoting stages have to be drawn
#' @param x_axis_label_position \code{character} one of following: \code{top}, \code{bottom}
#' @param y_axis_label_position \code{character} one of following: \code{left}, \code{right}
#' @param y_axis_color_by_stages \code{integer}
#' @param x_axis_text_size \code{integer}
#' @param y_axis_text_size \code{integer}
#' @param task_bar_width \code{integer}
#' @param plot_title \code{character}
#' @param plot_title_size \code{integer}
#' @details 
#' If \code{stage_path = TRUE} and \code{control_path = TRUE} then path is drawn within stages and within
#' control groups. \\ If \code{stage_path = FALSE} and \code{control_path = FALSE} then no path is drawn
#' @export
create_gantt_config <- function(x_axis_text_size = 10, y_axis_text_size = 10, 
                                x_axis_label_position = 'bottom', y_axis_label_position = 'left',
                                task_bar_color = 'type', task_bar_width = 5,
                                plot_title = 'Gantt chart', plot_title_size = 15,
                                stage_path = TRUE, control_path = TRUE, stage_arrows = TRUE,
                                y_axis_color_by_stages = TRUE) {
    gantt_config <-  as.list(environment(), all=TRUE)
    gantt_config
}

#' @name gantt
#' @title Draws Gantt chart
#' @param task \code{data.frame} with tasks specified, created with \code{read_task} function
#' @param conf \code{list} configuration list with styling parameters, created with 
#' \code{create_gantt_config}
#' @export
#' @references  \code{\link[gantt]{read_task}}
#' @references \code{\link[gantt]{create_gantt_config}}
gantt <- function(task, conf) {
    critical <- critical_path(task)
    col = assign_colors_to_stages(task, conf$y_axis_color_by_stages)
    summaries <- create_summaries(task)
    plot_task <- mutate_task(task)
    p <- ggplot(data = plot_task, aes(x = point, y = task))
    if (conf$stage_path == TRUE && conf$control_path == FALSE)
    {
        path_data <- critical[ifelse(critical$stage %in% unique(task$stage), TRUE, FALSE), ]
        p <- p + geom_path(aes(x = point, y = value, group = stage), color = '#252525', 
                           data = path_data)
    }
    if (conf$stage_path == FALSE && conf$control_path == TRUE)
    {
        path_data <- critical[ifelse(critical$control %in% unique(task$control), TRUE, FALSE), ]
        p <- p + geom_path(aes(x = point, y = value, group = control), color = '#252525', 
                           data = path_data)
    }
    if (conf$stage_path == TRUE && conf$control_path == TRUE)
    {
        path_data <- critical[ifelse(critical$stage %in% unique(task$stage), TRUE, FALSE), ]
        path_data <- critical[ifelse(critical$control %in% unique(task$control), TRUE, FALSE), ]
        path_data <- adjust_path_data(path_data)
        p <- p + geom_path(aes(x = point, y = value, group = control), color = '#252525', 
                           data = path_data)
    }
    if (conf$task_bar_color == 'type') {
        p <- p + geom_line(aes(color = type), size = conf$task_bar_width, data = plot_task) 
    } else if (conf$task_bar_color == 'stage') {
        p <- p + geom_line(aes(color = stage), size = conf$task_bar_width, data = plot_task)
    }
    if (conf$stage_arrows == TRUE) {
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
        geom_text(aes(x = middle_date(start, end), y = value + 0.2, label = stage) , data = summaries, fontface = 'bold') +
        scale_y_discrete(limits = rev(task$task), position = conf$y_axis_label_position) + 
        scale_x_date(date_breaks = "1 month", date_labels = "%b %d", date_minor_breaks = "1 day",
                     position = conf$x_axis_label_position)
    p
}
