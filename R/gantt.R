#' @name read_task
#' @title Read task
#' 
#' @description Creates task data.frame from csv file
#' @param task_file \code{string} path to csv file where tasks are specified
#' @param date_format \code{string} representing format of dates in task_file
#' @param delimiter \code{char} symbol which separates values in task_file
#' @details CSV file requires column described below. Each row represents one task, order given in the file,
#' will be also present in data.frame.
#' \itemize{
#' \item{\code{start}}{  date when task starts}
#' \item{\code{end}}{  date when task ends}
#' \item{\code{task}}{ name of a task}
#' \item{\code{type}}{ user specified type of task e.x. 'critical', 'regular', there is a special type 'milestone',
#' which should be specified for tasks with same start and end values.}
#' \item{\code{stage}}{ tasks can be divided in stages which are more general elements of the project}
#' \item{\code{control}}{ each task can be assigned with control value, tasks with same control value are considered to
#' be dependent. In other words, it means that one task can start after the other is finished}
#' \item{\code{people}}{ come separated people names which are assigned to perform a task}
#' }
#' @return \code{data.frame} with same columns as in csv file with value column added. Value column gives
#' unique value to each task.
#' @export
read_task <- function(task_file = 'task.csv', date_format = '%d/%m/%y', delimiter = ';') {
    task_df <- read.csv(file = task_file, sep = delimiter, header = TRUE, stringsAsFactors = TRUE)
    task_df$start <- as.Date(task_df$start, date_format)
    task_df$end <- as.Date(task_df$end, date_format)
    task_df$value <- nrow(task_df):1
    task_df
}

#' @name mutate_task
#' @title Mutate task
#' 
#' @description Transforms task data.frame into ggplot friendly format
#' @param task \code{data.frame} with tasks specified
#' @details Transforms task \code{data.frame} in a way that each row from original \code{data.frame}
#' is expanded to two rows, with new column 'point' having values from 'start' and 'end' columns.  
#' @return Transformed \code{data.frame}
#' @seealso \code{\link[gantt]{read_task}}
mutate_task <- function(task) {
    pivot_longer(task, cols = c('start', 'end'), values_to = 'point') %>% 
        select(-c(!!'name', people)) %>% 
        select(point, task, type, value, stage, control) %>% 
        as.data.frame
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
#' @param cascade_summaries \code{string} decides how the staging areas should be created. If 'None', then
#' start and end of each stage are set as a start and end values of respectively first and last task within stage.
#' If 'on_finish' is provided, then each stage starts while last task of previous stage ends and lasts to the end 
#' of the last task within stage. On the other hand 'on_begin' option makes stage starting point with 
#' start of the first task in a stage and ending point with start of first task in the next stage.
#' @details  Each row has columns: 'stage', 'start', 'end', 'value' and 'type'. 
#' Type is always set as 'summary', the value is calculated as maximum value of task 
#' in a stage plus 0.35.
#' @return \code{data.frame} with stages summary
#' @seealso \code{\link[gantt]{read_task}}
create_summaries <- function(task, cascade_summaries = 'None') {
    tab <- task %>%
        group_by(stage) %>%
        summarise(start = min(start), end = max(end), value = max(value) + 0.35, type = 'summary') %>%
        arrange(desc(value)) %>%
        as.data.frame
    if (cascade_summaries == 'on_finish') { 
        tab <- tab %>% mutate(!!'prev' := lag(end, 1)) %>% 
                        mutate(start = if_else(!is.na(.data$prev) & start > .data$prev, .data$prev, start)) %>%
                        select(-!!'prev')
    } else if (cascade_summaries == 'on_begin') {
        tab <- tab %>% mutate(!!'next_' := lead(start, 1)) %>% 
            mutate(end = if_else(!is.na(.data$next_) & .data$next_ > end, .data$next_, end)) %>%
            select(-!!'next_')
    }
    tab
}

#' @name create_stage_task_list
#' @title Create list of tasks in each stages
#' @param task \code{data.frame} 
#' @details Creates a list in which each element is a \code{character} vector containing names of tasks
#' corresponding to every stage.
#' @return \code{list} of task, named by stages 
#' @seealso \code{\link[gantt]{read_task}}
create_stage_task_list <- function(task) {
    split(task$task, task$stage)
}

#' @name create_color_dict
#' @title stage color palette
#' @param task \code{data.frame}
#' @details Makes a named list of color codes, where names corresponds to unique stages in \code{task} 
#' @return \code{list} of color codes named by stages 
#' @seealso \code{\link[gantt]{read_task}}
create_color_dict <- function(task) {
    n <- length(unique(task$stage))
    split(hue_pal()(n), rev(unique(task$stage)))
}

#' @name assign_colors_to_task_labels
#' @title Assign color to task labels
#' @param task \code{data.frame}
#' @param color_by_stages \code{logical} if \code{TRUE} then color assigned to each task label (y-axis) is same
#' within stage. If \code{FALSE} then black color is assigned to every task label.
#' @details Makes a named list of color codes, where names corresponds to unique stages in \code{task} 
#' @return \code{list} of task with names of stages 
#' @seealso \code{\link[gantt]{read_task}}
assign_colors_to_task_labels <- function(task, color_by_stages) {
    dict <- create_color_dict(task)
    if (color_by_stages == FALSE){
        return(rep('#000000', nrow(task)))
    }
    as.character(sapply(task$stage, function(x, dict) { dict[[x]] } ,dict))
}

# zrobic refactor i dopracowac opis

#' @name adjust_path_data
#' @title Recalculate control value
#' @details If path needs to be drawn within both stage and control group then 
#' it is treated as drawing path within new control group which satisfy this need. This
#' function recalculates the control groups to fulfill this requirement. This can be seen if 
#' some of tasks are in the same control group but in different stages. Then, after recalculation,
#' this tasks will have same control value within same stage but different within two different
#' stages. In other words, new, unique control value is submitted for pair (stage, control).
#' If path have to be drawn within one of stage of control, then the control is recalculated to fulfill this need.
#' @param path_data \code{data.frame}
#' @param columns \code{list} 
#' 
adjust_path_data <- function(path_data, columns) {
    if (length(columns) == 1) {
        path_data[['control']] <- as.numeric(as.factor(path_data[[columns]]))
        return(path_data)
    } 
    dict <- path_data %>% group_by(stage) %>% summarise(max = max(control))
    stages <- as.data.frame(dict[, 'stage'])[, 'stage']
    maxes <- cumsum(as.numeric(as.data.frame(dict[, 'max'])[, 1]))
    for (i in 2:length(stages)) {
        path_data[path_data$stage == stages[i], ]$control <- as.numeric(path_data[path_data$stage == stages[i], ]$control) + maxes[i-1]
    }
    path_data
}

# dopracowac opis, pomyslec nad refactorem

#' @name critical_path
#' @title Calculate paths for Gantt chart
#' @details Function creates data.frame that allows to draw path with parameters (x : point , y : value)
#' At first, task data.frame is mutated with \link[gantt]{mutate_task} function to generate separate rows for start and end, 
#' later referred as point. Then, iterating over next rows, it is checked either stage or control group 
#' changed between this rows. If so, then new row (between them) is added with piped stage or control values respectively.
#' For pretty paths, the straight lines need to be provided between each two connected task bars. This was accomplished with 
#' following rules for rows with different task values.
#' \itemize{
#' \item{If point value of two next rows is equal then nothing has to be done}
#' \item{If point value of predecessor is greater than successor, then create two middle rows with middle value and 
#' same points as in regarded rows}
#' \item{If point value of predecessor is lower than successor, then create one middle row with value of successor and point of
#' predecessor}
#' }
#' @param task \code{data.frame}
#' 
critical_path <- function(task) {
    task[, 'start'] <- task[, 'start'] - 1 # to draw little horizontal lines after bar start and end
    task[, 'end'] <- task[, 'end'] + 1  # 
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
                r1 <- data.frame(point = shadow['point'], task = current['task'],
                                 type = 'tmp', value = shadow['value'] - 0.5)
                r2 <- data.frame(point = current['point'], task = current['task'],
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
    new_df <- rbind(new_df, current)
    new_df
}


#' @name char
#' @title as.character
#' @param x \code{object}
char <- function(x) {
    as.character(x)
}

#uporzadkowac jakos logicznie te parametry, dodac @return, usunac details na rzecz czegos innego

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
#' @param arrow_label_offset \code{numeric}
#' @param task_bar_width \code{integer}
#' @param plot_title \code{character}
#' @param plot_title_size \code{integer}
#' @param bar_labels \code{string} can be 'people', 'task' or 'None', decides which parameter should label task bars
#' @param date_breaks \code{string} How dense the labels on the x-axis are (\link[ggplot2]{scale_x_date})
#' @param date_minor_breaks \code{string} How dense are vertical lines dividing x-axis (\link[ggplot2]{scale_x_date})
#' @param y_axis_label \code{string} can be 'people', 'task' or 'None', decides which parameter should label y-axis
#' @param add_staging_areas \code{logical} if the each stage should have colored background
#' @param arrows_position \code{string} where staging arrows should be placed, possible 'top' or 'inplace'
#' @param cascade_staging \code{string} decides how the staging areas should be created. If 'None', then
#' start and end of each stage are set as a start and end values of respectively first and last task within stage.
#' If 'on_finish' is provided, then each stage starts while last task of previous stage ends and lasts to the end 
#' of the last task within stage.
#' On the other hand 'on_begin' option makes stage starting point with start of the first task in a stage and ending
#' point with start of first task in the next stage.
#' @param bar_labels_position \code{string} sets position of labels draw for each bar. Can be 'right' or 'bottom'
#' @details 
#' If \code{stage_path = TRUE} and \code{control_path = TRUE} then path is drawn within stages and within
#' control groups. \\ If \code{stage_path = FALSE} and \code{control_path = FALSE} then no path is drawn
#' @export
create_gantt_config <- function(x_axis_text_size = 10, y_axis_text_size = 12, 
                                x_axis_label_position = 'bottom', y_axis_label_position = 'left',
                                task_bar_color = 'type', task_bar_width = 5,
                                plot_title = 'Gantt chart', plot_title_size = 15,
                                stage_path = TRUE, control_path = TRUE, stage_arrows = TRUE,
                                y_axis_color_by_stages = TRUE, arrow_label_offset = 0.3,
                                date_breaks = '1 week', date_minor_breaks = "1 day", 
                                y_axis_label = 'task', bar_labels = 'people',
                                add_staging_areas = FALSE, cascade_staging = 'None',
                                arrows_position = 'inplace', bar_labels_position = 'right') {
    gantt_config <-  as.list(environment(), all=TRUE)
    gantt_config
}

# zrobic refactor, poprawic opis

#' @name delete_path_config
#' @title Delete path endings
#' @details To create paths in a way that there is a little horizontal line at the starting and ending point of task bars, path is created
#' as if tasks would start a day earlier and end one day later. In that manner there are unnecessary horizontal lines at path endings.
#' This function finds the starting and ending point of each visible path and shorten this path from both sides, so the endings are
#' not longer visible.
#' @param path_df \code{data.frame} with path points
#'
delete_path_endings <- function(path_df) {
    path_df <- path_df %>% mutate(!!'id' := rownames(path_df))
    dict <- path_df %>%
        group_by(control) %>% 
        summarise(min = first(.data$id), max = last(.data$id)) %>%
        as.data.frame  
    path_df <- path_df %>% mutate(point = if_else(.data$id %in% dict$min, point + 1, point)) %>%
        mutate(point = if_else(.data$id %in% dict$max, point - 1, point)) %>%
        select(-!!'id')
}


#' @name gantt
#' @title Draws Gantt chart
#' @param task \code{data.frame} with tasks specified, created with \code{\link[gantt]{read_task}} function
#' @param conf \code{list} configuration list with styling parameters, created with \code{\link[gantt]{create_gantt_config}}
#' 
#' @return \code{ggplot object} with gantt chart
#' @export
#' @references  \code{\link[gantt]{read_task}}
#' @references \code{\link[gantt]{create_gantt_config}}
gantt <- function(task, conf) {
    # preparation
    milestones_df <- task[task$type == 'milestone', ]
    levels(task$task) <- c(levels(task$task), ' ')
    task[task$type == 'milestone', 'task'] <-  ' '
    plot_task <- mutate_task(task)
    # drawing
    (ggplot(data = plot_task, aes(x = point, y = task)) + theme_fivethirtyeight()) %>%
        draw_path(task, conf$stage_path, conf$control_path) %>%
        add_staging_areas(task, conf$cascade_staging) %>%
        draw_bars(plot_task, conf$task_bar_width, conf$task_bar_color) %>%
        draw_arrows(task, conf$arrow_label_offset, conf$arrows_position, conf$cascade_staging) %>%
        add_themes(task, conf) %>%
        draw_milestones(milestones_df) %>%
        add_labels(task, conf$bar_labels, conf$bar_labels_position)
}

#' @name draw_bars
#' @title Draw bars
#' @description  Draws bars for each task 
#' @param plt \code{ggplot object}
#' @param plot_task \code{data.frame}
#' @param bar_width \code{numeric}
#' @param coloring_variable \code{string} either 'type' or 'stage'
#' 
draw_bars <- function(plt, plot_task, bar_width, coloring_variable) {
    if (coloring_variable == 'type') {
        colors <- create_legend(task)
        (plt + geom_line(aes(color = type), size = bar_width, data = plot_task) +
                     scale_color_manual(name = 'Type', values = colors, labels = names(colors)))
    } else if (coloring_variable == 'stage') {
        (plt + geom_line(aes(color = stage), size = bar_width, data = plot_task))
    }
}

#' @name draw_path
#' @title Draw path
#' @details Draws path between bars on the plot. If only \code{is_stage_path} equals \code{TRUE} then 
#' path is drawn between every bar within stages. If only \code{is_control_path} equals \code{TRUE} then
#' path is drawn between every bar within control groups. With both values equal \code{TRUE} then
#' path is drawn between every bar within stages and control groups. 
#' Function draws a line by connecting points from \link[gantt]{critical_path} for valid stages and control groups.
#' Function groups path over control groups, previously modified with \link[gantt]{adjust_path_data}
#' which transforms control values in a way satisfying the given logic in \code{is_stage_path} and \code{is_control_path}.
#' 
#' @param plt \code{ggplot object}
#' @param task \code{task}
#' @param is_stage_path \code{logical} if path will be drawn within stages
#' @param is_control_path \code{logical}  if path will be drawn within control groups
#' 
#' @return \code{ggplot object}
#' 
draw_path <- function(plt, task, is_stage_path, is_control_path ) {
    if (is_stage_path == FALSE  & is_control_path == FALSE) return(plt)
    critical <- critical_path(task) 
    where_draw <- c('stage', 'control')[c(is_stage_path, is_control_path)]
    for(colname in where_draw) {
        original_tasks_names <- unique(task[[colname]])
        path_data <- critical %>% filter(!!as.symbol(colname) %in% original_tasks_names)
    }
    path_data <- adjust_path_data(path_data, columns = where_draw) %>%
                    delete_path_endings
    (plt + geom_path(aes(x = point, y = value, group = control), color = '#252525', 
                           data = path_data))
}

#' @name draw_milestones
#' @title Draw milestone points
#' @description Adds geom_point layer with points to corresponding milestones
#' 
#' @param plt \code{ggplot object}
#' @param milestones_df \code{data.frame}, it is data.frame containing every task of milestone type
#' 
#' @return \code{ggplot object}
#'
draw_milestones <- function(plt, milestones_df) {
    (plt + geom_point(data = milestones_df,
                        aes(x = start, y = value), size = 8, shape = 18, col = 'orange'))
     #   + geom_label(data = milestones_df, mapping = aes(x = (start + 6), y = value + 0.5, label = task))
}

#' @name draw_arrows
#' @title Add arrows for stages
#' @details Adds plot layer with arrows describing each stage. Horizontal arrows are drawn under the earliest
#' task bar in each stage. Each starts in starting point of earliest task in the stage and ends in the ending
#' point of latest task in the the stage.
#' @param plt \code{ggplot object}
#' @param task \code{data.frame}
#' @param label_y_offset \code{numeric} offset between arrow and its label
#' @param position \code{string} either 'top' or 'inplace', if 'top', arrows are drawn at the top of the plot.
#' If 'inplace' then arrows are drawn just under first task in each stage. 
#' @param cascade_staging \code{logical} whether next stage starts at the end of the previous stage (\code{TRUE}) or
#' starts when first task in given stage starts (\code{FALSE}). If two stages overlaps then the second behavior occurs.
#' Note that, the \code{TRUE} value leads to continuous staging area, and in \code{FALSE} option, 
#' gaps can be present between stages.
#' 
#' @return \code{ggplot object}
draw_arrows <- function(plt, task, label_y_offset, position, cascade_staging) {
    summaries <- create_summaries(task, cascade_staging)
    if (position == 'top') {
        summaries[, 'value'] <- max(summaries$value)
    }
    (plt + geom_segment(aes(x = start, y = value, xend = end, yend = value), data = summaries, 
                              arrow = arrow(length = unit(0.2,'cm'), ends = 'both', type = 'open'), 
                              lineend = 'round', linejoin = 'round', size = 2) +
                 geom_text(aes(x = middle_date(start, end), y = value + label_y_offset , label = stage) ,
                           data = summaries, fontface = 'bold'))
}

#' @name add_themes
#' @title Add theme to the plot
#' @description Adds styling to the plot with user defined configuration
#' @param plt \code{ggplot object}
#' @param task \code{data.frame}
#' @param conf \code{list} with styling configuration
add_themes <- function(plt, task, conf) {
    colors <- deframe(ggthemes::ggthemes_data[["fivethirtyeight"]])
    col <- assign_colors_to_task_labels(task, conf$y_axis_color_by_stages)
    plt <- plt +
        ggtitle(conf$plot_title) + 
        theme(plot.title = element_text(hjust = 0.5, size = conf$plot_title_size, face = 'bold'), 
              axis.text.y = element_text(size = conf$y_axis_text_size, color = rev(col), face ='bold'),
              axis.text.x = element_text(size = conf$x_axis_text_size),
              axis.title = element_blank(), panel.grid.minor.x = element_line(color = colors['Light Gray']),
              panel.background = element_rect(fill = 'white'), plot.background = element_blank()) +
         scale_x_date(date_breaks = conf$date_breaks, date_labels = "%b %d",
                      date_minor_breaks = conf$date_minor_breaks,
                      position = conf$x_axis_label_position,
                      limits = c(task$start[1], task$end[nrow(task)] + nchar(as.character(task$end[nrow(task)]))))
    # limits expands plot area that geom_label will be fully present in the plot
    if (conf$y_axis_label != 'None') {
        plt <- plt + scale_y_discrete(limits = rev(task$task), position = conf$y_axis_label_position, 
                                      labels = rev(task[[conf$y_axis_label]]), expand = expand_scale(add = 1))
    }
    plt
}

#' @name add_labels
#' @title Add task bar labels
#' @description Adds geom_label layer with names given in \code{label} parameter. Labels are drawn next
#' to the task bars.
#' 
#' @param plt \code{ggplot object}
#' @param task \code{data.frame}
#' @param label \code{string} decides which variable from task will be drawn as a label. Possible values are: 'people',
#' 'task' and 'None'. If 'None', then labels are not added to the plot.
#' @param position \code{string} can be either 'left' or 'bottom', decides where label should be placed
#' referring to the task bar  
#' @return \code{ggplot object}
add_labels <- function(plt, task, label, position = 'right') {
    if (label == 'None') return(plt)
    task <- task %>% filter(!(!!as.symbol(label)) %in% c(' ', ''))
    height_offset <- 0
    if (position == 'bottom') { # end treated as a x-axis postion value
        task$end <- task$start + 2
        height_offset <- 0.35
    }
    else if (position == 'right') {
        task$end <- task$end + 2
    }
    (plt + geom_label(data = task, mapping = aes(x = end, y = value - height_offset, label = !!as.symbol(label)),
                     hjust = 'left'))
}

#' @name add_staging_areas
#' @title Add coloring areas to denote stages
#' 
#' @param plt \code{ggplot object}
#' @param task \code{data.frame}
#' @param cascade_staging \code{string} decides how the staging areas should be created. If 'None', then
#' start and end of each stage are set as a start and end values of respectively first and last task within stage.
#' If 'on_finish' is provided, then each stage starts while last task of previous stage ends and lasts to the end 
#' of the last task within stage.
#' On the other hand 'on_begin' option makes stage starting point with start of the first task in a stage and ending
#' point with start of first task in the next stage.
add_staging_areas <- function(plt, task, cascade_staging) {
    summaries <- create_summaries(task, cascade_staging)
    (plt + geom_rect(mapping = aes(xmin = start, xmax = end, fill = stage), 
                  ymin = -Inf, ymax = Inf,
                  alpha = 0.08, data = summaries, inherit.aes = FALSE)) # legend brokes
}


#' @name create_legend
#' @title Create legend
#' @details Creates color list that can be used to manually scale colors in the plot. It is necessary because
#' bars and milestones are created as different geoms, but has to be considered in one legend.
#' 
#' @param task \code{data.frame}
#' 
#' @return \code{list} with colors, named by types of tasks
#' @seealso \link[gantt]{draw_bars}
create_legend <- function(task) { # order may cause errors
    labels <- levels(task$type)
    milestone_index <- which(labels == 'milestone')
    colors <- hue_pal()(length(labels))
    if (length(milestone_index) != 0) {
        colors[milestone_index] <- 'orange'
    }
    names(colors) <- labels
    colors
}
