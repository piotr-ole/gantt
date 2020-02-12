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
#' @param cascade_summaries \code{logical} should stage just starts after previous ends (\code{TRUE}) or starts with first task
#' in the next stage (\code{FALSE}). Note, if there are stages that overlap then stage always starts with first task 
#' (the \code{FALSE} equivalent).
#' @details  Each row has columns: 'stage', 'start', 'end', 'value' and 'type'. 
#' Type is always set as 'summary', the value is calculated as maximum value of task 
#' in a stage plus 0.3.
#' @return \code{data.frame} with stages summary
#' @seealso \code{\link[gantt]{read_task}}
create_summaries <- function(task, cascade_summaries = FALSE) {
    tab <- task %>%
        group_by(stage) %>%
        summarise(start = min(start), end = max(end), value = max(value) + 0.35, type = 'summary')
    tab <- as.data.frame(tab[order(tab$value, decreasing = TRUE), ])
    if (cascade_summaries == TRUE) {
        for (i in 2:nrow(tab)) { 
            if (tab[i, 'start'] > tab[i-1, 'end']) tab[i , 'start'] <- tab[i-1, 'end']
        }
    }
    tab
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
    split(hue_pal()(n), rev(unique(task$stage)))
}

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
#' @details If path needs to be drawn within both stage and control group then 
#' it is treated as drawing path within new control groups which satisfy this need. This
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
#' @param cascade_staging \code{logical} if TRUE, then arrows arrows and staging backgrounds would depicts
#' stages which starts at the end of previous stage and ends at the end of last task of the stage.
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
                                add_staging_areas = FALSE, cascade_staging = FALSE,
                                arrows_position = 'inplace', bar_labels_position = 'left') {
    gantt_config <-  as.list(environment(), all=TRUE)
    gantt_config
}

#' @name delete_path_config
#' @title Delete path endings
#' @details To create paths in a way that there is a little horizontal line at the starting and ending point of task bars, path is created
#' as if tasks would start a day earlier and end one day later. In that manner there are unnecessary horizontal lines at path endings.
#' This function finds the starting and ending point of each visible path and shorten this path from both sides, so the endings are
#' not longer visible.
#' @param path_df \code{data.frame} with path points
#'
delete_path_endings <- function(path_df) {
    path_df <- path_df %>% mutate(id = rownames(path_df))
    dict <- path_df %>%
        group_by(control) %>% 
        summarise(min = first(id), max = last(id)) %>%
        as.data.frame  
    for (control in unique(path_df$control)) {
        path_df[path_df$control == control & path_df$id == dict[ dict$control == control, 'min'] ,'point'] = path_df[path_df$control == control & path_df$id == dict[ dict$control == control, 'min'] ,'point'] + 1
        path_df[path_df$control == control & path_df$id == dict[ dict$control == control, 'max'] , 'point'] = path_df[path_df$control == control & path_df$id == dict[ dict$control == control, 'max'] , 'point'] - 1
    }
    select(path_df, -id)
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
    # preparation
    milestones_df <- task[task$type == 'milestone', ]
    levels(task$task) <- c(levels(task$task), ' ')
    task[task$type == 'milestone', 'task'] <-  ' '
    plot_task <- mutate_task(task)
    # drawing
    plt <- ggplot(data = plot_task, aes(x = point, y = task)) + theme_fivethirtyeight()
    if (conf$stage_path == TRUE  | conf$control_path == TRUE) {
        plt <- draw_path(plt, task, conf$stage_path, conf$control_path)
    }
    plt <- add_staging_areas(plt, task, conf$cascade_staging) %>%
        draw_bars(plot_task, conf$task_bar_width, conf$task_bar_color) %>%
                draw_arrows(task, conf$arrow_label_offset, conf$arrows_position, conf$cascade_staging) %>%
                add_themes(task, conf) %>%
                draw_milestones(milestones_df) %>%
                add_labels(task, conf$bar_labels, conf$bar_labels_position)
    
    plt
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
        plt <- plt + geom_line(aes(color = type), size = bar_width, data = plot_task)
        colors <- create_legend(task)
        plt <- plt + scale_color_manual(name = 'Type', values = colors, labels = names(colors))
    } else if (coloring_variable == 'stage') {
        plt <- plt + geom_line(aes(color = stage), size = bar_width, data = plot_task)
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
    critical <- critical_path(task)
    where_draw <- c('stage', 'control')
    where_draw <- where_draw[c(is_stage_path, is_control_path)]
    for(colname in where_draw) {
        path_data <- critical[ifelse(critical[[colname]] %in% unique(task[[colname]]), TRUE, FALSE), ]
    }
    path_data <- adjust_path_data(path_data, columns = where_draw)
    path_data <- delete_path_endings(path_data)
    plt <- plt + geom_path(aes(x = point, y = value, group = control), color = '#252525', 
                       data = path_data)
    plt
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
    plt <- plt + geom_point(data = milestones_df,
                        aes(x = start, y = value), size = 8, shape = 18, col = 'orange')
     #   geom_label(data = milestones_df, mapping = aes(x = (start + 6), y = value + 0.5, label = task))
    
    plt
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
    plt <- plt + geom_segment(aes(x = start, y = value, xend = end, yend = value), data = summaries, 
                          arrow = arrow(length = unit(0.2,'cm'), ends = 'both', type = 'open'), 
                          lineend = 'round', linejoin = 'round', size = 2) +
        geom_text(aes(x = middle_date(start, end), y = value + label_y_offset , label = stage) ,
                  data = summaries, fontface = 'bold')
    plt
}

#' @name add_themes
#' @title Add theme to the plot
#' @description Adds styling to the plot with user defined configuration
#' @param plt \code{ggplot object}
#' @param task \code{data.frame}
#' @param conf \code{list} with styling configuration
add_themes <- function(plt, task, conf) {
    colors <- deframe(ggthemes::ggthemes_data[["fivethirtyeight"]])
    col <- assign_colors_to_stages(task, conf$y_axis_color_by_stages)
    plt <- plt +
        ggtitle(conf$plot_title) + 
        theme(plot.title = element_text(hjust = 0.5, size = conf$plot_title_size, face = 'bold'), 
              axis.text.y = element_text(size = conf$y_axis_text_size, color = rev(col), face ='bold'),
              axis.text.x = element_text(size = conf$x_axis_text_size),
              axis.title = element_blank(), panel.grid.minor.x = element_line(color = colors['Light Gray']),
              panel.background = element_rect(fill = 'white'), plot.background = element_blank())
    if (conf$y_axis_label != 'None') {
        plt <- plt + scale_y_discrete(limits = rev(task$task), position = conf$y_axis_label_position, 
                         labels = rev(task[[conf$y_axis_label]]), expand = expand_scale(add = 1))
        } 
    plt <- plt + scale_x_date(date_breaks = conf$date_breaks, date_labels = "%b %d",
                              date_minor_breaks = conf$date_minor_breaks,
                              position = conf$x_axis_label_position,
                              limits = c(task$start[1], task$end[nrow(task)] + nchar(as.character(task$end[nrow(task)]))))
    # limits expands plot area that geom_label will be fully present in the plot
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
add_labels <- function(plt, task, label, position = 'left') {
    if (label == 'None') return(plt)
    task <- task[ !(task[[label]] %in% c(' ', '')), ]
    height_offset <- 0
    if (position == 'bottom') { # end treated as a x-axis postion value
        task$end <- task$start + 2 #middle_date(task$start, task$end) - 2
        height_offset <- 0.35
    }
    else if (position == 'left') {
        task$end <- task$end + 2
    }
    plt <- plt +
        geom_label(data = task, mapping = aes(x = end, y = value - height_offset, label = eval(sym(eval(label)))),
                   hjust = 'left')
    plt
}

add_staging_areas <- function(plt, task, cascade_staging) {
    summaries <- create_summaries(task, cascade_staging)
    plt <- plt + 
        geom_rect(mapping = aes(xmin = start, xmax = end, fill = stage), 
                  ymin = -Inf, ymax = Inf,
                  alpha = 0.08, data = summaries, inherit.aes = FALSE) # legend brokes
    plt
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
