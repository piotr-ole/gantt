merge_lists <- function(l1, l2) {
    c(l1, list(l2))
}

ui <- fluidPage(
    
    # App title ----
    titlePanel("Gantt app"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Slider for the number of bins ----
            selectInput('selectChoice', label = 'Select action', choices = c('Add Faze', 'Add Task')),
            uiOutput('addFazeUI'),
            uiOutput('addTaskUI')
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            h3('Fazes'),
            rHandsontableOutput('fazesOutput'),
            h3('Tasks'),
            rHandsontableOutput('tasksOutput')
        )
    )
)

server <- function(input, output) {
    
    appReactive <- reactiveValues()
    appReactive$fazes <- list("None")
    
    observeEvent(input$selectChoice, {
    if (input$selectChoice == 'Add Faze'){
        output$addFazeUI <- renderUI({
            tagList(
                hr(),
                textInput('fazeName', 'Faze name'),
                actionButton('addFazeButton', 'Add Faze'))
            })
    } else output$addFazeUI <- renderUI({})
        
    if (input$selectChoice == 'Add Task') {
        output$addTaskUI <- renderUI({
            tagList(
                hr(),
                dateRangeInput('taskDateRange', label = 'Task time'),
                textInput('taskName', 'Task Name'),
                selectInput('taskTypeSelect', 'Type of Task', choices = c('critical', 'regular')),
                selectInput('taskFazeSelect', 'Task Faze', choices = appReactive$fazes),
                actionButton('addTaskButton', 'Add Task'))
        })
    } else output$addTaskUI <- renderUI({})
    })
    
    observeEvent(input$addFazeButton, {
        if (input$fazeName != '') {
            appReactive$fazes <- c(isolate(appReactive$fazes), input$fazeName)
        }
    })
    
    observeEvent(input$addTaskButton, {
        appReactive$tasks <- merge_lists(isolate(appReactive$tasks), 
                                  list(start = input$taskDateRange[1],
                                       end = input$taskDateRange[2],
                                       task = input$taskName,
                                       type = input$taskTypeSelect,
                                       faze = input$taskFazeSelect))
    })
    
    output$fazesOutput <- renderRHandsontable({
        rhandsontable(matrix(appReactive$fazes), colHeaders = c('Faze name'), width = 1000)
    })
    
    output$tasksOutput <- renderRHandsontable({
        df <- rbindlist(appReactive$tasks)
        df$type <- factor(df$type, levels = c('critical', 'regular'))
        colHeaders <-  c('Start date', 'End date', 'Task', 'Task type', 'Faze')
        rhandsontable(df, colHeaders = colHeaders, width = 1000, height = 300)
    })
}

shinyApp(ui, server)
