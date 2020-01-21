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
            selectInput('selectChoice', label = 'Select action', choices = c('Add stage', 'Add Task')),
            uiOutput('addstageUI'),
            uiOutput('addTaskUI')
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            h3('stages'),
            rHandsontableOutput('stagesOutput'),
            h3('Tasks'),
            rHandsontableOutput('tasksOutput')
        )
    )
)

server <- function(input, output) {
    
    appReactive <- reactiveValues()
    appReactive$stages <- list("None")
    
    observeEvent(input$selectChoice, {
    if (input$selectChoice == 'Add stage'){
        output$addstageUI <- renderUI({
            tagList(
                hr(),
                textInput('stageName', 'stage name'),
                actionButton('addstageButton', 'Add stage'))
            })
    } else output$addstageUI <- renderUI({})
        
    if (input$selectChoice == 'Add Task') {
        output$addTaskUI <- renderUI({
            tagList(
                hr(),
                dateRangeInput('taskDateRange', label = 'Task time'),
                textInput('taskName', 'Task Name'),
                selectInput('taskTypeSelect', 'Type of Task', choices = c('critical', 'regular')),
                selectInput('taskstageSelect', 'Task stage', choices = appReactive$stages),
                actionButton('addTaskButton', 'Add Task'))
        })
    } else output$addTaskUI <- renderUI({})
    })
    
    observeEvent(input$addstageButton, {
        if (input$stageName != '') {
            appReactive$stages <- c(isolate(appReactive$stages), input$stageName)
        }
    })
    
    observeEvent(input$addTaskButton, {
        appReactive$tasks <- merge_lists(isolate(appReactive$tasks), 
                                  list(start = input$taskDateRange[1],
                                       end = input$taskDateRange[2],
                                       task = input$taskName,
                                       type = input$taskTypeSelect,
                                       stage = input$taskstageSelect))
    })
    
    output$stagesOutput <- renderRHandsontable({
        rhandsontable(matrix(appReactive$stages), colHeaders = c('stage name'), width = 1000)
    })
    
    output$tasksOutput <- renderRHandsontable({
        df <- rbindlist(appReactive$tasks)
        df$type <- factor(df$type, levels = c('critical', 'regular'))
        colHeaders <-  c('Start date', 'End date', 'Task', 'Task type', 'stage')
        rhandsontable(df, colHeaders = colHeaders, width = 1000, height = 300)
    })
}

shinyApp(ui, server)
