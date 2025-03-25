library(shiny)
library(shinyWidgets)
library(plotly)

# Define the UI
ui <- fluidPage(
    titlePanel("UI Testing Checklist"),
    fluidRow(
        column(
            width = 8,  # 80% width for the checklist
            h4("Checklist"),
            uiOutput("checklist_ui"),
            actionButton("reset_btn", "Reset Checklist", icon = icon("redo"))
        ),
        column(
            width = 4,  # 20% width for the progress chart
            h4("Progress"),
            plotlyOutput("progress_chart")
        )
    )
)


# Define the server
# Define the server
server <- function(input, output, session) {

    # Initialize checklist items
    checklist <- reactiveVal(
        data.frame(
            Task = c(
                "Search by all (ARU, Camera, Point Count)",
                "Search with multiple layers",
                "Search with filters",
                "Search with a polygon",
                "Navigate to a Published - Map Only Project",
                "Create a social account (Google)",
                "Create an Auth0 account (Email)",
                "Verify the email",
                "Forget your password",
                "Login via Data Discover",
                "Create an Organization",
                "Create a Project"
            ),
            Section = c(
                "Data Discover",
                "Data Discover",
                "Data Discover",
                "Data Discover",
                "Navigation",
                "Account Management",
                "Account Management",
                "Account Management",
                "Account Management",
                "Login",
                "Project Management",
                "Project Management"
            ),
            Status = "null", # null = not done, pass = successful, fail = failed
            stringsAsFactors = FALSE
        )
    )

    # Render checklist UI
    output$checklist_ui <- renderUI({
        items <- checklist()
        sections <- unique(items$Section)
        lapply(sections, function(section) {
            div(
                h5(section, style = "font-weight: bold; margin-top: 15px;"),
                lapply(which(items$Section == section), function(i) {
                    fluidRow(
                        column(8, p(items$Task[i], style = "font-size: 0.9em;")),
                        column(4,
                               actionButton(
                                   inputId = paste0("task_status_pass_", i),
                                   label = "\u2713", # Checkmark for Pass
                                   style = if (items$Status[i] == "pass") "color: green;" else ""
                               ),
                               actionButton(
                                   inputId = paste0("task_status_fail_", i),
                                   label = "\u2717", # X mark for Fail
                                   style = if (items$Status[i] == "fail") "color: red;" else ""
                               )
                        )
                    )
                })
            )
        })
    })


    # Reset checklist status
    observeEvent(input$reset_btn, {
        checklist(
            checklist() %>% mutate(Status = "null")
        )
    })

    # Update checklist status for individual tasks
    # Update checklist status for individual tasks
observe({
    items <- checklist()

    lapply(1:nrow(items), function(i) {
        observeEvent(input[[paste0("task_status_pass_", i)]], {
            # Set status to "pass" and deselect "fail"
            items$Status[i] <- "pass"
            checklist(items)
        })

        observeEvent(input[[paste0("task_status_fail_", i)]], {
            # Set status to "fail" and deselect "pass"
            items$Status[i] <- "fail"
            checklist(items)
        })
    })
})


    # Calculate progress
    progress_data <- reactive({
        items <- checklist()
        total <- nrow(items)
        passed <- sum(items$Status == "pass")
        failed <- sum(items$Status == "fail")
        null <- sum(items$Status == "null")
        data.frame(
            Category = c("Pass", "Fail", "Not Done"),
            Count = c(passed, failed, null),
            Percent = c(passed / total, failed / total, null / total) * 100
        )
    })

    # Render pie chart
    output$progress_chart <- renderPlotly({
        data <- progress_data()
        plot_ly(
            data,
            labels = ~Category,
            values = ~Percent,
            type = "pie",
            textinfo = "label+percent",
            marker = list(colors = c("#28a745", "#dc3545", "#6c757d")) # Pass: green, Fail: red, Null: gray
        ) %>% layout(
            showlegend = TRUE
        )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
