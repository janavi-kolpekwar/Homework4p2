# loading libraries
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(lubridate)
library(readr)
library(caret)

# data
train <- read_csv("train_dataset.csv.gz")
test  <- read_csv("test_dataset.csv.gz")

factor_cols <- c("provider_id", "address", "specialty")
for (col in factor_cols) {
  combined_levels <- union(unique(train[[col]]), unique(test[[col]]))
  train[[col]] <- factor(train[[col]], levels = combined_levels)
  test[[col]]  <- factor(test[[col]],  levels = combined_levels)
}

# outcome to factor
train$no_show <- factor(train$no_show, levels = c(0, 1), labels = c("Show", "NoShow"))
test$no_show  <- factor(test$no_show,  levels = c(0, 1), labels = c("Show", "NoShow"))

# fine-tuning and feature engineering
train <- train %>% mutate(days_between = as.numeric(difftime(appt_time, appt_made, units = "days")))
test  <- test  %>% mutate(days_between = as.numeric(difftime(appt_time, appt_made, units = "days")))

# logistic regression model (train)
set.seed(123)
model <- train(
  no_show ~ provider_id + address + specialty + days_between,
  data = train,
  method = "glm",
  trControl = trainControl(method = "cv", number = 5, classProbs = TRUE)
)

# test data
test$pred_prob  <- predict(model, newdata = test, type = "prob")[, "NoShow"]
test$pred_class <- predict(model, newdata = test)
test <- test %>%
  mutate(
    PUR = (1 - pred_prob) * 100,
    appt_date = as.Date(appt_time),
    appt_hour = hour(appt_time)
  )

# hour slots
hour_labels <- c(
  "07:00 AM - 08:00 AM", "08:00 AM - 09:00 AM", "09:00 AM - 10:00 AM",
  "10:00 AM - 11:00 AM", "11:00 AM - 12:00 PM", "12:00 PM - 01:00 PM",
  "01:00 PM - 02:00 PM", "02:00 PM - 03:00 PM", "03:00 PM - 04:00 PM",
  "04:00 PM - 05:00 PM", "05:00 PM - 06:00 PM", "06:00 PM - 07:00 PM",
  "07:00 PM - 08:00 PM"
)
hour_breaks <- 7:19  # corresponding start hours

# ui
ui <- dashboardPage(
  dashboardHeader(title = "Cumulative No-Show Forecast Dial Dashboard", titleWidth = 420),
  
  dashboardSidebar(
    width = 250,
    h4("Filters", style = "padding-left: 15px; margin-top: 10px;"),
    dateRangeInput("date_range", "Select Week:",
                   start = min(test$appt_date),
                   end   = min(test$appt_date) + 6),
    hr(),
    selectInput("provider", "Provider:", choices = c("All", sort(unique(test$provider_id)))),
    selectInput("site", "Address:", choices = c("All", sort(unique(test$address)))),
    hr(),
    selectInput("hour_slot", "Select 1-Hour Slot:",
                choices = setNames(hour_breaks, hour_labels),
                selected = 10),
    hr(),
    actionButton("refresh", "🔄 Refresh Forecast")
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .box { border-radius: 8px; }
      .legend-box {
        background-color: #f9f9f9;
        padding: 10px;
        border-radius: 5px;
        border: 1px solid #ddd;
        margin-top: 10px;
      }
      .legend-item {
        display: flex;
        align-items: center;
        margin-bottom: 4px;
      }
      .legend-color {
        width: 18px;
        height: 18px;
        border-radius: 3px;
        margin-right: 8px;
      }
      .btn-custom {
        width: 100%;
        margin-bottom: 10px;
        background-color: white;
        border: 1px solid #ddd;
        padding: 12px;
        border-radius: 5px;
      }
      .btn-custom:hover { background-color: #f5f5f5; }
    "))),
    
    fluidRow(
      column(width = 7,
             box(width = NULL, title = "Predicted Utilization Forecast Dial",
                 solidHeader = TRUE, status = "primary",
                 plotlyOutput("forecast_dial", height = "400px"),
                 uiOutput("legend_UI")),
             
             box(width = NULL, title = "Appointment Summary",
                 solidHeader = TRUE, status = "info", uiOutput("summary_text"))
      ),
      
      column(width = 5,
             box(width = NULL, title = "Actions", solidHeader = TRUE, status = "success",
                 actionButton("send_reminders", "📧 Send Reminders", class = "btn-custom"),
                 actionButton("export_pdf", "📄 Export PDF Report", class = "btn-custom")
             ),
             box(width = NULL, title = "Operational Guidance",
                 solidHeader = TRUE, status = "warning",
                 p("Use this dashboard to identify hours when predicted utilization (PUR) drops below 70%."),
                 p("These time slots indicate higher no-show risk and are ideal for controlled overbooking."),
                 p("Adjust schedules dynamically to maintain optimal clinic efficiency."))
      )
    )
  )
)

# server
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    selected_hour <- as.numeric(input$hour_slot)
    df <- test %>%
      filter(appt_date >= input$date_range[1],
             appt_date <= input$date_range[2],
             appt_hour == selected_hour)
    if (input$provider != "All") df <- df %>% filter(provider_id == input$provider)
    if (input$site != "All") df <- df %>% filter(address == input$site)
    df
  })
  
  summary_stats <- reactive({
    df <- filtered_data()
    avg_PUR  <- round(mean(df$PUR, na.rm = TRUE), 1)
    avg_noshow <- round(mean(df$pred_prob, na.rm = TRUE) * 100, 1)
    total_appts <- nrow(df)
    list(PUR = avg_PUR, noshow = avg_noshow, total = total_appts)
  })
  
  output$forecast_dial <- renderPlotly({
    s <- summary_stats()
    plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = s$PUR,
      title = list(text = "Predicted Utilization Rate (PUR)", font = list(size = 18)),
      number = list(suffix = "%", font = list(size = 42)),
      gauge = list(
        axis = list(range = list(0, 100), tickwidth = 1, tickcolor = "gray"),
        bar = list(color = "#0072B2", thickness = 0.25),
        steps = list(
          list(range = c(0, 60),  color = "#D55E00"),  # Red
          list(range = c(60, 80), color = "#E69F00"),  # Yellow/Amber
          list(range = c(80, 100), color = "#009E73")  # Green
        ),
        threshold = list(
          line = list(color = "#0072B2", width = 4),
          thickness = 0.8,
          value = s$PUR
        )
      )
    ) %>%
      layout(
        margin = list(l = 20, r = 20, t = 50, b = 20),
        paper_bgcolor = "white",
        font = list(color = "black", family = "Arial")
      )
  })
  
  output$legend_UI <- renderUI({
    div(class = "legend-box",
        h5("Legend: Color Interpretation"),
        div(class = "legend-item",
            div(class = "legend-color", style = "background-color:#D55E00;"), "Low Utilization (<60%) – High Risk"),
        div(class = "legend-item",
            div(class = "legend-color", style = "background-color:#E69F00;"), "Moderate Utilization (60–80%) – Watch Zone"),
        div(class = "legend-item",
            div(class = "legend-color", style = "background-color:#009E73;"), "High Utilization (>80%) – Stable"))
  })
  
  output$summary_text <- renderUI({
    s <- summary_stats()
    HTML(paste0(
      "<p><b>Total Appointments:</b> ", s$total, "</p>",
      "<p><b>Average Predicted No-Show:</b> ", s$noshow, "%</p>",
      "<p><b>Predicted Utilization Rate (PUR):</b> ", s$PUR, "%</p>"
    ))
  })
  
  observeEvent(input$send_reminders, {
    showModal(modalDialog("Reminders have been sent to high-risk patients.", easyClose = TRUE))
  })
  
  observeEvent(input$export_pdf, {
    showModal(modalDialog("Weekly utilization report generated and downloaded.", easyClose = TRUE))
  })
}

shinyApp(ui = ui, server = server)



