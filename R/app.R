library(shiny)
library(tidyverse)
library(readxl)
library(mgcv)
library(lubridate)
library(bslib)
library(kableExtra)

# --- UI Definition ---
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("Predictive Epidemiological Detection & Outbreak Tracking"),
  
  sidebarLayout(
    sidebarPanel(
      # 1. File Upload
      fileInput("file1", "1. Upload Surveillance Data (.xlsx)", accept = c(".xlsx")),
      
      # 2. Area Selector
      selectInput("region_code", "2. Select Geographic Area:",
                  choices = c("Lombardy (Regional)" = "N_accessi_RL_",
                              "Milan (ASST Milanesi)" = "N_accessi_ASST_Milanesi_",
                              "Valtellina (ASST Valtellina)" = "N_accessi_ASST_Valtellina_")),
      
      # 3. Date Selector (Dynamic)
      uiOutput("date_selector_ui"),
      
      hr(),
      # 4. Run Button
      actionButton("run", "Run Analysis", class = "btn-primary w-100", icon = icon("play")),
      hr(),
      helpText("The model trains on data prior to the selected date and calculates Z-Score/P-Values based on an Adaptive Negative Binomial distribution.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Daily Alert Summary", 
                 htmlOutput("comparison_tab_ui")), 
        tabPanel("Visual Surveillance Horizon", 
                 plotOutput("forecast_plot", height = "800px"))
      )
    )
  )
)

# --- Server Logic ---
server <- function(input, output, session) {
  
  # 1. Data Import
  raw_file_content <- reactive({
    req(input$file1)
    read_excel(input$file1$datapath)
  })
  
  # 2. Dynamic Preprocessing based on Area
  current_data <- reactive({
    req(raw_file_content(), input$region_code)
    
    df <- raw_file_content() |>
      select(Data_inizio_sett, starts_with(input$region_code)) |>
      rename_with(~ str_remove(.x, input$region_code), starts_with(input$region_code)) |>
      pivot_longer(cols = -Data_inizio_sett, names_to = "syndrome", values_to = "cases") |>
      mutate(Data_inizio_sett = as.Date(Data_inizio_sett)) |>
      arrange(syndrome, Data_inizio_sett)
    
    df
  })
  
  # 3. UI: Date Selector
  output$date_selector_ui <- renderUI({
    req(current_data())
    dates <- sort(unique(current_data()$Data_inizio_sett), decreasing = TRUE)
    dateInput("target_date", "3. Choose Prediction Date:", 
              value = max(dates), 
              min = min(dates) + days(30), 
              max = max(dates))
  })
  
  # 4. Analysis Engine (Adaptive Negative Binomial GAM)
  processed_results <- eventReactive(input$run, {
    req(current_data(), input$target_date)
    
    df_full <- current_data() |>
      filter(Data_inizio_sett <= input$target_date) |>
      group_by(syndrome) |>
      mutate(
        week_num = row_number(),
        week_of_year = isoweek(Data_inizio_sett),
        dow = wday(Data_inizio_sett, label = TRUE, week_start = 1) |> 
          as.factor() |> fct_relevel("Mon")
      ) |> ungroup()
    
    syndromes <- unique(df_full$syndrome)
    flat_syndromes <- c("alt_cosc", "conv", "dec_gravi", "int_farm", "rash_cut")
    results_list <- list()
    
    withProgress(message = 'Calculating statistical metrics...', value = 0, {
      for (i in seq_along(syndromes)) {
        s <- syndromes[i]
        
        train_df <- df_full |> filter(syndrome == s & Data_inizio_sett < input$target_date)
        test_df  <- df_full |> filter(syndrome == s & Data_inizio_sett == input$target_date)
        
        # --- Fit GAM based on Syndrome Type ---
        if (s %in% flat_syndromes) {
          # Stiff Model (No seasonality, k=3)
          m <- gam(cases ~ s(week_num, bs = "ts", k = 3) + dow,
                   data = train_df, family = nb())
          bias <- 1 # Frozen baseline
        } else {
          # Flexible Model (Annual seasonality, k=20)
          m <- gam(cases ~ s(week_num, bs = "ts", k = 20) + 
                     s(week_of_year, bs = "cc", k = 12) + dow,
                   data = train_df, family = nb())
          
          # Adaptive 14-day shift
          last_14 <- train_df |> tail(14)
          last_14_pred <- predict(m, newdata = last_14, type = "response")
          bias <- mean(last_14$cases) / mean(last_14_pred)
          if(is.na(bias) || is.infinite(bias)) bias <- 1
        }
        
        theta <- m$family$getTheta(TRUE)
        raw_pred <- predict(m, newdata = test_df, type = "response")
        adaptive_expected <- as.numeric(raw_pred) * bias
        
        test_df <- test_df |>
          mutate(
            expected = adaptive_expected,
            std_dev_nb = sqrt(expected + (expected^2 / theta)),
            z_mag = (cases - expected) / std_dev_nb,
            p_val = pnbinom(cases - 1, mu = expected, size = theta, lower.tail = FALSE),
            pct_dev = ((cases - expected) / expected) * 100,
            
            # Status Logic
            status = case_when(
              p_val <= 0.01 ~ "ALERT", 
              p_val <= 0.05 ~ "WATCH", 
              TRUE          ~ "STABLE"
            ),
            theta_val = theta
          )
        
        # Save bias so the forecast plot can use it!
        results_list[[s]] <- list(test_row = test_df, model = m, history = train_df, bias = bias)
        incProgress(1/length(syndromes))
      }
    })
    results_list
  })
  
  # 5. Output: Dashboard Table (KableExtra)
  output$comparison_tab_ui <- renderUI({
    req(processed_results())
    
    comparison_tab <- map_dfr(processed_results(), ~ .x$test_row) |>
      select(
        Syndrome = syndrome, 
        Observed = cases, 
        Expected = expected, 
        `Z-Mag` = z_mag, 
        `p-val` = p_val, 
        `% Dev` = pct_dev, 
        Status = status
      ) |>
      mutate(
        Expected = round(Expected, 1),
        `Z-Mag` = round(`Z-Mag`, 3),
        `p-val` = round(`p-val`, 3),
        `% Dev` = sprintf("%+.1f%%", `% Dev`)
      )
    
    status_levels <- c("STABLE", "WATCH", "ALERT")
    
    kable(comparison_tab, format = "html", escape = FALSE, align = "c",
          caption = paste("Surveillance Summary:", input$region_code)) |>
      kable_styling(bootstrap_options = c("striped", "hover", "condensed")) |>
      column_spec(5, bold = TRUE, color = ifelse(comparison_tab$`p-val` <= 0.05, "red", "black")) |>
      column_spec(7, color = "black", bold = TRUE,
                  background = spec_color(as.numeric(factor(comparison_tab$Status, levels=status_levels)), 
                                          begin = 0.3, end = 0.9, option = "viridis", direction = 1)) |>
      HTML()
  })
  
  # 6. Output: Surveillance Plot with 3-color bands
  output$forecast_plot <- renderPlot({
    req(processed_results())
    
    all_plot_data <- map_dfr(processed_results(), function(res) {
      m <- res$model
      h <- res$history
      t <- res$test_row
      theta <- t$theta_val[1]
      bias <- res$bias # Retrieve the saved bias factor!
      
      # Forecast next 21 days
      future_dates <- seq(input$target_date + 1, input$target_date + 21, by="day")
      future_df <- tibble(
        Data_inizio_sett = future_dates,
        syndrome = t$syndrome[1],
        week_num = max(h$week_num) + (1:21) + 1,
        week_of_year = isoweek(Data_inizio_sett),
        dow = wday(Data_inizio_sett, label = TRUE, week_start = 1) |> as.factor()
      )
      
      # Apply the adaptive bias to the future predictions as well
      future_df$expected <- as.numeric(predict(m, newdata = future_df, type = "response")) * bias
      
      bind_rows(
        h |> filter(Data_inizio_sett >= input$target_date - 21) |> mutate(type = "History"),
        t |> mutate(type = "Target"),
        future_df |> mutate(type = "Forecast")
      ) |>
        mutate(
          limit_95 = qnbinom(0.95, mu = expected, size = theta),
          limit_99 = qnbinom(0.99, mu = expected, size = theta)
        )
    })
    
    ggplot(all_plot_data, aes(x = Data_inizio_sett)) +
      # 1. STABLE Zone (Green)
      geom_ribbon(aes(ymin = 0, ymax = limit_95, fill = "STABLE"), alpha = 0.2) +
      # 2. WATCH Zone (Yellow)
      geom_ribbon(aes(ymin = limit_95, ymax = limit_99, fill = "WATCH"), alpha = 0.3) +
      # 3. ALERT Zone (Orange/Red)
      geom_ribbon(aes(ymin = limit_99, ymax = Inf, fill = "ALERT"), alpha = 0.1) +
      # Observed Data
      geom_line(aes(y = cases), color = "black", linewidth = 1, na.rm = TRUE) +
      geom_point(aes(y = cases), size = 2, na.rm = TRUE) +
      geom_vline(xintercept = as.numeric(input$target_date), linetype = "dashed", color = "darkred") +
      facet_wrap(~syndrome, scales = "free_y", ncol = 2) +
      # Explicit Color Mapping
      scale_fill_manual(values = c("STABLE" = "#2A9D8F", "WATCH" = "#E9C46A", "ALERT" = "#BC4749")) +
      scale_x_date(date_labels = "%d %b") +
      theme_minimal() +
      labs(title = paste("P.E.D.O.T. Surveillance Horizon -", input$region_code),
           subtitle = paste("Date of analysis:", input$target_date),
           x = "Date", y = "ED Visits", fill = "Risk Level")
  })
}

shinyApp(ui, server)