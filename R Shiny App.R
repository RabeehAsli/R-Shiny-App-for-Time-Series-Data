library(shiny)
library(ggplot2)
library(forecast)
library(tseries)
library(zoo)
library(seastests)
library(rugarch)
library(moments)
library(lmtest)

ui <- fluidPage(
  tags$head(
    tags$style(HTML(
      "body { background-color: #FFFACD; } 
       .navbar-default { background-color: #F9E79F; border-color: #F9E79F; } 
       .tab-pane { background-color: white; padding: 15px; border-radius: 5px; }"
    ))
  ),
  titlePanel("Time Series Analysis & Forecasting App"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File"),
      uiOutput("column_select"),
      numericInput("frequency", "Time Series Frequency", 12),
      actionButton("analyze", "Analyze Data")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Exploratory Analysis",
                 tabsetPanel(
                   tabPanel("Original Time Series", plotOutput("originalPlot")),
                   tabPanel("Decomposition", plotOutput("decompositionPlot"))
                 )
        ),
        
        tabPanel("Statistical Tests",
                 tabsetPanel(
                   tabPanel("Stationarity Check", verbatimTextOutput("stationarityCheck")),
                   tabPanel("DF/ADF Test", verbatimTextOutput("dfAdfTest")),
                   tabPanel("HEGY Test", verbatimTextOutput("hegyTest")),
                   tabPanel("ACF & PACF", 
                            plotOutput("acfPlot_st"),
                            plotOutput("pacfPlot_st"))
                 )
        ),
        
        tabPanel("Differencing",
                 tabsetPanel(
                   tabPanel("Original Series ACF/PACF",
                            plotOutput("acfPlot"),
                            plotOutput("pacfPlot")
                   ),
                   tabPanel("Recommended Differencing",
                            verbatimTextOutput("ndiffsOrder"),
                            verbatimTextOutput("nsdiffsOrder")
                   ),
                   tabPanel("Apply Differencing",
                            fluidRow(
                              column(4, numericInput("diff_order", "Differencing (d)", value = 1, min = 0)),
                              column(4, checkboxInput("seasonal_diff", "Apply Seasonal Differencing", value = FALSE)),
                              column(4, conditionalPanel(
                                condition = "input.seasonal_diff == true",
                                numericInput("seasonal_lag", "Seasonal Lag (e.g., 12 for monthly)", value = 12, min = 1)
                              ))
                            ),
                            plotOutput("differencedTSPlot"),
                            plotOutput("diffAcfPlot"),
                            plotOutput("diffPacfPlot"),
                            verbatimTextOutput("stationarityCheckAfterDiff")
                   )
                 )
        ),
        
        
        tabPanel("Post-Differencing Check",
                 verbatimTextOutput("adfDiff"),
                 verbatimTextOutput("kpssDiff")),
        
        tabPanel("Model Selection",
                 h4("Input ARIMA/SARIMA Order Parameters"),
                 fluidRow(
                   column(4, numericInput("ar_order", "AR (p)", value = 1, min = 0)),
                   column(4, numericInput("diff_order", "Differencing (d)", value = 1, min = 0)),
                   column(4, numericInput("ma_order", "MA (q)", value = 1, min = 0))
                 ),
                 checkboxInput("use_seasonal", "Include Seasonality (SARIMA)", value = FALSE),
                 conditionalPanel(
                   condition = "input.use_seasonal == true",
                   fluidRow(
                     column(3, numericInput("sar_order", "Seasonal AR (P)", value = 1, min = 0)),
                     column(3, numericInput("sdiff_order", "Seasonal Diff (D)", value = 1, min = 0)),
                     column(3, numericInput("sma_order", "Seasonal MA (Q)", value = 1, min = 0)),
                     column(3, numericInput("season_period", "Seasonal Period (S)", value = 12, min = 1))
                   )
                 ),
                 actionButton("fit_model", "Fit Model"),
                 verbatimTextOutput("selectedModelSummary"),
                 fluidRow(
                   column(6, verbatimTextOutput("aicValue")),
                   column(6, verbatimTextOutput("bicValue"))
                 ),
                 h4("Auto ARIMA Comparison"),
                 verbatimTextOutput("autoModelSummary"),
                 verbatimTextOutput("modelRecommendation")
        ),
        
        tabPanel("Residual Diagnostics",
                 selectInput("residual_model_choice", "Choose Model for Residual Diagnostics",
                             choices = c("User-defined Model", "Auto ARIMA Model"),
                             selected = "User-defined Model"),
                 
                 tabsetPanel(
                   
                   tabPanel("Normality",
                            h4("Shapiro-Wilk Normality Test or Kolmogorov-Smirnov Test (Based on sample size)"),
                            verbatimTextOutput("normalityTest")
                   ),
                   
                   tabPanel("Autocorrelation",
                            h4("Ljung-Box Test"),
                            verbatimTextOutput("autocorrTest")
                   ),
                   
                   tabPanel("Homoscedasticity",
                            h4("Breusch-Pagan Test"),
                            verbatimTextOutput("homoscedasticityTest")
                   ),
                   
                   tabPanel("Residual Summary Verdict",
                            h4("Overall Residual Assumptions Check"),
                            verbatimTextOutput("residualVerdict")
                   )
                   
                 )
        ),
        
        
        tabPanel("Forecasting",
                 selectInput("forecast_model", "Select Forecasting Model", 
                             choices = c("Holt-Winters")),
                 numericInput("forecast_horizon", "Forecast Horizon", 12),
                 actionButton("forecast", "Generate Forecast"),
                 verbatimTextOutput("forecastSummary"),
                 plotOutput("forecastPlot")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath, header = TRUE)
    df <- na.omit(df)
    updateSelectInput(session, "column", choices = colnames(df))
    return(df)
  })
  
  output$column_select <- renderUI({
    selectInput("column", "Select Time Series Column", choices = colnames(data()))
  })
  
  tsData <- eventReactive(input$analyze, {
    req(input$column)
    df <- data()
    ts(df[[input$column]], frequency = input$frequency)
  })
  
  output$originalPlot <- renderPlot({
    req(tsData())
    autoplot(tsData()) + ggtitle("Original Time Series")
  })
  
  output$decompositionPlot <- renderPlot({
    req(tsData())
    autoplot(decompose(tsData())) + ggtitle("Time Series Decomposition")
  })
  
  output$acfPlot_st <- renderPlot({
    req(tsData())
    acf(tsData(), main = "ACF - Statistical Tests Tab")
  })
  
  output$pacfPlot_st <- renderPlot({
    req(tsData())
    pacf(tsData(), main = "PACF - Statistical Tests Tab")
  })
  
  output$stationarityCheck <- renderPrint({
    req(tsData())
    test_result <- kpss.test(tsData())
    
    cat("KPSS Test for Level Stationarity:\n")
    cat("Test Statistic:", test_result$statistic, "\n")
    cat("P-value:", test_result$p.value, "\n\n")
    
    cat("Interpretation:\n")
    if (test_result$p.value < 0.05) {
      cat("→ Null hypothesis rejected: Time series is likely not stationary.\n")
    } else {
      cat("→ Failed to reject null: Time series is likely stationary.\n")
    }
  })
  
  output$dfAdfTest <- renderPrint({
    req(tsData())
    test_result <- adf.test(tsData())
    
    cat("Augmented Dickey-Fuller Test:\n")
    cat("Test Statistic:", test_result$statistic, "\n")
    cat("P-value:", test_result$p.value, "\n\n")
    
    cat("Interpretation:\n")
    if (test_result$p.value < 0.05) {
      cat("→ Null hypothesis rejected: Time series is likely stationary.\n")
    } else {
      cat("→ Failed to reject null: Time series is likely not stationary.\n")
    }
  })
  
  output$hegyTest <- renderPrint({
    req(tsData())
    sdiffs <- nsdiffs(tsData())
    
    cat("HEGY Test Approximation (Using nsdiffs):\n")
    cat("Approximate Seasonal Differences Required:", sdiffs, "\n\n")
    
    cat("Interpretation:\n")
    if (sdiffs > 0) {
      cat("→ Series exhibits seasonality.\n")
      cat("→ Seasonal differencing of order", sdiffs, "recommended.\n")
    } else {
      cat("→ No strong evidence of seasonality detected.\n")
    }
  })
  
  
  output$acfPlot <- renderPlot({
    req(tsData())
    acf(tsData(), main = "ACF - Original Series")
  })
  
  output$pacfPlot <- renderPlot({
    req(tsData())
    pacf(tsData(), main = "PACF - Original Series")
  })
  
  # Show recommended differencing order
  output$ndiffsOrder <- renderPrint({
    req(tsData())
    order_diff <- ndiffs(tsData())
    paste("Recommended Order of Differencing (d):", order_diff)
  })
  
  # Show recommended seasonal differencing order
  output$nsdiffsOrder <- renderPrint({
    req(tsData())
    sdiff <- nsdiffs(tsData())
    paste("Recommended Seasonal Differencing (D):", sdiff)
  })
  
  # Apply user-defined differencing with optional seasonal differencing
  diffData <- reactive({
    req(tsData())
    data <- tsData()
    
    # Apply regular differencing
    if (input$diff_order > 0) {
      data <- diff(data, differences = input$diff_order)
    }
    
    # Apply seasonal differencing
    if (input$seasonal_diff && input$seasonal_lag > 1) {
      data <- diff(data, lag = input$seasonal_lag)
    }
    
    return(data)
  })
  
  # Differenced series
  output$differencedTSPlot <- renderPlot({
    req(diffData())
    autoplot(diffData()) + ggtitle("Differenced Time Series")
  })
  
  output$diffAcfPlot <- renderPlot({
    req(diffData())
    acf(diffData(), main = "ACF - Differenced Series")
  })
  
  output$diffPacfPlot <- renderPlot({
    req(diffData())
    pacf(diffData(), main = "PACF - Differenced Series")
  })
  
  # ADF test on differenced data
  output$adfDiff <- renderPrint({
    req(diffData())
    test_result <- adf.test(diffData())
    
    cat("ADF Test on Differenced Series:\n")
    cat("Test Statistic:", test_result$statistic, "\n")
    cat("P-value:", test_result$p.value, "\n\n")
    
    cat("Interpretation:\n")
    if (test_result$p.value < 0.05) {
      cat("✅ Stationarity likely achieved after differencing.\n")
    } else {
      cat("⚠️ Series may still not be stationary.\n")
    }
  })
  
  # KPSS test on differenced data
  output$kpssDiff <- renderPrint({
    req(diffData())
    test_result <- kpss.test(diffData())
    
    cat("KPSS Test on Differenced Series:\n")
    cat("Test Statistic:", test_result$statistic, "\n")
    cat("P-value:", test_result$p.value, "\n\n")
    
    cat("Interpretation:\n")
    if (test_result$p.value < 0.05) {
      cat("⚠️ Still not stationary – further differencing or transformation may be needed.\n")
    } else {
      cat("✅ Stationarity achieved (null hypothesis not rejected).\n")
    }
  })
  
  # Fit user-selected ARIMA or SARIMA model
  userModel <- eventReactive(input$fit_model, {
    req(tsData())
    if (input$use_seasonal) {
      Arima(tsData(), 
            order = c(input$ar_order, input$diff_order, input$ma_order),
            seasonal = list(order = c(input$sar_order, input$sdiff_order, input$sma_order), 
                            period = input$season_period))
    } else {
      Arima(tsData(), order = c(input$ar_order, input$diff_order, input$ma_order))
    }
  })
  
  output$selectedModelSummary <- renderPrint({
    req(userModel())
    summary(userModel())
  })
  
  output$aicValue <- renderPrint({
    req(userModel())
    paste("User Model AIC:", round(AIC(userModel()), 2))
  })
  
  output$bicValue <- renderPrint({
    req(userModel())
    paste("User Model BIC:", round(BIC(userModel()), 2))
  })
  
  # Auto ARIMA for comparison
  autoModel <- reactive({
    req(tsData())
    auto.arima(tsData(), seasonal = TRUE)
  })
  
  output$autoModelSummary <- renderPrint({
    req(autoModel())
    summary(autoModel())
  })
  
  # Recommendation based on AIC
  output$modelRecommendation <- renderPrint({
    req(userModel(), autoModel())
    aic_user <- AIC(userModel())
    aic_auto <- AIC(autoModel())
    
    cat("Model Selection Recommendation:\n")
    cat("User-defined Model AIC:", round(aic_user, 2), "\n")
    cat("Auto ARIMA Model AIC:", round(aic_auto, 2), "\n\n")
    
    if (aic_user < aic_auto) {
      cat("✅ Recommendation: Use the user-defined model (lower AIC).\n")
    } else if (aic_user > aic_auto) {
      cat("✅ Recommendation: Use Auto ARIMA (lower AIC).\n")
    } else {
      cat("✅ Recommendation: Both models perform equally well.\n")
    }
  })
  
  
  # Residuals and fitted values
  residualModel <- reactive({
    req(userModel(), autoModel())
    if (input$residual_model_choice == "User-defined Model") {
      return(userModel())
    } else {
      return(autoModel())
    }
  })
  
  residuals_model <- reactive({
    residuals(residualModel())
  })
  
  fitted_model <- reactive({
    fitted(residualModel())
  })
  
  output$normalityTest <- renderPrint({
    
    cat("Normality Check of Residuals:\n")
    
    n <- length(residuals_model())
    
    if (n >= 3 && n <= 5000) {
      test <- shapiro.test(residuals_model())
      cat("Shapiro-Wilk Test:\n")
      cat("Statistic:", round(test$statistic, 4), "\n")
      cat("P-value:", round(test$p.value, 4), "\n")
      
      cat("\nInterpretation:\n")
      if (test$p.value < 0.05) {
        cat("❌ Residuals are likely NOT normally distributed.\n")
      } else {
        cat("✅ Residuals appear normally distributed.\n")
      }
      
    } else {
      cat("Sample size =", n, "– using Kolmogorov-Smirnov Test (n > 5000)\n")
      ks_test <- ks.test(scale(residuals_model()), "pnorm")
      cat("Kolmogorov-Smirnov Test:\n")
      cat("Statistic:", round(ks_test$statistic, 4), "\n")
      cat("P-value:", round(ks_test$p.value, 4), "\n")
      
      cat("\nInterpretation:\n")
      if (ks_test$p.value < 0.05) {
        cat("❌ Residuals are likely NOT normally distributed.\n")
      } else {
        cat("✅ Residuals appear normally distributed.\n")
      }
    }
  })
  
  # 2. Ljung-Box Test for Autocorrelation
  output$autocorrTest <- renderPrint({
    lb <- Box.test(residuals_model(), lag = 10, type = "Ljung-Box")
    cat("Ljung-Box Test for Autocorrelation\n")
    cat("Statistic:", round(lb$statistic, 4), "\n")
    cat("P-value:", round(lb$p.value, 4), "\n\n")
    if (lb$p.value < 0.05) {
      cat("❌ Autocorrelation detected in residuals.\n")
    } else {
      cat("✅ No significant autocorrelation detected in residuals.\n")
    }
  })
  
  # 3. Breusch-Pagan Test for Homoscedasticity
  output$homoscedasticityTest <- renderPrint({
    bp <- bptest(residuals_model() ~ fitted_model())
    cat("Breusch-Pagan Test for Homoscedasticity\n")
    cat("Statistic:", round(bp$statistic, 4), "\n")
    cat("P-value:", round(bp$p.value, 4), "\n\n")
    if (bp$p.value < 0.05) {
      cat("❌ Heteroscedasticity detected (non-constant variance).\n")
    } else {
      cat("✅ Homoscedasticity likely (constant variance).\n")
    }
  })
  
  # 4. Overall Verdict
  output$residualVerdict <- renderPrint({
    shapiro <- shapiro.test(residuals_model())
    lb <- Box.test(residuals_model(), lag = 10, type = "Ljung-Box")
    bp <- bptest(residuals_model() ~ fitted_model())
    
    cat("Residual Analysis Summary Verdict:\n\n")
    issues <- 0
    
    if (shapiro$p.value < 0.05) {
      cat("❌ Normality assumption violated.\n")
      issues <- issues + 1
    } else {
      cat("✅ Normality assumption satisfied.\n")
    }
    
    if (lb$p.value < 0.05) {
      cat("❌ Autocorrelation present in residuals.\n")
      issues <- issues + 1
    } else {
      cat("✅ No autocorrelation in residuals.\n")
    }
    
    if (bp$p.value < 0.05) {
      cat("❌ Heteroscedasticity detected.\n")
      issues <- issues + 1
    } else {
      cat("✅ Constant variance assumption satisfied.\n")
    }
    
    cat("\n🔎 Final Check:\n")
    if (issues == 0) {
      cat("✅ Residuals satisfy all assumptions — model diagnostics are good.\n")
    } else {
      cat(paste("⚠️", issues, "assumption(s) violated — consider model refinement.\n"))
    }
  })
  
  output$forecastPlot <- renderPlot({
    req(tsData())
    forecast_model <- HoltWinters(tsData())
    forecast_values <- forecast(forecast_model, h = input$forecast_horizon)
    autoplot(forecast_values) + ggtitle("Forecast Plot")
  })
  
  output$forecastSummary <- renderPrint({
    req(tsData())
    forecast_model <- HoltWinters(tsData())
    forecast_values <- forecast(forecast_model, h = input$forecast_horizon)
    print(summary(forecast_values))
  })
}

shinyApp(ui = ui, server = server)