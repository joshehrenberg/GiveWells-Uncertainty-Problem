library(shiny)
library(magrittr)
library(dplyr)
library(purrr)
library(ggplot2)

ui <- fluidPage(
  # Application title
  titlePanel("Selection Rule Portfolio Comparison"),
  fluidRow(
    column(6,
           sliderInput("n_programs", "Number of programs", 1000, 100000, 50000),
           sliderInput("n_trials", "Number of trials per program", 2, 10, 3)
    ),
    column(6,
           numericInput("mean_threshold", "Threshold the measured mean must cross to be selected", 3, 1, 10),
           numericInput("lower_bound_threshold", "Threshold the lower bound of the confidence interveal must cross to be selected", 2.5, 0, 8),
           numericInput("alpha", "alpha (1 - confidence level) for lower bound", .2, .1, .5)
    )
  ),
  fluidRow(
    column(4,
      sliderInput("intervention_mean", "Average effectiveness", -3, 5, 0),
      sliderInput("intervention_sd", "Standard deviation of effectiveness", 0.1, 10, 1.5)
    ),
    column(8,
      plotOutput("intervention_distribution_plot", height = "200px")
    )
  ),
  fluidRow(
    column(4,
      sliderInput("measurement_error_shape", "Shape parameter of measurement error", 0.1, 3, 1),
      sliderInput("measurement_error_scale", "Scale parameter of measurement error", 0.1, 3, 1)
    ),
    column(8,
      plotOutput("measurement_error_distribution_plot", height = "200px")
    )
  ),
  fluidRow(
    tabsetPanel(
      tabPanel("Overall Portfolio",
               fluidRow(plotOutput("comparison_portfolio_plot")),
               fluidRow(
                 column(width = 4, offset = 4, tableOutput("portfolio_table"))
               )
      ),
      tabPanel("Portfolio Differences",
        fluidRow(
          column(6,
                 plotOutput("comparison_single_measurement_error_plot")
          ),
          column(6,
            plotOutput("comparison_single_effectiveness_plot"),
          )
        ),
        fluidRow(
          column(width = 4, offset = 4, tableOutput("single_selection_table"))
        )
      )
    )
  )
)

server <- function(input, output) {

##Simulations
  #Create a set of random interventions with a random mean taken from a normal distribution
  # and a random trial level measurement error taken from a gamma distribution
  intervention_data <- reactive({
    true_mean <- rnorm(input$n_programs, input$intervention_mean, input$intervention_sd)
    true_measurement_error <- rgamma(input$n_programs, shape = input$measurement_error_shape, scale = input$measurement_error_scale)

    #Simulate "trials" of each intervention. In each trial a result is drawn from
    #a distribution with the same mean as the true distribution with a standard deviation
    #equal to the true measurement error
    trial_results <- map2(true_mean, true_measurement_error, rnorm, n = input$n_trials)

    #Find the measured mean and standard deviation of the trial results
    measured_mean <- map_dbl(trial_results, mean)
    measured_standard_deviation <- map_dbl(trial_results, sd)

    #Turn everything into a single data frame for analysis
    trial_results <- do.call(rbind.data.frame, trial_results)
    names(trial_results) <- paste0("Trial_", 1:input$n_trials)
    intervention_data <- cbind.data.frame(true_mean, true_measurement_error, trial_results, measured_mean, measured_standard_deviation)

    #Add which interventions meet the acceptance criteria
    intervention_data <- mutate(intervention_data, meets_threshold_mean = measured_mean > input$mean_threshold) %>%
      mutate(lower_confidence_bound = measured_mean + (qt(input$alpha, input$n_trials - 1) * measured_standard_deviation / sqrt(input$n_trials))) %>%
      mutate(meets_threshold_lower_bound = lower_confidence_bound  > input$lower_bound_threshold)
  })
    #Combine selected results into portfolios of selected interventions
  portfolio_data <- reactive({threshold_portfolio_mean <- filter(intervention_data(), meets_threshold_mean == TRUE) %>%
      mutate(selection_rule = "Mean", .before = 1)

    threshold_portfolio_lower_bound <- filter(intervention_data(), meets_threshold_lower_bound == TRUE) %>%
      mutate(selection_rule = "Lower Bound", .before = 1)

    portfolio_data <- rbind.data.frame(threshold_portfolio_mean, threshold_portfolio_lower_bound)
  })
  #Summarize selected results
  portfolio_summary <- reactive({group_by(portfolio_data(), selection_rule) %>%
      summarise(mean_effectiveness = mean(true_mean), mean_sd = mean(true_measurement_error), n_programs_selected = n())
  })
    #Pull out results only selected by one rule
  selected_by_mean_only <- reactive({filter(portfolio_data(), meets_threshold_mean == TRUE & meets_threshold_lower_bound == FALSE)})

  selected_by_lower_bound_only <- reactive({filter(portfolio_data(), meets_threshold_mean == FALSE & meets_threshold_lower_bound == TRUE)})

  single_selection_data <- reactive({rbind.data.frame(selected_by_mean_only(), selected_by_lower_bound_only())})

  #Summarize results selected by one rule
  single_selection_summary <- reactive({group_by(single_selection_data(), selection_rule) %>%
      summarise(mean_effectiveness = mean(true_mean), mean_sd = mean(true_measurement_error), n_programs_selected = n())})


##Plots
  #Distribution of intervention effectiveness plot
  output$intervention_distribution_plot <- renderPlot({
    ggplot(intervention_data(), aes(x = true_mean)) +
      geom_density(alpha = .5, aes(color = "2", fill = "2")) +
      theme_minimal() +
      scale_fill_brewer(palette = "Dark2") +
      scale_color_brewer(palette = "Dark2") +
      guides(color = "none", fill = "none") +
      xlab("Effectiveness") +
      labs(title = "Selected effectiveness of all interventions")
  })

  #Distribution of measurement error plot
  output$measurement_error_distribution_plot <- renderPlot({
    ggplot(intervention_data(), aes(x = true_measurement_error)) +
      geom_density(alpha = .5, aes(color = "1", fill = "1")) +
      theme_minimal() +
      scale_fill_brewer(palette = "Dark2") +
      scale_color_brewer(palette = "Dark2") +
      guides(color = "none", fill = "none") +
      xlab("Measurement Error") +
      labs(title = "Selected measurement error of all interventions")
  })

  #Compare each portfolio of interventions plot
  output$comparison_portfolio_plot <- renderPlot({
    ggplot(portfolio_data(), aes(x = true_mean)) +
      geom_density(alpha = .4, aes(color = selection_rule, fill = selection_rule)) +
      geom_vline(aes(xintercept = mean_effectiveness, color = selection_rule), data = portfolio_summary()) +
      scale_fill_brewer(palette = "Dark2") +
      scale_color_brewer(palette = "Dark2") +
      theme_minimal() +
      xlab("Effectiveness") +
      facet_wrap(vars(selection_rule), ncol = 1) +
      labs(title = "Comparative effectiveness of portfolios selected by each decision rule")
  })

  #Compare the effectiveness of interventions chosen by one rule and not the other
  output$comparison_single_effectiveness_plot <- renderPlot({
    ggplot(single_selection_data(), aes(x = selection_rule)) +
      geom_violin(aes(y = true_measurement_error, x = selection_rule, fill = selection_rule), alpha = .4) +
      scale_fill_brewer(palette = "Dark2") +
      theme_minimal() +
      labs(title = "Comparison of measurement error of interventions only selected by one decision rule") +
      xlab("Selection_rule") +
      ylab("Measurement Error")
  })

  #Compare the measurement error of interventions chosen by one rule and not the other
  output$comparison_single_measurement_error_plot <- renderPlot({
    ggplot(single_selection_data(), aes(x = selection_rule)) +
      geom_violin(aes(y = true_mean, x = selection_rule, fill = selection_rule), alpha = .4) +
      scale_fill_brewer(palette = "Dark2") +
      theme_minimal() +
      labs(title = "Comparison of effectiveness of interventions only selected by one decision rule") +
      xlab("Selection_rule") +
      ylab("Mean Effectiveness")
  })
  output$portfolio_table <- renderTable(portfolio_summary(), align = 'c')
  output$single_selection_table <- renderTable(single_selection_summary(), align = 'c')
}

shinyApp(ui, server)
