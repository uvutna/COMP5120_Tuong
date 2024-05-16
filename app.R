library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
#library(readxl)
library(tidyverse)
library(patchwork)

# Define the UI
ui <- fluidPage(
  titlePanel("Berkeley Admissions Data"),
  # Introduction paragraph
  p("In an analysis of admissions data for UC Berkeley's graduate programs (Fall
    1973), a statistically significant disparity was observed favoring male applicants. 
    The acceptance rate for male applicants exceeded that of female applicants."),
  # Encapsulate DTOutput in a div and control its size
  div(style = "height: 400px; overflow-y: scroll;", DTOutput("data_table")),
  p("The chart below show the acceptance and rejection rate with respect to their gender."),
  plotOutput("pie_charts"),
  #plotOutput("gender_plot"), # Directly include the plot in the UI
  p("Initial statistics show that the acceptance rate for female (35%) is less than male (44%) over all majors. 
    The difference is 9% which is difficult to ignore."), # Include the sentence directly in the UI
  plotOutput("acceptance_rate_plot"),
  p("By stratifying the data, we will have the following statistics"),
  uiOutput("summary_table_html"),
  p("The corresponding calculation (Num/Den) is: "),
  uiOutput("latexOutput_temp"),
  p("These statistics can be visualized by using stack bar chart as follows:"),
  plotOutput("admission_status_plot"),
  p(" We then separate the stacked bar chart by group bar chart. Note that major 
    A, B, D, F show favorite acceptance rate for female. 
    In contrast, other major and major C and E show favorite acceptance rate toward male"),
  plotOutput("acceptance_rate_by_major_plot"),
  p("We then filtered out all rows correspond to major C, E and other major 
    respectively since they show favorite acceptance rate toward male. 
    Which leave us with the following chart"),
  plotOutput("filtered_acceptance_rate_plot"),
  p("We then recalculate the acceptance rate base on gender again. Surprisingly, the gender bias
    in admission still exist even though all majors (A, B, D, F) show good acceptance rate for female"),
  plotOutput("acceptance_rate_gender_plot"),
  p("The update statistics is as follows"),
  uiOutput("filtered_html_table"),
  # p("The reason behind this counterintuitive fact is that 
  # a huge portion of men (2488 Male vs 849 Female) tend to apply to majors that are easy (A, B, D and F).
  # On the other hand, a huge portion of women (3472 Female vs ) tend to apply to majors that are difficult (C, E and Other)."), 
  withMathJax(),
  uiOutput("latexOutput"),
  p("Considering the data on department applications, the varying percentages of rejections highlight the disparity in 
    admission difficulty across departments. 
    Furthermore, it illustrates that women more frequently sought admission into highly competitive departments, 
    which traditionally have lower acceptance rates, even for qualified candidates (like those applying to the English department). 
    On the other hand, men were more likely to opt for departments that were less competitive and had higher acceptance rates 
    (such as engineering)[1]."), 
  plotOutput("applicants_by_major"),
  p("[1] Bickel, P. J., Hammel, E. A., & O'Connell, J. W. (1975). Sex Bias in Graduate Admissions: Data from Berkeley: 
    Measuring bias is harder than is usually assumed, and the evidence 
    is sometimes contrary to expectation. Science, 187(4175), 398-404."),
)

# Define the server logic
server <- function(input, output, session) {
  # Read the dataset
  berkeley_data <- read.csv("berkeley.csv")
  
  # Render the data table with specific height
  output$data_table <- renderDT({
    datatable(berkeley_data, options = list(scrollX = TRUE, pageLength = 10))
  }, container = div(style = "height: calc(100% - 30px); overflow-y: auto;"))
  
  # Render the gender plot directly
  output$gender_plot <- renderPlot({
    # Data preparation
    total_applicants_by_gender <- berkeley_data %>%
      group_by(Gender) %>%
      summarise(TotalApplicants = n(), .groups = 'drop')
    
    # Plotting
    ggplot(total_applicants_by_gender, aes(x = Gender, y = TotalApplicants, fill = Gender)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("M" = "blue", "F" = "red")) +
      geom_text(aes(label = TotalApplicants), vjust = -0.3, color = "black",size=6) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20), 
        axis.title = element_text(size = 16), 
        axis.text = element_text(size = 18)  
      ) +
      labs(title = "Total Number of Male vs. Female Applicants",
           x = "Gender",
           y = "Total Number of Applicants")+
      guides(fill = FALSE)
  }) # End of gender plot
  
  # Render the overall acceptance rates plot
  output$acceptance_rate_plot <- renderPlot({
    # Calculating overall acceptance rates
    overall_rates <- aggregate(Admission ~ Gender, data = berkeley_data, FUN = function(x) mean(x == "Accepted"))
    
    # Plotting overall acceptance rates by gender
    ggplot(overall_rates, aes(x = Gender, y = Admission, fill = Gender)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = scales::percent(Admission, accuracy = 1)), vjust = -0.5,size=5.5) +
      scale_fill_manual(values = c("M" = "blue", "F" = "red")) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20), 
        axis.title = element_text(size = 16), 
        axis.text = element_text(size = 18)   
      ) +
      labs(title = "Overall Acceptance Rates by Gender",
           y = "Acceptance Rate",
           x = "") +
      guides(fill = FALSE) # Remove the legend for fill
  })
  
  
  # Load and render the pre-formatted HTML table
  output$summary_table_html <- renderUI({
    # Assuming the HTML content is stored in a variable `html_content`
    # This is where you'd read the HTML content from the file or a string
    html_content <- readLines("summarize_table.html") # Example path to an HTML file
    HTML(paste(html_content, collapse = "\n"))
  })
  
  
  
  
  # Plot for the proportion of admission status by major and gender
  output$admission_status_plot <- renderPlot({
    ggplot(berkeley_data, aes(x = Major, fill = Admission)) +
      geom_bar(position = "fill") +
      facet_wrap(~Gender) +
      labs(title = "Stacked Bar Chart of Admission Status by Major and Gender",
           x = "Major",
           y = "Proportion of Admission Status") +
      theme(axis.text.x = element_text(angle = 0, hjust = 1))+
      theme(
        plot.title = element_text(size = 20), 
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 18)  
      ) 
  })
  
  berkeley_data$AcceptedNumeric <- ifelse(berkeley_data$Admission == "Accepted", 1, 0)
  
  
  # Add this for the new acceptance rates chart
  output$acceptance_rate_by_major_plot <- renderPlot({
    # Assuming berkeley_data is loaded and has the AcceptedNumeric column
    
    # Recalculate acceptance rates by major and gender
    rates_by_major_gender <- aggregate(AcceptedNumeric ~ Major + Gender, data = berkeley_data, mean)
    
    # Convert to data frame and reshape
    rates_df <- as.data.frame(rates_by_major_gender) %>%
      spread(Gender, AcceptedNumeric) %>%
      mutate(Difference = F - M) %>%
      arrange(desc(Difference)) %>%
      gather(Gender, AcceptedNumeric, -Major, -Difference)
    
    # Use the ordered Major for plotting
    rates_df$Major <- factor(rates_df$Major, levels = unique(rates_df$Major))
    
    # Plot
    ggplot(rates_df, aes(x = Major, y = AcceptedNumeric, fill = Gender)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = scales::percent(AcceptedNumeric), y = AcceptedNumeric + 0.02), 
                position = position_dodge(width = 0.9), size = 5, vjust = 0) +
      scale_fill_manual(values = c("F" = "red", "M" = "blue")) +
      geom_vline(xintercept = 4.5, linetype = "dashed", color = "black") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 16), 
        axis.text = element_text(size = 18)   
      ) +
      labs(title = "Acceptance Rates by Gender Within Each Major",
           y = "Acceptance Rate",
           x = "Major") +
      theme(axis.text.x = element_text(angle = 0, hjust = 1)) 
  })
  
  # Plot for the filtered acceptance rate by gender within each major
  output$filtered_acceptance_rate_plot <- renderPlot({
    filtered_df <- berkeley_data %>% 
      filter(!(Major %in% c("C", "Other", "E"))) %>%
      group_by(Major, Gender) %>%
      summarise(AcceptanceRate = mean(Admission == "Accepted"), .groups = 'drop') %>%
      ungroup()
    
    ggplot(filtered_df, aes(x = Major, y = AcceptanceRate, fill = Gender)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("M" = "blue", "F" = "red")) +
      geom_text(aes(label = scales::percent(AcceptanceRate),
                    y = AcceptanceRate + 0.02, # Adjust for label placement
                    group = Gender),
                position = position_dodge(width = 0.9), vjust = 0,size = 6) +
      labs(title = "Acceptance Rate by Gender Within Each Major",
           x = "Major",
           y = "Acceptance Rate") +
      theme_minimal()+
      theme(
        plot.title = element_text(size = 20), 
        axis.title = element_text(size = 16), 
        axis.text = element_text(size = 18)   
      ) 
  })
  
  # Plot for the acceptance rate by gender, excluding specific majors
  output$acceptance_rate_gender_plot <- renderPlot({
    filtered_df <- berkeley_data %>%
      filter(!(Major %in% c("C", "Other", "E"))) %>%
      group_by(Gender) %>%
      summarise(AcceptanceRate = mean(Admission == "Accepted"), .groups = 'drop')
    
    ggplot(filtered_df, aes(x = Gender, y = AcceptanceRate, fill = Gender)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("M" = "blue", "F" = "red")) +
      geom_text(aes(label = scales::percent(AcceptanceRate), 
                    y = AcceptanceRate + 0.02), # Adjust the y position to place the text above the bars
                position = position_dodge(width = 0.9), vjust = 0, size=6) +
      labs(title = "Acceptance Rate by Gender",
           x = "Gender",
           y = "Acceptance Rate") +
      theme_minimal() +     
      theme(
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 16), 
        axis.text = element_text(size = 18)  
      ) +
      guides(fill = FALSE)
  })
  
  # Add this to render the HTML table
  output$filtered_html_table <- renderUI({
    # Read the HTML content from the file
    html_content <- readLines("summarize_table _filtered.html")
    # Convert the content to HTML that Shiny can render
    HTML(paste(html_content, collapse = "\n"))
  })
  
  # Render the pie charts
  output$pie_charts <- renderPlot({
    # Data for the pie charts
    data_men <- data.frame(
      Status = c("Accepted", "Rejected"),
      Percentage = c(44, 56)
    )
    
    data_women <- data.frame(
      Status = c("Accepted", "Rejected"),
      Percentage = c(35, 65)
    )
    
    # Plot for men
    pie_men <- ggplot(data_men, aes(x = "", y = Percentage, fill = Status)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar(theta = "y") +
      geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 0.5),
                color = "#383838", fontface = "bold", size = 8) +
      scale_fill_manual(values = c("Accepted" = "#FFA500", "Rejected" = "#87CEEB")) +
      labs(title = "Men") +
      theme_void()+
      theme(
        plot.title = element_text(size = 25),

      ) +
      guides(fill = FALSE,size = 5)
    
    # Plot for women
    pie_women <- ggplot(data_women, aes(x = "", y = Percentage, fill = Status)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar(theta = "y") +
      geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 0.5),
                color = "#383838", fontface = "bold", size = 8) +
      scale_fill_manual(values = c("Accepted" = "#FFA500", "Rejected" = "#87CEEB")) +
      labs(title = "Women") +
      theme_void()+     
      theme(
        plot.title = element_text(size = 25),
      ) 
    
    # Combine the plots side by side
    pie_men + pie_women
  })
  
  
  # Render the applicants by major and gender chart
  output$applicants_by_major <- renderPlot({
    # Assuming 'data' is already loaded. If not, you need to read 'berkeley.csv' here
    
    # Aggregate data
    applicant_counts <- berkeley_data %>% # Assuming your dataset is named 'berkeley_data'
      group_by(Major, Gender) %>%
      summarise(Count = n(), .groups = 'drop')
    
    # Order the Major factor
    applicant_counts$Major <- factor(applicant_counts$Major, levels = c("A", "B", "D", "F", "C", "Other", "E"))
    
    # Plot with custom colors and numbers on top
    ggplot(applicant_counts, aes(x = Major, y = Count, fill = Gender)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("F" = "red", "M" = "blue")) +
      geom_text(aes(label = Count), vjust = -0.5, position = position_dodge(width = 0.9), color = "black",size=4.5) +
      geom_vline(xintercept = 4.5, linetype = "dashed", color = "black") +
      labs(title = "Number of Applicants by Major and Gender", x = "Major", y = "Number of Applicants") +
      theme_minimal() +     
      theme(
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 16), 
        axis.text = element_text(size = 18)   
      ) 
  })
  
  output$latexOutput <- renderUI({
    tagList(
      # Include MathJax script with configuration to automatically process math in the text
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-AMS_HTML"),
      tags$script(HTML("
        MathJax.Hub.Queue(['Typeset', MathJax.Hub]);
      ")),
      # Your LaTeX content wrapped in HTML() with the math delimiters that MathJax expects
      HTML('
        <p>Here are the calculations for the female and male acceptance rate respectively (Num/Den):</p>
        <p>$$\\begin{array}{l}
        \\color{red}{\\frac{{89 + 17 + 131 + 25}}{{108 + 25 + 375 + 341}}} \\times 100 \\approx 31\\% \\\\
        \\color{blue}{\\frac{{825 + 353 + 138 + 22}}{{1138 + 560 + 417 + 373}}} \\times 100 \\approx 54\\%
        \\end{array}$$</p>
      ')
    )
  })
  
  
  
  
  output$latexOutput_temp <- renderUI({
    tagList(
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-AMS_HTML"),
      tags$script(HTML("
      MathJax.Hub.Queue(['Typeset', MathJax.Hub]);
    ")),
      HTML('
      <p>$$\\begin{array}{l}
      \\color{red}{\\frac{{89 + 17 + 201 + 131 + 94 + 25 + 937}}{{108 + 25 + 593 + 375 + 393 + 341 + 2486}}} \\times 100 \\approx 35\\% \\\\
      \\color{blue}{\\frac{{825 + 353 + 120 + 138 + 53 + 22 + 2227}}{{1138 + 560 + 325 + 417 + 191 + 373 + 5438}}} \\times 100 \\approx 44\\% 
      \\end{array}$$</p>
    ')
    )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
