library(shiny)
library(dplyr)
library(arules)
library(arulesViz)
library(svDialogs)

ui <- fluidPage(
  titlePanel("Our DS Project"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Dataset:", accept = ".csv"),
      numericInput("clusters", "Enter Number of Clusters", 3, min = 2, max = 4),
      actionButton("cluster", "Submit Clusters"),
      numericInput("confidence", "Enter The Confidence", value = 1, min = 0.001, max = 1, step = 0.001),
      actionButton("confidence_btn", "Submit Confidence"),
      numericInput("support", "Enter The Support", 1, min = 0.001, max = 1),
      actionButton("support_btn", "Submit Support"),
      actionButton("run_association", "Run Association Rules"),
      actionButton("run_visualization", "Run Visualizations")  
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Clustering Results", 
                 h3("K-means Clustering"),
                 tableOutput("cluster_table"),
                 plotOutput("cluster_plot")), 
        
        tabPanel("Association Rules", 
                 h3("Generated Rules"),
                 tableOutput("rules_output"),  # جدول القواعد
                 plotOutput("rules_graph"),
                 plotOutput("rules_scatter")),
        
        tabPanel("Data Visualizations",
                 h3("Visualizations"),
                 plotOutput("city_spending_barplot"),
                 plotOutput("payment_type_pie"),
                 plotOutput("spending_by_age_plot"),
                 plotOutput("total_spending_boxplot"))
      )
    )
  )
)

server <- function(input, output, session) {
  observe({
    validate(
      need(input$confidence >= 0.001 && input$confidence <= 1, 
           "The confidence must be between 0.001 and 1.")
    )
  })
  
  data_input <- reactive({
    req(input$file)  
    read.csv(input$file$datapath)  
  })
  
  clustered_data <- eventReactive(input$cluster, {
    req(data_input()) 
    my_data <- data_input()
    newdata <- my_data %>% select(total, age)
    numclusters <- input$clusters   
    kmeansresult <- kmeans(newdata, centers = numclusters)  
    my_data$cluster <- kmeansresult$cluster
    
    finaldata <- my_data[, c("customer", "total", "age", "cluster")]
    summarydata <- aggregate(total ~ customer + age, data = my_data, FUN = sum)
    summarydata <- merge(summarydata, finaldata[, c("customer", "cluster")],
                         by = "customer", all.x = TRUE)
    summarydata_20 <- summarydata %>% sample_n(20)
    list(finaldata = finaldata, summarydata_20 = summarydata_20, kmeansresult = kmeansresult)
  })
  
  output$cluster_table <- renderTable({
    req(clustered_data())  
    clustered_data()$finaldata  
  })
  
  output$cluster_plot <- renderPlot({
    req(clustered_data()) 
    summarydata_20 <- clustered_data()$summarydata_20
    colors <- c("red", "green", "blue", "orange")   
    plot(summarydata_20$age,
         summarydata_20$total,
         col = colors[summarydata_20$cluster],
         pch = 19,  
         xlab = "Age",
         ylab = "Total Spending",
         main = "Customers' Clustering Scatter Plot")
  }) 
  
  # جزء Association Rules:
  rules_data <- eventReactive(input$run_association, {
    req(data_input()) 
    my_data <- data_input()
    transactions <- as(my_data, "transactions")
    rules <- apriori(transactions, parameter = list(support = input$support, confidence = input$confidence))
    rules
  })
  
  output$rules_output <- renderTable({
    req(rules_data())  
    rules_df <- as(rules_data(), "data.frame")  # تحويل القواعد إلى data.frame لعرضها في جدول
    rules_df
  })
  
  output$rules_graph <- renderPlot({
    req(rules_data())  
    plot(rules_data(), method = "graph", control = list(type = "items"))  
  })
  
  output$rules_scatter <- renderPlot({
    req(rules_data())  
    plot(rules_data(), method = "scatterplot", measure = c("support", "confidence"), shading = "lift")  
  })
  
  observeEvent(input$run_visualization, {
    req(data_input())  
    datam <- data_input()
    
    output$city_spending_barplot <- renderPlot({
      city_spending <- aggregate(total ~ city, data = datam, sum)
      cities <- c("Alexandria", "Cairo", "Hurghada", "Dakahlia", "Sohag", "Port Said", "Giza", "Fayoum", "Gharbia", "Aswan")
      totals <- c(2509308, 2415823, 1626544, 869425, 866690, 815411, 795044, 792390, 785008, 751420)
      sorted_numbers <- sort(totals, decreasing = TRUE)
      colors <- c("palevioletred1", "palevioletred3", "peachpuff", "peachpuff3", "pink", "pink3", "plum1", "plum4", "purple1", "purple4")
      
      barplot(height = totals, names.arg = cities, col = colors[1:length(city_spending$city)],
              main = "City Total Spending Chart", xlab = "Cities", ylab = "Total Spending")
    })
    
    output$payment_type_pie <- renderPlot({
      types <- factor(datam$paymentType)
      x <- c(4774, 4708)
      percentage <- paste0(round(100 * x / sum(x), 1), "%")
      pie(x, labels = percentage, main = "Comparison between Cash and Credit Totals", col = c("lightskyblue4", "lightgray"))
      legend("bottomright", legend = c("Cash", "Credit"), fill = c("lightskyblue4", "lightgray"))
    })
    
    output$spending_by_age_plot <- renderPlot({
      city_spending <- aggregate(total ~ age, data = datam, sum)
      ages <- c(22, 23, 25, 29, 30, 35, 36, 37, 39, 50, 55, 60)
      spending_ages <- c(1563238, 751420, 896338, 858431, 795044, 840966, 866690, 1655969, 785008, 797884, 1630497, 785578)
      plot(x = ages, y = spending_ages, type = "o", main = "Total Spending by Age", 
           xlab = "Age", ylab = "Total Spending", col = "palevioletred4", pch = 16)
    })
    
    output$total_spending_boxplot <- renderPlot({
      boxplot(x = datam$total, main = "Distribution of Total Spending", xlab = "Total Spending")
    })
  })
}

shinyApp(ui = ui, server = server)
