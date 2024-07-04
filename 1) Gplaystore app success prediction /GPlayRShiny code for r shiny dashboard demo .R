#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

install.packages("shiny")
install.packages("shinydashboard")
install.packages("ggplot2")
install.packages("plotly")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("gtools")
install.packages("data.table")
install.packages("imager")
install.packages("data.table")
install.packages("fastDummies")
install.packages("caret")
install.packages("glmnet")
install.packages("rpart")

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyverse)
library(gtools)
library(data.table)
library(imager)
library(data.table)
library(fastDummies)
library(caret)
library(glmnet)
library(rpart)

#Reading dataframes used for R Shiny
df <- read.csv("~/Desktop/Old sem materials/semester 1 data full/group projects/R project data /mydata_modified 1 (1).csv")

# Define UI for R Shiny App
ui <- dashboardPage(skin = 'purple',
                    dashboardHeader(title = 'Google Play Store App Analyzer', titleWidth = 400
                    ),
                    dashboardSidebar(
                      #Side Panels
                      sidebarMenu(menuItem('Introduction', tabName = 'Introduction'),
                                  menuItem('EDA',tabName = 'EDA'),
                                  menuItem('Prediction',tabName = 'Prediction')
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        #Introduction Page
                        tabItem(tabName = 'Introduction',
                                fluidPage(
                                  titlePanel("Google Play Store App Analyzer"),
                                  fluidRow(
                                    column(7,
                                           HTML("<h4>Welcome to the Google Play Store App Analyzer! In today's highly competitive mobile app market, understanding the dynamics of app success is crucial. With millions of apps available, users have more choices than ever. To stand out and thrive, app developers need data-driven insights.<p>
                                       <h4>Our app provides you with a powerful tool to analyze app statistics and make predictions about an app's potential for success. By focusing on the key factors of app category, pricing, advertisement support, content rating we help you make informed decisions about your app's future.<p>
                                       <p><h3>Key Features:<p>
                                       <p><h4>Data Analysis: We gather and analyze data from the Google Play Store, providing you with a comprehensive overview of app statistics, including app features, ratings, and installs.<h4><p>
                                       <p><h4>Category Insights: Discover which app categories are thriving and which are saturated. We provide visualizations and trends to help you understand the market dynamics.<p>
                                       <p>Success Prediction: Using machine learning algorithms, we predict an app's success within its chosen category with its existing features and pricing. Get insights into whether your app has the potential to become a hit or might need some feature improvements.<p>"),
                                           HTML("<style>
                                        h4 {
                                          margin-bottom : 20px;
                                          color : grey;
                                        }
                                       </style>"),
                                    ),
                                    div(
                                      style = "margin-right = 100px;",
                                      column(5,imageOutput("gimage",width = "100%",height = "650px"))
                                    ))
                                )
                        ),
                        #Ui for Exploratory Data Analysis Tab
                        tabItem(tabName = 'EDA',
                                fluidPage(
                                  titlePanel("Descriptive Statistics"),
                                  #Box Plot
                                  tabsetPanel(
                                    tabPanel("Box Plot",fluidRow(
                                      column(3,
                                             selectInput("var",label = h4("Select x variable"),
                                                         choices = c("Rating","Price","Size_mb","Installs","Rating_Count"))
                                      )),
                                      mainPanel(
                                        div(
                                          style = 'margin-left : 300px',
                                          plotOutput("boxPlotOutput",width = "400px")
                                        )
                                      )),
                                    #Histogram
                                    tabPanel("Histogram",fluidRow(
                                      column(5,
                                             selectInput("x1_var",label = h4("Select x variable:"),
                                                         choices = c("Rating","Price","Size_mb","Installs","Rating_Count")),
                                             sliderInput("bins", "Select bins:", min = 1, max = 1000, value = 50)
                                      )),
                                      mainPanel(
                                        div(
                                          style = 'margin-left : 100px',
                                          plotOutput("histPlotOutput")
                                        )
                                      )),
                                    #Box Plot with Groups
                                    tabPanel("Box Plot with Groups",fluidRow(
                                      column(5,
                                             selectInput("x2_var",label = h4("Select x variable:"),
                                                         choices = c("Rating","Price","Size_mb","Installs","Rating_Count")),
                                             selectInput("y2_var",label = h4("Select categorical variable:"),
                                                         choices = c("Free","In_App_Purchases","Ad_Supported","Editors_Choice","Content_Rating","Category"))
                                             
                                      )),
                                      mainPanel(
                                        plotOutput("boxPlotWithGroupsOutput",width = "1200px")
                                      )),
                                    tabPanel("Frequency of Categories",fluidRow(
                                    ),
                                    mainPanel(
                                      div(
                                        plotOutput("barPlotOutput",width = "1200px", height = "600px")
                                      )
                                    )
                                    ),
                                    #Bar Plot of Free/Paid vs Categories
                                    tabPanel("Bar Plot of Free/Paid vs Categories",fluidRow(
                                      mainPanel(
                                        div(
                                          plotOutput("barPlotOutput2",width = "1200px", height = "600px")
                                        )
                                      )
                                    )),
                                    #Bar Chart of Size Categories
                                    tabPanel("Bar Plot of Price & Size Categories vs Ratings",fluidRow(
                                      mainPanel(
                                        div(
                                          plotOutput("barPlotOutput3",width = "1200px",height = "300px"),
                                          plotOutput("barPlotOutput4",width = "1200px",height = "300px")
                                        )
                                      )
                                    )),
                                    #Pie Chart
                                    tabPanel("Pie Chart",fluidRow(
                                      column(5,plotOutput("donutChart",height = "300px")),
                                      div(
                                        column(5,plotOutput("donutChart2",height = "300px"))
                                      )
                                    ),
                                    fluidRow(
                                      column(5,plotOutput("donutChart3",height = "300px")),
                                      column(5,plotOutput("donutChart4",height = "300px"))
                                    )
                                    )
                                  ))
                        ),
                        tabItem(tabName = 'Prediction',
                                fluidPage(
                                  titlePanel("Predictive Model"),
                                  tabsetPanel(
                                    #Numbers obtained for train and test score by building models in R
                                    tabPanel("Logistic Regression",fluidRow(
                                      div(
                                        style = "margin-left : 50px; margin-right : 50px;",
                                        HTML("<h3>Logistic Regression</h3>"),
                                        HTML("<h4>Logistic Regression is a statistical method used for binary classification, where the goal is to predict one of two possible outcomes, typically represented as 0 or 1. It models the relationship between a dependent variable (the outcome) and one or more independent variables using the logistic function. The logistic function maps any real-valued number to a value between 0 and 1, making it suitable for estimating probabilities. Logistic Regression is widely employed in fields like healthcare (e.g., disease diagnosis), finance (e.g., credit risk assessment), and marketing (e.g., customer churn prediction).We have used a Logistic Regression Model to predict App Success Measure."),
                                        HTML("<h3>Model Statistics"),
                                        HTML("<h4>Number of variables: 53"),
                                        HTML("<h4>Number of records: 50000"),
                                        HTML("<h4>Train Score: 96.43%"),
                                        HTML("<h4>Test Score: 96.57%")
                                      ))),
                                    #Numbers obtained for train and test score by building models in R
                                    tabPanel("Random Forest",fluidRow(
                                      div(
                                        style = "margin-left : 50px; margin-right : 50px;",
                                        HTML("<h3>Random Forest</h3>"),
                                        HTML("<h4>Random Forest is an ensemble learning method that combines multiple Decision Trees to improve predictive accuracy and reduce overfitting. It works by training a multitude of Decision Trees on random subsets of the data and features, and then averaging their predictions.Random Forest is robust, scalable, and effective in handling high-dimensional data. It is commonly used in various applications, including image classification, recommendation systems, and quality control in manufacturing. Its ability to handle complex relationships and feature importance analysis makes it a valuable tool in machine learning."),
                                        HTML("<h3>Model Statistics"),
                                        HTML("<h4>Number of variables: 53"),
                                        HTML("<h4>Number of records: 50000"),
                                        HTML("<h4>Train Score: 96.47%"),
                                        HTML("<h4>Test Score: 96.57%")
                                      )
                                    )),
                                    #Numbers obtained for train and test score by building models in R
                                    tabPanel("Decision Trees",fluidRow(
                                      div(
                                        style = "margin-left : 50px; margin-right : 50px;",
                                        HTML("<h3>Decision Trees</h3>"),
                                        HTML("<h4>Decision Trees are a machine learning algorithm used for both classification and regression tasks. They create a tree-like structure where each internal node represents a decision or a test on an attribute, each branch represents an outcome of that test, and each leaf node represents a class label or a numerical value. Decision Trees are interpretable and can handle both categorical and numerical data. They are used in applications such as customer segmentation, fraud detection, and medical diagnosis."),
                                        HTML("<h3>Model Statistics"),
                                        HTML("<h4>Number of variables: 53"),
                                        HTML("<h4>Number of records: 50000"),
                                        HTML("<h4>Train Score: 96.43%"),
                                        HTML("<h4>Test Score: 96.57%")
                                      )
                                    )),
                                    tabPanel("Predictive Model",fluidRow(
                                      #Inputs taken for Predicting App Success
                                      column(5,
                                             selectInput("logisticx_1",label = h5("Select a category:"),
                                                         choices = unique(df$Category)),
                                             selectInput("logisticx_2",label = h5("Ads supported"),
                                                         choices = c("TRUE","FALSE")),
                                             selectInput("logisticx_3",label = h5("In-app Purchases"),
                                                         choices = c("TRUE","FALSE")),
                                             selectInput("logisticx_4",label = h5("Content Rating"),
                                                         choices = unique(df$Content_Rating)),
                                             sliderInput("logisticx_5", "Select a Price:", min = 0, max = 100, value = 0),
                                             sliderInput("logisticx_6", "Select Size in MB:", min = 0, max = 1000, value = 5)
                                      ),
                                      column(3,
                                             div(
                                               style = "margin-top: 250px",
                                               actionButton("appsuccess","Show App Success"),
                                               HTML("<h4>App Success Measure"),
                                               div(
                                                 verbatimTextOutput("numeric_output"),
                                                 style = "font-size: 60px; margin-top: 15px;"
                                               ))),
                                    )
                                    )))))))


# Define server logic required to draw a histogram
server <- function(input, output,session) {
  observe({
    updateSelectInput(session, "var")
    updateSelectInput(session, "x1_var")
    updateSelectInput(session, "x2_var")
    updateActionButton(session, "appsuccess")
  })
  #Boxplot for single variable
  output$boxPlotOutput <- renderPlot({
    boxplot(df[[input$var]],xlab = input$var,width = 0.3,main = paste("Boxplot of",input$var,sep = " "),col = "blue")
  })
  #Histogram
  output$histPlotOutput <- renderPlot({
    hist(df[[input$x1_var]],xlab = input$x1_var,main = paste("Histogram of",input$x1_var,sep = " "),breaks = input$bins,xlim = c(min(df[[input$x1_var]]),max(df[[input$x1_var]])),col = 'blue')
  })
  #Boxplot with Groups
  output$boxPlotWithGroupsOutput <- renderPlot({
    boxplot(df[[input$x2_var]] ~ df[[input$y2_var]],xlab = input$y2_var,ylab = input$x2_var,main = paste("Boxplot of",input$x2_var,"grouped with",input$y2_var,sep = " "),col = c('green','orange','red','purple','yellow','magenta'))
    
  })
  
  output$barPlotOutput <- renderPlot({
    agg_data <- df %>%
      group_by(Category) %>%
      summarize(Count = n_distinct(App.Id))
    
    
    #Bar Chart for Category Distribution
    # Sort the data by frequency in decreasing order
    agg_data <- agg_data[order(agg_data$Count), ]
    # Create the bar plot with sorted data
    bar_plot <- ggplot(agg_data, aes(x = reorder(Category, -Count), y = Count)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(title = "Frequency of Categories", x = "Category", y = "Frequency") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      geom_text(aes(label = format(Count, big.mark = ",")), vjust = -0.5, size = 2) +
      scale_y_continuous(labels = scales::comma)  # Add commas and adjust font size
    print(bar_plot)
  })
  
  output$barPlotOutput2 <- renderPlot({
    free_true_data <- df %>%
      filter(Free == 'TRUE') %>%
      group_by(Category) %>%
      summarize(Count = n_distinct(App.Id)) %>%
      arrange(desc(Count))
    
    bar_chart_free_true <- ggplot(free_true_data, aes(x = reorder(Category, -Count), y = Count, fill = Count)) +
      geom_bar(stat = "identity") +
      labs(title = "Distribution of Free Apps by Category", x = "Category", y = "# of Apps") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1) ) +
      geom_text(aes(label = format(Count, big.mark = ",")), vjust = -0.5, size = 3,angle=90) +
      scale_y_continuous(labels = scales::comma) +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      guides(fill = FALSE)  # Remove the legend
    print(bar_chart_free_true)# Remove the legend
    
    free_false_data <- df %>%
      filter(Free == 'FALSE') %>%
      group_by(Category) %>%
      summarize(Count = n_distinct(App.Id)) %>%
      arrange(desc(Count))
    
    # Create the bar chart with a gradient within the same color "#009999"
    bar_chart_free_false <- ggplot(free_false_data, aes(x = reorder(Category, -Count), y = Count, fill = Count)) +
      geom_bar(stat = "identity") +
      labs(title = "Distribution of Paid Apps by Category", x = "Category", y = "# of Apps") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1) ) +
      geom_text(aes(label = format(Count, big.mark = ",")), vjust = 0, size = 3,angle=90) +
      scale_y_continuous(labels = scales::comma) +
      scale_fill_gradient(low = "#00FFFF", high = "#009999") +
      guides(fill = FALSE) 
    #print(bar_chart_free_false)# Remove the legend
    
    free_true_data <- data.frame(
      Category = free_true_data$Category,
      Count = free_true_data$Count,
      Free = "True"
    )
    
    #Create a data frame for Free False
    free_false_data <- data.frame(
      Category = free_false_data$Category,
      Count = free_false_data$Count,
      Free = "False"
    )
    
    #Combine the data frames
    combined_data <- rbind(free_true_data, free_false_data)
    
    #Create the combined chart with facet_grid
    combined_chart <- ggplot(combined_data, aes(x = reorder(Category, -Count), y = Count, fill = Count)) +
      geom_bar(stat = "identity") +
      labs(title = "Distribution of Free and Paid Apps by Category", x = "Category", y = "# of Apps") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      geom_text(aes(label = format(Count, big.mark = ",")), vjust = 0.5, size = 3, angle = 90) +
      scale_y_continuous(labels = scales::comma) +
      scale_fill_gradient(low = "#99FFCC", high = "#00994C") +
      facet_grid(Free ~ ., scales = "free")
    
    # Print the combined chart
    print(combined_chart)
  })
  
  output$barPlotOutput3 <- renderPlot({
    df <- df %>%
      mutate(Pricing_Category = case_when(
        Price == 0 ~ "Free",
        Price > 0 & Price <= 100 ~ "Upto 100$",
        Price > 100 & Price <= 300 ~ "Between 100$- 300$",
        Price > 300 ~ "Greater than 300$"
      ))
    
    
    df$Rating <- as.numeric(df$Rating)
    sum(is.na(df$Rating))
    
    average_ratings <- df %>%
      group_by(Pricing_Category) %>%
      summarize(Avg_Rating = ifelse(all(!is.na(Rating)) && all(is.numeric(Rating)), mean(Rating, na.rm = TRUE), NA))
    
    
    
    # Sort the data frame by Avg_Rating in descending order
    average_ratings <- average_ratings %>%
      arrange(desc(Avg_Rating))
    
    
    # Define custom colors based on "Grand Budapest Hotel" theme
    custom_colors <- c(
      "Free" = "#F08D77",
      "Upto 100$" = "#66A182",
      "Less than 300$" = "#D5C295",
      "Greater than 300$" = "#AE8A6D"
    )
    
    
    
    bar_average_ratings_by_price <- ggplot(data = average_ratings, aes(x = Pricing_Category, y = Avg_Rating, fill = Pricing_Category)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = custom_colors) +
      labs(x = "Pricing Category", y = "Average Rating") +
      ggtitle("Average Rating by Pricing Category") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(bar_average_ratings_by_price)
  })
  
  df <- df %>%
    mutate(Size_Category = case_when(
      Size_mb >= 0 & Size_mb <= 100 ~ "Small",
      Size_mb > 100 & Size_mb <= 500 ~ "Medium",
      Size_mb > 500 & Size_mb <= 1000 ~ "Large",
      Size_mb > 1000 ~ "Extremely Large",
      TRUE ~ "Unknown"  # Add this line for handling values outside the specified ranges
    ))
  
  
  output$barPlotOutput4 <- renderPlot({
    average_ratings_by_size_category <- df %>%
      group_by(Size_Category) %>%
      summarize(Avg_Rating = mean(Rating, na.rm = TRUE))
    
    
    # Create a bar plot with custom colors
    custom_colors_1 <- c(
      "Small" = "#CA2C92",
      "Medium" = "#E97E77",
      "Large" = "#91D9A9",
      "Extremely Large" = "#624C30"
    )
    
    
    bar_average_ratings_by_size <- ggplot(data = average_ratings_by_size_category, aes(x = reorder(Size_Category, -Avg_Rating), y = Avg_Rating, fill = Size_Category)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = custom_colors_1) +
      labs(x = "Size Category", y = "Average Rating") +
      ggtitle("Average Rating by Size Category") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(bar_average_ratings_by_size)
  })
  
  output$donutChart <- renderPlot({
    #Create Pies for Overall Distribution
    # Filter the dataset for In.App.Purchases and aggregate data
    in_app_purchases_data <- df %>%
      group_by(In_App_Purchases) %>%
      summarize(Count = n_distinct(App.Id))
    #print(in_app_purchases_data)
    
    # Create the donut chart
    donut_chart_in_app_purchases <- ggplot(in_app_purchases_data, aes(x = "", y = Count, fill = In_App_Purchases)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      theme_void() +
      scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "orange")) +
      labs(title = "Pie Chart of In App Purchases")
    
    print(donut_chart_in_app_purchases)
  })
  
  output$donutChart2 <- renderPlot({
    #Create Pies for Overall Distribution
    # Filter the dataset for In.App.Purchases and aggregate data
    adsupported_data <- df %>%
      group_by(Ad_Supported) %>%
      summarize(Count = n_distinct(App.Id))
    #print(adsupported_data)
    
    # Create the pie chart
    donut_chart_adsupported <- ggplot(adsupported_data, aes(x = "", y = Count, fill = Ad_Supported)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      theme_void() +
      scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "orange")) +
      labs(title = "Pie Chart of Ads Supported")
    
    print(donut_chart_adsupported)
  })
  
  output$donutChart3 <- renderPlot({
    #Create Donuts for Overall Distribution
    # Filter the dataset for In.App.Purchases and aggregate data
    contentrating_data <- df %>%
      group_by(Content_Rating) %>%
      summarize(Count = n_distinct(App.Id))
    
    # Create the donut chart
    donut_chart_contentrating <- ggplot(contentrating_data, aes(x = "", y = Count, fill = Content_Rating)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      theme_void() +
      scale_fill_manual(values = c("Everyone" = "darkgreen", "Teen" = "orange","Mature 17+" = "red","Adults only 18+" = "blue","Everyone 10+" = "purple","Unrated" = "yellow")) +
      labs(title = "Pie Chart of Content Rating")
    
    print(donut_chart_contentrating)
  })
  
  output$donutChart4 <- renderPlot({
    #Create Donuts for Overall Distribution
    # Filter the dataset for In.App.Purchases and aggregate data
    free_data <- df %>%
      group_by(Free) %>%
      summarize(Count = n_distinct(App.Id))
    
    # Create the donut chart
    
    donut_chart_free <- ggplot(free_data, aes(x = 1,y = Count, fill = Free)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      theme_void() +
      scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "orange")) +
      labs(title = "Pie Chart of Free and Paid Apps")
    
    #donut_chart_free <- ggdonutchart(free_data, Count, label = "")
    
    print(donut_chart_free)
  })
  output$gimage <- renderImage({
    list(src = "C:/Users/DELL/Downloads/googleplayimage.png")
  },deleteFile = F)
  
  #Output for App Success Measure calculated using Logistic Regression Model
  output$numeric_output <- renderText({
    req(input$appsuccess)
    
    if(input$logisticx_5 == 0){
      free_var <- 1
    }
    else {
      free_var <- 0
    }
    #Computed dummy variables for the new input data using existing Datafram used for EDA
    mydata_v2 <- data.frame(Price = input$logisticx_5, Size_mb = input$logisticx_6,Free = free_var)
    mydata_v2_cat <- data.frame(Category = input$logisticx_1, Ad.supported = input$logisticx_2,In.App.Purchases = input$logisticx_3,Content.Rating = input$logisticx_4)
    
    df$Category[1048575] <- mydata_v2_cat$Category
    df$Ad_Supported[1048575] <- mydata_v2_cat$Ad.supported
    df$In_App_Purchases[1048575] <- mydata_v2_cat$In.App.Purchases
    df$Content_Rating[1048575] <- mydata_v2_cat$Content.Rating
    
    #Creating Dummy Variables for Categorical Data
    dummies_Content.Rating <- dummy_cols(df$Content_Rating,remove_first_dummy = TRUE)
    dummies_In.App.Purchases <- dummy_cols(df$In_App_Purchases,remove_first_dummy = TRUE)
    dummies_Category <- dummy_cols(df$Category,remove_first_dummy = TRUE)
    dummies_Ad.Supported <- dummy_cols(df$Ad_Supported,remove_first_dummy = TRUE)

    #colnames(dummies_Free)[2] <- "Free"
    colnames(dummies_In.App.Purchases)[2] <- "InAppPurchases_True"
    colnames(dummies_Ad.Supported)[2] <- "AdSupported_True"
    
    
    #Creating dataframe that includes old dataset with new dummy variables
    mydata_v3<-cbind(mydata_v2,dummies_Content.Rating,
                     dummies_In.App.Purchases,dummies_Category,dummies_Ad.Supported)
    
    # Assuming 'mydata_v3' is your data frame and 'i' is the index of the variable you want to rename
    colnames(mydata_v3)[colnames(mydata_v3) == '.data_Everyone'] <- 'ContentRated_Everyone'
    colnames(mydata_v3)[colnames(mydata_v3) == '.data_Everyone 10+'] <- 'ContentRated_10Plus'
    colnames(mydata_v3)[colnames(mydata_v3) == '.data_Mature 17+'] <- 'ContentRated_17Plus'
    colnames(mydata_v3)[colnames(mydata_v3) == '.data_Teen'] <- 'ContentRated_Teen'
    colnames(mydata_v3)[colnames(mydata_v3) == '.data_Unrated'] <- 'ContentRated_Unrated'
    colnames(mydata_v3)[colnames(mydata_v3) == '.data_Adventure'] <- 'Category_Adventure'
    colnames(mydata_v3)[colnames(mydata_v3) == '.data_Video Players & Editors'] <- 'Category_Video'
    colnames(mydata_v3)[colnames(mydata_v3) == '.data_Communication'] <- 'Category_Communication'
    colnames(mydata_v3)[colnames(mydata_v3) == '.data_Productivity'] <- 'Category_Productivity'
    colnames(mydata_v3)[colnames(mydata_v3) == '.data_Social'] <- 'Category_Social'
    colnames(mydata_v3)[colnames(mydata_v3) == '.data_Books & Reference'] <- 'Category_Books'
    colnames(mydata_v3)[colnames(mydata_v3) == '.data_Photography'] <- 'Category_Photography'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Travel & Local'] <- 'Category_Travel'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Arcade'] <- 'Category_Arcade'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Tools'] <- 'Category_Tools'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Entertainment'] <- 'Category_Entertainment'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_News & Magazines'] <- 'Category_News'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Health & Fitness'] <- 'Category_Health'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Casual'] <- 'Category_Casual'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Action'] <- 'Category_Action'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Personalization'] <- 'Category_Personalization'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Strategy'] <- 'Category_Strategy'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Lifestyle'] <- 'Category_Lifestyle'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Shopping'] <- 'Category_Shopping'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Adventure'] <- 'Category_Adventure'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Finance'] <- 'Category_Finance'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Music & Audio'] <- 'Category_Music'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Racing'] <- 'Category_Racing'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Puzzle'] <- 'Category_Puzzle'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Board'] <- 'Category_Board'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Education'] <- 'Category_Education'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Simulation'] <- 'Category_Simulation'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Sports'] <- 'Category_Sports'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Business'] <- 'Category_Business'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Role Playing'] <- 'Category_RolePlaying'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Food & Drink'] <- 'Category_Food'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Card'] <- 'Category_Card'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Weather'] <- 'Category_Weather'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Maps & Navigation'] <- 'Category_Maps'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Art & Design'] <- 'Category_Art'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Word'] <- 'Category_Word'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Casino'] <- 'Category_Casino'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Libraries & Demo'] <- 'Category_Library'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Trivia'] <- 'Category_Trivia'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Beauty'] <- 'Category_Beauty'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_House & Home'] <- 'Category_House'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Dating'] <- 'Category_Dating'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Parenting'] <- 'Category_Parenting'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Auto & Vehicles'] <- 'Category_Vehicles'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Medical'] <- 'Category_Medical'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Comics'] <- 'Category_Comics'
    colnames(mydata_v3)[colnames(mydata_v3) ==  '.data_Events'] <- 'Category_Events' 
    
    mydata_v4 <- mydata_v3 %>%
      select(-contains(".data"))
    #Dropping features for ContentRated_Everyone & ContentRated_Others based on Multicollinearlity
    mydata_v4 <- mydata_v4 %>%
      select(-c(4,8))
    X_test <- mydata_v4[1048575,]
    
    X_test <- data.frame(X_test)
    set.seed(12)
      
      # Read in  the logistic regression model
    logistic_model <- readRDS("logistic_model (1).rds")

    #Predict App Success Probability
    y_test_pred_lr <- predict(logistic_model, newdata = X_test, type = "response")

    #Predict App Success      
    y_test_pred_binary_lr <- ifelse(y_test_pred_lr > 0.5, 1, 0)
    if(y_test_pred_binary_lr == 1)
        paste("Success")
    else paste("Failure")

  })
  library(stats)
}


# Run the application 
shinyApp(ui = ui, server = server)
