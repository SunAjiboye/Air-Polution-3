# Load the data
data <- read.csv("C:/Users/sunaj/OneDrive/Documents/Albany_Airbnb_data.csv", stringsAsFactors = FALSE)

# Safe type conversion
data$id <- as.numeric(data$id)
data$host_id <- as.numeric(data$host_id)
data$price <- as.numeric(data$price)
data$minimum_nights <- as.integer(as.numeric(data$minimum_nights))
data$number_of_reviews <- as.integer(as.numeric(data$number_of_reviews))
data$availability_365 <- as.integer(as.numeric(data$availability_365))
data$number_of_reviews_ltm <- as.integer(as.numeric(data$number_of_reviews_ltm))

# Filter using base R subset()
data <- subset(data, 
               (price >= 0 | is.na(price)) &
                 (minimum_nights >= 1 | is.na(minimum_nights)) &
                 (number_of_reviews >= 0 | is.na(number_of_reviews)) &
                 (availability_365 >= 0 & availability_365 <= 365 | is.na(availability_365)) &
                 (number_of_reviews_ltm >= 0 | is.na(number_of_reviews_ltm)))

# Verify
str(data)
head(data)



# Save the cleaned data (optional)
write.csv(data, "C:/Users/sunaj/OneDrive/Documents/Albany_Airbnb_data_cleaned.csv", row.names = FALSE)
print("Cleaned data saved as 'Albany_Airbnb_data_cleaned.csv'")
new_data <- ("Cleaned data saved as 'Albany_Airbnb_data_cleaned.csv'")
head(new_data)
summary(new_data)




library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Albany Airbnb data"),
  
  sidebarLayout(
    sidebarPanel(
      shiny::sliderInput("bins", "Number of bins:", min = 5, max = 50, value = 25),
      shiny::selectInput("color", "Choose a color:", choices = c("Blue" = "#007bc2", "Red" = "#c20000", "Green" = "#00c244")),
      shiny::selectInput("theme", "Choose a theme:", choices = c("Classic", "Minimal", "Dark"))
    ),
    
    mainPanel(
      plotOutput("distPlot"),
      plotOutput("densityPlot"),
      plotOutput("boxPlot"),
      plotOutput("scatterPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  theme_choice <- reactive({
    switch(input$theme,
           "Classic" = theme_classic(),
           "Minimal" = theme_minimal(),
           "Dark" = theme_dark())
  })
  
  output$distPlot <- renderPlot({
    x <- data$price
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    ggplot(data.frame(x), aes(x)) +
      geom_histogram(breaks = bins, fill = input$color, color = "white") +
      labs(x = "Price", y = "Availability_365") +
      theme_choice()
  })
  
  output$densityPlot <- renderPlot({
    x <- data$price
    
    ggplot(data.frame(x), aes(x)) +
      geom_density(fill = input$color, alpha = 0.5) +
      labs(title = "Density Plot of Availability_365", x = "Availability_365", y = "Density") +
      theme_choice()
  })
  
  output$boxPlot <- renderPlot({
    x <- data$price
    
    ggplot(data.frame(x), aes(y = x)) +
      geom_boxplot(fill = input$color, color = "black") +
      labs(title = "Boxplot of Availability_365", y = "Availability_365") +
      theme_choice()
  })
  
  output$scatterPlot <- renderPlot({
    ggplot(data$price, aes(x = price, y = availability_365)) +
      geom_point(color = input$color) +
      labs(title = "Price vs Availability_365", x = "Price", y = "Availability_365") +
      theme_choice()
  })
}

# Run the application
shinyApp(ui, server, options = list(display.mode = "showcase"))

