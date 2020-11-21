library(shiny)
library(shinydashboard)
library(tree)
library(ggplot2)


# Load the tree model
load("Tree.RData")

# Load color brewer library
library(RColorBrewer)

# Create a color palette
palette <- brewer.pal(3, "Set2")


# Machine Learning for Predictions
data(iris)
# Set random seed
set.seed(42)

# Randomly sample 100 of 150 row indexes
indexes <- sample(
  x = 1:150,
  size = 100)

# Create a training set from indexes
train <- iris[indexes, ]

# Create a test set from remaining indexes
test <- iris[-indexes, ]

model <- tree(
  formula = Species ~ Petal.Length + Petal.Width,
  data = train)



# UI Code
ui <- dashboardPage(
  dashboardHeader(title = "Iris Dashboard"),
  dashboardSidebar(disable = T),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      
      # Title box
      box(width = 7,
          title = "Iris Species Predictor", status="primary",
          "Drag the sliders to predict the kind of Iris Flower"),
     
       # Interactive graph
      box(plotOutput("plot1", height = 350),
          textOutput("text1"), status = "warning"),
      
      #Sliders for interactive graph
      box(width = 6,
        status = "warning",
        title = "Controls",
        sliderInput(
          inputId = "petal.length",
          label = "Petal Length (cm)",
          min = 1,
          max = 7,
          step = 0.5,
          value = 4),
        sliderInput(
          inputId = "petal.width",
          label = "Petal Width (cm)",
          min = 0.0,
          max = 2.5,
          step = 0.1,
          value = 1.5)),
      ),
    
    # Header and Legend for Density Distribution
    fluidRow(
        box(width = 7,
          title = "Density of Iris Features", status = "primary",
          "Below includes Desnity plots for each of the main Iris Features")
     ),
    
    # Density Distribution Graphs
    fluidRow(
      column(width = 6,
             # Petal Length Density Distribution
             box(
               plotOutput("plot2"), status = "warning"
             ),
             # Petal Width Density Distribution
             box(
               plotOutput("plot3"), status = "warning"
             )),
      
      column(width = 6,
             # Sepal Width Density Distribution
             box(
               plotOutput("plot4"), status = "warning"
             ),
             # Sepal Length Density Distribution
              box(
                plotOutput("plot5"), status = "warning"
            )),
    )
    ),)
  

# Server Code
server <- function(input, output) {
  output$text1 = renderText({
    
    # Create predictors
    predictors <- data.frame(
      Petal.Length = input$petal.length,
      Petal.Width = input$petal.width,
      Sepal.Length = 0,
      Sepal.Length = 0)
    
    # Make prediction
    prediction = predict(
      object = model,
      newdata = predictors,
      type = "class")
    
    # Create prediction text
    paste(
      "The predicted species is: ",
      as.character(prediction))
  })
  
  output$plot1 = renderPlot({
    
    
    # Create a scatterplot colored by species
    plot(
      iris$Petal.Length,
      iris$Petal.Width,
      pch = 19,
      col = palette[as.numeric(iris$Species)],
      main = "Iris Petal Length vs. width",
      xlab = "Petal Length (cm)",
      ylab = "Petal Width (cm)")
    
    # Plot the decision boundaries
    partition.tree(
      model, 
      label = "Species",
      add = TRUE)
    
    # Draw predictor on plot
    points(
      x = input$petal.length,
      y = input$petal.width,
      col = "red",
      pch = 4,
      cex = 2,
      lwd = 2)
  })
  
  # Petal Length Density Distribution
  output$plot2 = renderPlot({
      ggplot(iris, aes(x=Petal.Length, colour=Species, fill=Species)) +
      geom_density(alpha=.3) +
      geom_vline(aes(xintercept=mean(Petal.Length),  colour=Species),linetype="dashed",color="black")+
      xlab("Petal Length (cm)") +  
      ylab("Density")
  })
  
  # Petal Width Density Distribution
  output$plot3 = renderPlot({
    ggplot(iris, aes(x=Petal.Width, colour=Species, fill=Species)) +
      geom_density(alpha=.3) +
      geom_vline(aes(xintercept=mean(Petal.Width),  colour=Species),linetype="dashed",color="black")+
      xlab("Petal Width (cm)") +  
      ylab("Density")
  })
  
  # Sepal Width Density Distribution
  output$plot4 = renderPlot({
    ggplot(iris, aes(x=Sepal.Width, colour=Species, fill=Species)) +
      geom_density(alpha=.3) +
      geom_vline(aes(xintercept=mean(Sepal.Width),  colour=Species), linetype="dashed",color="black")+
      xlab("Sepal Width (cm)") +  
      ylab("Density")
  })
  
  # Sepal Length Density Distribution
  output$plot5 = renderPlot({
    ggplot(iris, aes(x=Sepal.Length, colour=Species, fill=Species)) +
      geom_density(alpha=.3) +
      geom_vline(aes(xintercept=mean(Sepal.Length),  colour=Species),linetype="dashed", color="black")+
      xlab("Sepal Length (cm)") +  
      ylab("Density")
  })
  
}

#Run the App
shinyApp(ui, server)
