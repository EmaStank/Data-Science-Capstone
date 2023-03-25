library(shiny)

ui <- fluidPage(
  verticalLayout(
    titlePanel("Space X Launch Records Dashboard"),
    wellPanel(
      selectInput('dropdown', label = list('All Sites','CCAFS LC-40','CCAFS SLC-40','KSC LC-39A','VAFB SLC-4E'), 
                  choices = list('All Sites','CCAFS LC-40','CCAFS SLC-40','KSC LC-39A','VAFB SLC-4E'),
                  selected = 'All Sites')
    ),
    plotOutput('pie_chart'),
    wellPanel(
      sliderInput('slider', "Range:", min = 0, max = 10000, 
                  value = c(2000, 7000), step = 1000)
    ),
    plotOutput('scatter_plot')
  )
)



server <- function(input, output){
  output$pie_chart <- renderPlot({
    if (input$dropdown == 'All Sites'){
      labels <- round(table(DataXLS$`Launch Site`)/nrow(DataXLS)*100,2)
      labels <- paste(labels, '%', sep = "")
      pie(table(DataXLS$`Launch Site`), labels = labels,
          main = "Total Success Launches by Sites",
          col = c('darkred','blue3','darkturquoise','darkorchid3'))
      legend(x = 'topright', legend = c('CCAFS LC-40','CCAFS SLC-40','KSC LC-39A','VAFB SLC-4E'),
             fill = c('darkred','blue3','darkturquoise','darkorchid3'), cex = 1.1)
    }else{
      df <- DataXLS[DataXLS$`Launch Site` == input$dropdown,]
      x <- df$class
      labels <- round(table(df$class)/length(x)*100,2)
      labels <- paste(labels, '%', sep = "")
      pie(x = table(x), labels = labels,
          main = "Total Success Launches by Sites",
          col = c('darkred','blue3'))
      legend(x = 'topright', legend = c('Failure','Success'),
             fill = c('darkred','blue3'), cex = 1.1)
    }
  })
  output$scatter_plot <- renderPlot({
    if(input$dropdown == 'All Sites'){
      df <- DataXLS[DataXLS$`Payload Mass (kg)`>= input$slider[1] & DataXLS$`Payload Mass (kg)`< input$slider[2],]
      x <- df$`Payload Mass (kg)`
      y <- df$class
      colors <- c('darkred','blue3','darkturquoise','darkorchid3','darkgoldenrod1')
      plot(x = x, y = y, main = "Correlation between Payload and Success for all Sites",
           col = colors[factor(df$`Booster Version Category`)], xlab = "Payload Mass (kg)",
           ylab = "class")
      legend('topright', legend = unique(df$`Booster Version Category`), cex = 1.1,
             fill = colors)
    }else{
      df <- DataXLS[DataXLS$`Payload Mass (kg)`>= input$slider[1] & DataXLS$`Payload Mass (kg)`< input$slider[2],]
      df <- df[df$`Launch Site` == input$dropdown,]
      x <- df$`Payload Mass (kg)`
      y <- df$class
      colors <- c('darkred','blue3','darkturquoise','darkorchid3','darkgoldenrod1')
      plot(x = x, y = y, main = "Correlation between Payload and Success for all Sites",
           col = colors[factor(df$`Booster Version Category`)], xlab = "Payload Mass (kg)",
           ylab = "class")
      legend('topright', legend = unique(df$`Booster Version Category`), cex = 1.1,
             fill = colors)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
