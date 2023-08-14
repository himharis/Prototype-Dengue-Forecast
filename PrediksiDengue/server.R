library(shiny)
# library(shinylogs)

# setwd("~/Downloads/dengue phlc/PrediksiDengue")

shinyServer(function(input, output){
  prediction <- reactive({
    pred <- predict(readRDS("dengue_svr.rds"), newdata = data.frame(
      Tavg = c(as.numeric(input$Tavg)),
      RH_avg = c(as.numeric(input$RH_avg)),
      RR = c(as.numeric(input$RR)),
      ss = c(as.numeric(input$ss)),
      ff_avg = c(as.numeric(input$ff_avg)),
      ABJ = c(as.factor(input$ABJ))
    ))
    return(pred)
  })
  output$userOutput <-renderText(
    paste("Dari data input, hasil prediksi kasus adalah:",round(prediction(),0))
  )
  # output$table1 <-renderTable(
  #   if(input$dropdown != "All") {
  #   USArrests[rownames(USArrests) == input$dropdown, ]
  #   } else {
  #     USArrests[USArrests$Murder >= input$slider1[1] &
  #                 USArrests$Murder <= input$slider1[2],]
  #   }
  # output$plotOutput <- renderPlot(
  #   hist(USArrests[,input$kolompilih],
  #        main = paste("Histogram of", input$kolompilih),
  #        breaks = input$binpilih)
  # )
})
# track_usage(storage_mode = store_json(path = "logs/"))
