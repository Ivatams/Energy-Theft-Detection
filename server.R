library(shiny)
library(stats)
library(caret)
library(randomForest)
library(DT)
library(tidyverse)
# load the trained final predictive model.
#con <- url("http://michaelchen1004.github.io/ChurnPrediction/churnModel.Rdata")
#load(con)   
shinyServer(function(input, output) {
    
    # retrieving the customer information through the uploaded csv file.
    churnDF <- reactive({
        inFile <- input$file1
        if (is.null(inFile)) return(NULL)
        else read.csv(inFile$datapath, header = T)
    })

    
    modelCreation <- reactive({readRDS("logmodel.rds")})    
    
    # retrieving the customer information through the manually added data.
    churnDF2 <- reactive({
        input$submit
        isolate({
                custDf <- data.frame(Average.Consumption = input$Average.Consumption,
                                     Availability = input$Availability,
                                     Estimated.Connected.Load = input$Estimated.Connected.Load,
                                     group_cons =   input$group_cons,
                                     MeterBypass =  input$MeterBypass,
                                     FeederBypass = input$FeederBypass,
                                     TariffBypass = input$TariffBypass)
                # write manually input data into a file for further retrieving.
                write.table(custDf, file = "churnList.csv", append = T, sep = ",", 
                            row.names = F, col.names = F)
                # retrieve the data from the file.
                if (file.exists("churnList.csv")) {
                    custDf <- read.csv("churnList.csv", header = F)  
                    # For manually input data, read from the saved file to render table.
                    names(custDf) <- c("Average.Consumption", "Availability",
                                       "Estimated.Connected.Load", "group_cons",
                                       "MeterBypass", "FeederBypass", "TariffBypass")
                    return(custDf)
                } else return(NULL)
        })
    })
    
    
     # display the data from the csv file.
     output$content <- DT::renderDataTable(datatable(churnDF(), 
                                           options = list(searching = FALSE,pageLength = 5,
                                                          lengthMenu = c(5, 10, 15, 20), scrollX = T)))
     
    # display the data from manually input.
    output$content2 <- renderDataTable({
        input$submit
        isolate(churnDF2())
        }, options = list(searching = FALSE,pageLength = 5,
                          lengthMenu = c(5, 10, 15, 20), scrollX = T,
                          autoWidth = TRUE,
                          columnDefs = list(list(width = '20px', targets = "_all"))))
    
    # predicting the churn using the ML model for the displayed customer accounts.
    churnResult <- reactive({
        input$pred
        isolate({
            churnDf <- churnDF()
            churnDf2 <- churnDF2()
            
            if (!is.null(churnDf)) {
               pred <- predict(modelCreation(), newdata = churnDf, type = 'response')
               return(cbind.data.frame(churnDf, as.data.frame(pred)))
                # if (sum(pred == 1) == 0) return(NULL)  # check if exists churn customers
                # else churnDf[pred == 1, ]
            } else if (!is.null(churnDf2)) {
                pred <- predict(modelCreation(), newdata = churnDf2, type = 'response')
                return(cbind.data.frame(churnDf2, as.data.frame(pred)))
            } else return()
        })
    })
    
    

    # display the predicted accounts which have potential churn probabilities.
    output$result <- DT::renderDataTable(datatable(churnResult() %>% 
                                             
                                             mutate(pred = round(pred, 2)) %>% 
                                             mutate(Status = ifelse(pred <= 0.5, "Non Bypasser",
                                                                    "ByePasser")) %>% 
                                             select(9, 8, everything()), 
                                             options = list(searching = T,pageLength = 5,
                                                            autoWidth = T,
                                                            lengthMenu = c(5, 10, 15, 20), scrollX = T)) %>% 
                                             formatStyle(
                                                 'Status',
                                                 backgroundColor = styleEqual(c("Non Bypasser", "ByePasser"), c('gray', 'Red'))))
    
    output$text1 <-renderText({
        if (input$pred == 0) return()
        if (is.null(churnResult())) "No potentially bypass customers in this list!"
    })
    
    # download the predicted results
    output$dnld <- downloadHandler(filename = "churnList.csv", 
                                   content = function(file) {
                                       write.csv(churnResult(), file)
                                       })
})