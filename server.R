#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(wordcloud)

source("StupidBackoff.R")

# Define server logic required to output a table and a barchart
shinyServer(function(input, output) {
    
 output$table<-renderDataTable({
     par_sen<-input$text
     out<-predict_word(partial_sentence=par_sen)
     out})
 
   output$barchart <- renderPlot({
       par_sen<-input$text
       out<-predict_word(partial_sentence=par_sen)
      ifelse(nrow(out>=5), out2<-out[1:5,], out2<-out)
       levels<-as.character(out2$PredictedWord)
       out2$PredictedWord<-factor(out2$PredictedWord, levels=c(rev(levels)), labels=c(rev(levels)))# making a factor with character levels displayed
       p<-ggplot(data=out2, aes(x=PredictedWord, y=Score))
       p+ geom_bar(stat="identity", fill="blue")+coord_flip()+labs(x = "Predicted Words", y = "Backoff Score")+
           theme_bw()+
           theme(plot.background = element_blank()
                 ,panel.grid.major = element_blank()
                 ,panel.grid.minor = element_blank()
                 ,panel.border = element_blank()
           )+
           theme(axis.line.x = element_line(color="black", size = 1),
                 axis.line.y = element_line(color="black", size = 1),
                 axis.title.x= element_text(face="bold",size=16),
                 axis.title.y= element_text(face="bold",size=16),
                 axis.text.x =element_text(face="bold",size=16),
                 axis.text.y =element_text(face="bold",size=16),
                 legend.position = "none") 
      
     })
   
   wordcloud_rep <- repeatable(wordcloud)
   
   output$wordcloud <- renderPlot({
       par_sen<-input$text
       out<-predict_word(partial_sentence=par_sen)
       out$Score<-as.numeric(as.character(out$Score))
       out$PredictedWord<-as.character(out$PredictedWord)
       wordcloud_rep(words=out$PredictedWord, freq=out$Score,scale=c(8,2),
                      colors=brewer.pal(8, "Dark2"), max.words=100)
   })
  
})


