#IRIS DATA COMPUTATION
data(iris)
# log transform
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]

# apply PCA and the variance
ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
pca_sig.data<-data.frame(PC1=ir.pca$x[,1], PC2=ir.pca$x[,2], PC3=ir.pca$x[,3], PC4=ir.pca$x[,4], Species=ir.species)
pca_sig.var<-ir.pca$sdev^2
pca_sig.var.per<-round(pca_sig.var/sum(pca_sig.var)*100, 1)
pca_sig.var.per<- data.frame(pca_explained_variance_percentage=pca_sig.var.per, row.names=c('PCA1','PCA2','PCA3','PCA4'))

#SHINY
library(shiny)
library(ggplot2)
ui <- fluidPage(
  headerPanel('PCA: Choose X and Y:'),
  sidebarPanel(selectInput('xInput', 'X Axis', names(pca_sig.data[,1:4])),
               selectInput('yInput', 'Y Axis', names(pca_sig.data[,1:4]),
               selected=names(pca_sig.data)[[2]])
  ),
  mainPanel(plotOutput('pca_plot'), tableOutput('pca_var_table'))
)
server <- function(input, output){
  pca_Data <- reactive({
    pca_sig.data[c(input$xInput, input$yInput, 'Species')]
  })

  output$pca_plot <- renderPlot({
    ggplot(pca_Data(),aes(pca_Data()[,1],pca_Data()[,2],colour=pca_Data()[,3]))+
    geom_point(size=3)+xlab(input$xInput)+ylab(input$yInput)+labs(colour='Species', title="PCA analysis plot")+
                              theme(plot.title = element_text(color="red", size=30, face="bold.italic"),
                                    axis.title.x = element_text(color="blue", size=20, face="bold"),
                                    axis.title.y = element_text(color="blue", size=20, face="bold"), 
                                    legend.position = "top", legend.title=element_text(size=15), 
                                    legend.text=element_text(size=15))
                              
  })
  output$pca_var_table <- renderTable(pca_sig.var.per, rownames=T, colnames=T, 
                                      caption="The percentage of variances for the PCA:", 
                                      caption.placement="top")
}
shinyApp(ui=ui, server=server)
