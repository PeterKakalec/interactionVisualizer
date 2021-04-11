library(shiny)
library(ggplot2)
library(gridExtra)

makeDF <- function(xSig,modSig,intSig,cent){
    #Generating data
    mod <- c(rep(0,50),rep(1,50))
    x <- rnorm(100,1,1)
    
    #Centering if indicated
    if(cent==TRUE){
        x<-x-mean(x)
    }
    
    #Generating xm term
    xm <- x*mod
    
    #Setting coefficients for y variable construction
    if (xSig==TRUE){
        xMult<-1
    }else{
        xMult<-0    
    }
    if (modSig==TRUE){
        modMult<-1
    }else{
        modMult<-0
    }
    if (intSig==TRUE){
        intMult<-1
    }else{
        intMult<-0
    }
    
    #Generating Y value
    y <- (xMult*x)+(modMult*mod)+(intMult*(xm))+rnorm(100,0,1)
    
    #Creating residuals
    y.res <- lm(y~x+mod)$residuals

    #Storing values in a data frame and returning
    df <- data.frame(x,mod,xm,y,y.res)
    return(df)
}

ui <- fluidPage(
    titlePanel("Interaction Plotter"),
    sidebarLayout(
        sidebarPanel(
            checkboxInput("xSig", "X Significant", value = FALSE, width = NULL),
            checkboxInput("modSig", "Moderator Significant", value = FALSE, width = NULL),
            checkboxInput("intSig", "Interaction Significant", value = FALSE, width = NULL),
            checkboxInput("cent", "Centered", value = FALSE, width = NULL)
            ),
        mainPanel(
           plotOutput("intPlot"),
           DT::dataTableOutput("modelSummary"),
        )
    )
)
server <- function(input, output) {
    #Creating data frame from inputs
    myDF <- reactive({
        makeDF(input$xSig,input$modSig,input$intSig,input$cent)
    })
    
    #Creating LM object from inputs
    myLM <- reactive({
        lm(y~x+mod+xm, data=myDF())
    })
    
    #Making plots
    output$intPlot <- renderPlot({
        dat <- myDF()
        plot1 <- ggplot(data=dat,aes(x=x,y=y,color=as.factor(mod)))+
             geom_point()+
             geom_smooth(method="lm")+
             theme_classic()+
             scale_color_manual(values=c("Blue","Red"))+
             geom_hline(yintercept=0,linetype=2)+
             geom_vline(xintercept=0,linetype=2)+
             ggtitle("Y by X by Moderator Type")+
             xlab("X")+
             ylab("Y")+
             labs(color="Moderator")
          plot2 <- ggplot(data=dat,aes(x=x,y=y.res,color=as.factor(mod)))+
             geom_point()+
             geom_smooth(method="lm")+
             theme_classic()+
             scale_color_manual(values=c("Blue","Red"))+
             geom_hline(yintercept=0,linetype=2)+
             geom_vline(xintercept=0,linetype=2)+
             ggtitle("Y Residuals by X by Moderator Type")+
             xlab("X")+
             ylab("Y Residual")+
             labs(color="Moderator")
         grid.arrange(plot1,plot2,nrow=1)
    })
    
    #Output of model summary
    output$modelSummary <- DT::renderDataTable({
        round(as.data.frame(summary(myLM())$coefficients),3)
    }, options = list(dom = 't'))
}
shinyApp(ui = ui, server = server)
