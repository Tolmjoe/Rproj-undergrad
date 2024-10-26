library(shiny)
library(remotes)
library(rsconnect)
library(shinydashboard)
library(flexsurv)
library(fitdistrplus)
library(ggplot2)
library(hypergeo)
library(DT)
library(utf8)

options(max.print = 1000000)



head <-dashboardHeader(title="TRANSFORMATION OF FISK DISTRIBUTION",titleWidth = 470)
side <- dashboardSidebar(width="257px",
                         sidebarMenu(
                           menuItem("PROJECT",tabName = "intro",startExpanded=TRUE,icon = icon("home"),
                                    menuSubItem("Introduction",tabName = "data",icon = icon("envelope")),menuSubItem("Aim and Objectives",tabName = "aim",icon=icon("pen-nib")),
                                    menuSubItem("Literature Review",tabName = "lit",icon=icon("book")),
                                    menuSubItem("Methodology",tabName = "met",icon=icon("layer-group"))),
                           menuItem("Distribution plots",tabName = "plot", badgeLabel = "Base & transformed",selected = TRUE),
                           menuItem("Data Club",tabName = "club"),
                           menuItem("Characteristics function",tabName="cf"),
                           menuItem("Maximum Likelihood Estimation",tabName = "mle"),
                           menuItem("Fit distribution to extract best model",badgeLabel="Real life dataset",badgeColor="maroon",tabName="fita")
                           
                           
                           
                           
                           
                           
                           
                         )
)
body <- dashboardBody(
  fluidRow(
    tags$head(tags$link(rel="stylesheet",type="text/css",href="des.css")),
    tabItems(
      tabItem(tabName = "aim",box(width=12,tags$p(style="font-size:150%;font-family:courier","The aim of this study is to trasmutate,transform,extend or generalize known distributions to derive new models
                              that are more flexible in modelling real life problems.",tags$br(),tags$br(),
                                                  "The Objectives of this study are as follows:
                              
                              
                              
                              
                              
                              
                              
                              "))),
      tabItem(tabName="data",box(width=12,tags$p(style="font-size:140%;font-family:courier","A new continuous probability distribution which enhances and extends the Fisk distribution by employing the Alpha power transformation, the Beta transformation and the
transformed transformer (T-X) technique is proposed and studied. To a significant degree,
the new distribution is very flexible in analyzing positive data in hydrology, biostatistics
and economics. Various structural properties of the new distribution are derived, including explicit expressions for the moments, characteristics function, survival function, hazard
function and quantile function. The flexibility of the new density function is illustrated
by means of an application to a real finance dataset and the new distributions are finally
compared and weighed to obtain the best alternative distribution to model real finance
data.",tags$br(),tags$br(),
                                                 
                                                 "The Fisk distribution, also commonly called the Log-logistic distribution is a continuous
probability distribution with extensive applications in areas such as finance, reliability and
life-testing experiments, actuarial science, survival analysis, stream flow rates in hydrology
and engineering. The Fisk distribution is among the class of survival time parametric
models where the hazard rate initially increases and then decreases, hence its hump-shape.
It is a good substitute for the Weibull distribution and Log-normal distribution since it characterizes an increasing hazard rate function. Fisk distribution is in fact a mixture of
Gompertz distribution and Gamma distribution with the value of the mean and the variance
equal to one",tags$br(),tags$br(), "A real-life dataset on Nigeria's foreign debt is used to show the ï¬‚exibility of the new models compared to the base distribution (FISK DISTRIBUTION)"
      ))
      
      
      
      
      
      
      
      
      ),
      tabItem(tabName = "lit",box(width=12,tags$p(style="font-size:140%;font-family:courier","Many statistical approaches in literature were substantiated by numerous continuous univariate distributions. This has motivated the need to create new distributions that are
more flexible and effective in modeling real life data. The generator approach is a common method used in defining extended forms of distributions, it introduces one or more
additional shape parameter to the baseline distribution which helps to improve the tail
properties and the goodness of fit of the model. The beta-G(Eugene et al.,2002; Jones,
2004), Kumaraswamy-G (Cordeiro and de Castro, 2011), and gamma-G (type 3) (Torabi
and Montazari, 2014) are common examples of this approach. Other approaches like perturbation approach (Azzalini and Capitanio, 2003) and two piece approach (Hansen, 1994)
have also been used extensively to create extended forms of well-known distributions")      
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
      )), 
      tabItem(tabName = "met",box(width=12,"Methodology")),
      tabItem(tabName="plot",box(width=12,title=(icon=icon("pen")),status="danger",solidHeader = TRUE,selectInput("disto","Select Distribution",selectize = FALSE,
                                                                                                                  c("Base distribution",
                                                                                                                    "Alpha power transformed fisk distribution","Beta fisk distribution","T-X fisk distribution")),
                                 selectInput("plut","Choose plot",selectize=FALSE,c("PDF","CDF","Quantile function","Hazard function",
                                                                                    "Cumulative hazard function","Random deviates")),
                                 numericInput("alphaa","Input Alpha",value="1",min=1),numericInput("betaa","Input Beta",value="1",min=1),
                                 numericInput("lambda","Input lambda",value="2",min=2),splitLayout(numericInput("a","Beta(_,b)",value=1,min=1),
                                                                                                   numericInput("b","Beta(a,_)",value=1,min=1))),
              
              box(width=12,plotOutput("ddd"),status="warning"),
              box(sliderInput("axis","Specify x axis",value=c(0,4),min=0,max=300,step=0.5,width=900))
              
              
              
              
              
              
              
              
              
      ),
      tabItem(tabName = "cf",box("In development")
              
              
              
              
              
              
              
              
      ),
      tabItem(tabName="fita",box(title="Application of MLE",width=12,status = "success",solidHeader = TRUE,tags$h4("This analysis uses the data on Total
        External Public Debt (USD) of Nigeria from the year 1980 to 2022. The data is used because Fisk distribution
        can be used to model the distribution of wealth or income or finances. The data is fitted using Fisk distribution, Alpha power transformed fisk distribution,
        Beta fisk distribution, and Fisk-Uniform family of distribution, and the maximum likelihood estimation and goodness of fit tests
        is used to choose which among the distributions is the better one, and further prove that extending,transforming or
        generalizing the fisk distribution can produce distributions that are more flexible in modelling
        real life applications of the Fisk distribution"), tags$i("Source of data:"), tags$b("African Development Bank Group")),
              box(width=12,selectInput("mod","Analysis",c("Compute","Input comma seperated data(SID)")),tags$br()),
              box(width=12,verbatimTextOutput("har"),verbatimTextOutput("har0"),verbatimTextOutput("har1"),verbatimTextOutput("har2"),
                  box(width=12,title="Fisk distribution",plotOutput("hare")),box(width=12,title="Alpha power transformed fisk distribution",plotOutput("haro")))
              
              
              
              
              
              
              
              
              
      ),
      tabItem(tabName = "club",
              box(width=12,
                  fileInput("upload", "Choose CSV File",
                            multiple = FALSE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),
                  downloadButton("download")
              ),
              box(width=12,
                  DT::dataTableOutput('x1'),
                  verbatimTextOutput("print")
              )  
              
              
              
      ),  
      tabItem(tabName="mle", 
              box(width=12,title=(icon=icon("pen")),status="warning",solidHeader = TRUE,selectInput("dist","Select Distribution",selectize = FALSE,c("Base distribution",
                                                                                                                                                     "Alpha power transformed fisk distribution","Beta fisk distribution","T-X fisk distribution")),
                  splitLayout(numericInput("alpha","Init. Alpha",value="1",min=1),numericInput("beta","Initi. Beta",value="1",min=1),
                              numericInput("lambda","Initi. Lamda",value="2",min=2))),box(width=12,tableOutput("maxle"),status="danger"),
              plotOutput("fito")
              
              
              
              
              
      )
      
    ))  
)

ui <- dashboardPage(head,side,body)

server <- function(input,output,session){
  
  output$maxle <- renderTable(digits=7,hover=TRUE, {
    if(input$dist=="Base distribution"&&input$alpha>0){
      set.seed(1066)
      x <-rllogis(1000) 
      lnL <- function(param){  
        negL <- suppressWarnings(-sum(dllogis(x,scale=param[1],shape=param[2],log=TRUE)))
        return(negL)
      }
      p <- c(input$alpha,input$beta)
      n <- suppressWarnings(nlm(lnL,p,hessian=TRUE))
    }
    #else if(input$dist=="Alpha power transformed fisk distribution"&&input$alpha>0){
    #set.seed(1066)
    #x <-rllogis(1000) 
    #lnL <- function(param){  
    # negL <- suppressWarnings(-sum(log((log(param[3])/(param[3]-1))*dllogis(x,scale=param[1],shape=param[2])*(param[3])^(pllogis(x,scale=param[1],shape=param[2])))))
    # return(negL)
    # }
    # p <- array(c(input$alpha,input$beta,input$lambda),dim=c(3,1))
    #n <- suppressWarnings(nlm(lnL,p,hessian=TRUE))
    #}
    
  })
  output$fito <- renderPlot({ 
    if(input$dist=="Base distribution"){
      x <- rllogis(1000,scale=input$alpha,shape=input$beta)
      fit <- fitdist(x,"llogis")
      llplot(fit,fit.show = TRUE,loglik=TRUE)
    }
  })
  
  output$har <- renderPrint({  
    
    
    
    if(input$mod=="Compute"){ 
      x <- c(4434574127,6641000984,11502001296,12351000122,15344001235,19486001312,22456001892,
             23957100000,20303543796,23688000000,22897000000,22178100000,23502500000,
             24192900000,24238590000,23361907420,24715390510,24223506789,24907644255,30234066287,
             29685640593,30992159408,32916211354,35944972092,33203400000,8120900000,22594500000,
             25197200000,25201900000,25503651833,37843285667,47670581689,28513291851,49947524212,
             71378604979,60511616157,95294677295,99919234059,102309606703,104985647265,110942271934,
             114941415090,120689721007,127451750214,135354459261,145696153005) 
      
      dfisk <- function(x,a,b){dllogis(x,a,b)}
      pfisk <- function(q,a,b){pllogis(q,a,b)} 
      qfisk <- function(p,a,b){qllogis(p,a,b)} 
      res <- fitdist(x,"llogis", start=list(shape=1,scale=1)) 
      res 
      
    }
  })
  output$har0 <- renderPrint({
    if(input$mod=="Compute"){
      gofstat(res,fitnames="Fisk MLE") 
    }
  })
  output$har1 <- renderPrint({
    if(input$mod=="Compute"){
      x <- c(4434574127,6641000984,11502001296,12351000122,15344001235,19486001312,22456001892,
             23957100000,20303543796,23688000000,22897000000,22178100000,23502500000,
             24192900000,24238590000,23361907420,24715390510,24223506789,24907644255,30234066287,
             29685640593,30992159408,32916211354,35944972092,33203400000,8120900000,22594500000,
             25197200000,25201900000,25503651833,37843285667,47670581689,28513291851,49947524212,
             71378604979,60511616157,95294677295,99919234059,102309606703,104985647265,110942271934,
             114941415090,120689721007,127451750214,135354459261,145696153005) 
      
      
      dafisk <- function(x,a,b,l) {(log(l)/(l-1))*(dllogis())*(l^(pllogis()))} 
      pafisk <- function(q,a,b,l) {((l^(pllogis()) - 1)/(l-1))}
      qafisk <- function(p,a,b,l) {a*(((log(l*p-p+1))/(log(l)-log(l*p-p+1))))^(1/b)} 
      res2 <- fitdist(x,"afisk", start=list(a=1,b=1,l=2)) 
      res2
      
      
      
    }
  })
  output$har2 <- renderPrint({
    if(input$mod=="Compute"){
      gofstat(res2, fitnames="Alpha transformed fisk MLE")
    }
  })
  
  output$hare <- renderPlot({
    if(input$mod=="Compute"){
      x <- c(4434574127,6641000984,11502001296,12351000122,15344001235,19486001312,22456001892,
             23957100000,20303543796,23688000000,22897000000,22178100000,23502500000,
             24192900000,24238590000,23361907420,24715390510,24223506789,24907644255,30234066287,
             29685640593,30992159408,32916211354,35944972092,33203400000,8120900000,22594500000,
             25197200000,25201900000,25503651833,37843285667,47670581689,28513291851,49947524212,
             71378604979,60511616157,95294677295,99919234059,102309606703,104985647265,110942271934,
             114941415090,120689721007,127451750214,135354459261,145696153005) 
      
      dfisk <- function(x,a,b){dllogis(x,a,b)}
      pfisk <- function(q,a,b){pllogis(q,a,b)} 
      qfisk <- function(p,a,b){qllogis(p,a,b)} 
      res <- fitdist(x,"llogis", start=list(shape=1,scale=1)) 
      plot(res)
    }
  })
  
  
  output$ddd <- renderPlot({
    if(input$disto=="Base distribution"&&input$plut=="PDF"&&input$alphaa>0){
      x <- seq(from=input$axis[1],to=input$axis[2],length.out=1000)
      y <- dllogis(x,scale=input$alphaa,shape=input$betaa)
      datam <- data.frame(x,y)
      mam <- ggplot(datam,mapping=aes(x=x,y=y)) +geom_density(stat="identity",color="red") 
      mam
    }else if(input$disto=="Base distribution"&&input$plut=="CDF"&&input$alphaa>0){
      x <- seq(from=input$axis[1],to=input$axis[2],length.out=1000)
      y <- pllogis(x,scale=input$alphaa,shape=input$betaa)
      datam <- data.frame(x,y)
      mam <- ggplot(datam,mapping=aes(x=x,y=y)) +geom_density(stat="identity",color="red") 
      mam
    }else if(input$disto=="Base distribution"&&input$plut=="Hazard function"&&input$alphaa>0){
      x <- seq(from=input$axis[1],to=input$axis[2],length.out=1000)
      y <- hllogis(x,scale=input$alphaa,shape=input$betaa)
      datam <- data.frame(x,y)
      mam <- ggplot(datam,mapping=aes(x=x,y=y)) +geom_density(stat="identity",color="red") 
      mam
    }else if(input$disto=="Base distribution"&&input$plut=="Quantile function"&&input$alphaa>0){
      p <- seq(from=0,to=1,length.out=1000)
      y <- qllogis(p,scale=input$alphaa,shape=input$betaa)
      datam <- data.frame(p,y)
      mam <- ggplot(datam,mapping=aes(x=p,y=y)) +geom_density(stat="identity",color="red") 
      mam
    }else if(input$disto=="Base distribution"&&input$plut=="Cumulative hazard function"&&input$alphaa>0){
      x <- seq(from=input$axis[1],to=input$axis[2],length.out=1000)
      y <- Hllogis(x,scale=input$alphaa,shape=input$betaa)
      datam <- data.frame(x,y)
      mam <- ggplot(datam,mapping=aes(x=x,y=y)) +geom_density(stat="identity",color="red") 
      mam
    }else if(input$disto=="Base distribution"&&input$plut=="Random deviates"&&input$alphaa>0){
      n <- seq(from=input$axis[1],to=input$axis[2],length.out=1000)
      y <- rllogis(n,scale=input$alphaa,shape=input$betaa)
      datam <- data.frame(n,y)
      mam <- ggplot(datam,mapping=aes(x=n,y=y)) +geom_density(stat="identity",color="red") 
      mam
    }else if(input$disto=="Alpha power transformed fisk distribution"&&input$plut=="CDF"&&input$alphaa>0){
      x <- seq(from=input$axis[1],to=input$axis[2],length.out=1000)
      y <- ((input$lambda)^pllogis(x,scale=input$alphaa,shape=input$betaa) - 1)/(input$lambda -1)
      datam <- data.frame(x,y)
      mam <- ggplot(datam,mapping=aes(x=x,y=y)) +geom_density(stat="identity",color="purple") 
      mam
    }else if(input$disto=="Alpha power transformed fisk distribution"&&input$plut=="PDF"&&input$alphaa>0){
      x <- seq(from=input$axis[1],to=input$axis[2],length.out=1000)
      y <- (log(input$lambda)/(input$lambda-1))*dllogis(x,scale=input$alphaa,shape=input$betaa)*(input$lambda)^(pllogis(x,scale=input$alphaa,shape=input$betaa))
      datam <- data.frame(x,y)
      mam <- ggplot(datam,mapping=aes(x=x,y=y)) +geom_density(stat="identity",color="purple") 
      mam
    }else if(input$disto=="Alpha power transformed fisk distribution"&&input$plut=="Quantile function"&&input$alphaa>0){
      p <- seq(from=0,to=1,length.out=1000)
      y <- (input$alphaa)*((log(input$lambda*p-p+1))/(log(input$lambda)-log(input$lambda*p-p+1)))^(input$betaa/2)
      datam <- data.frame(p,y) 
      mam <- ggplot(datam,mapping=aes(x=p,y=y)) +geom_density(stat="identity",color="purple")  
      mam
    }else if(input$disto=="Alpha power transformed fisk distribution"&&input$plut=="Cumulative hazard function"&&input$alphaa>0){
      x <- seq(from=input$aix[1],to=input$axis[2],length.out=1000)
      H <- -log((input$lambda-(input$lambda)^(1-(1+(x/input$alphaa)^(input$alphaa)))^-1)/(input$lambda-1))
      datam <- data.frame(x,H) 
      mam <- ggplot(datam,mapping=aes(x=x,y=H)) +geom_density(stat="identity",color="purple")  
      mam
    }else if(input$disto=="Alpha power transformed fisk distribution"&&input$plut=="Hazard function"&&input$alphaa>0){
      x <- seq(from=input$axis[1],to=input$axis[2],length.out=1000)
      h <-(((input$alphaa)^-(input$betaa))*log(input$lambda)*(x^(input$betaa-1))*(1+(x/input$alphaa)^(input$betaa))^(-2)*((input$lambda)^(1-(1+(x/input$alphaa)^input$betaa)^-1)))/(((input$lambda)^(1-(1+(x/input$alphaa)^input$betaa)^-1))-1)
      datam <- data.frame(x,h) 
      mam <- ggplot(datam,mapping=aes(x=x,y=h)) +geom_density(stat="identity",color="purple")  
      mam
    }else if(input$disto=="Beta fisk distribution"&&input$plut=="PDF"&&input$alphaa>0){
      x <- seq(from=input$axis[1],to=input$axis[2],length.out=1000)
      y <- (dllogis(x,scale=input$alphaa,shape=input$betaa)/(beta(input$a,input$b)) * ((pllogis(x,scale=input$alphaa,shape=input$betaa))^(input$a*input$b-2))*(x/input$alphaa)^(-(input$betaa*input$b)+input$betaa))
      datam <- data.frame(x,y) 
      mam <- ggplot(datam,mapping=aes(x=x,y=y)) +geom_density(stat="identity",color="blue")  
      mam
    }else if(input$disto=="Beta fisk distribution"&&input$plut=="CDF"&&input$alphaa>0){
      x <- seq(from=input$axis[1],to=input$axis[2],length.out=1000)
      y <- 
        datam <- data.frame(x,y) 
      mam <- ggplot(datam,mapping=aes(x=x,y=y)) +geom_density(stat="identity",color="blue")  
      mam
    }
    
  })
  
  
  vals <- reactiveValues(x = NULL)
  observe({
    req(input$upload)
    
    tryCatch(
      {
        x <- read.csv(input$upload$datapath,
                      header = TRUE,
                      sep = ",",
                      stringsAsFactors = TRUE,
                      row.names = NULL)
        
      },
      error = function(e) {
        
        stop(safeError(e))
      }
    )
    
    vals$x <- x
  })
  
  output$print <- renderPrint({
    vals$x
  })
  output$x1 = DT::renderDataTable(vals$x, selection = 'none', rownames = FALSE, edit = TRUE)
  
  proxy = dataTableProxy('x1')
  
  observeEvent(input$x1_cell_edit, {
    info = input$x1_cell_edit
    str(info)
    i = info$row
    j = info$col + 1
    v = info$value
    
    vals$x[i, j] <<- DT:::coerceValue(v, vals$x[i, j])
    replaceData(proxy, vals$x, resetPaging = FALSE, rownames = FALSE)
  })
  
  output$download <- downloadHandler("example.csv", 
                                     content = function(file){
                                       write.csv(vals$x, file, row.names = F)
                                     },
                                     contentType = "text/csv")
  
  
  
  
  
  
}




shinyApp(ui, server)

