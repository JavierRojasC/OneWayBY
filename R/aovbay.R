aovbay <- function(dataset=FALSE) {

  require(shiny)
  require(highcharter)
  require(shinydashboard)
  require(shinydashboardPlus)
  require(BayesFactor)
  require(dplyr)
  require(waiter)
  require(broom)
  require(nortest)
  require(moments)
  require(car)
  require(shinycssloaders)
  require(rstan)
  require(reshape)
  require(purrr)

  # dotR <- file.path(Sys.getenv("HOME"), ".R")
  # if (!file.exists(dotR)) dir.create(dotR)
  # MAKEVARS <- file.path(dotR, "Makevars")
  # if (!file.exists(MAKEVARS)) file.create(MAKEVARS)
  #
  # cat(
  #   "\nCXXFLAGS=-Os -mtune=native -march=native",
  #   "CXXFLAGS += -Wno-unused-variable -Wno-unused-function  -Wno-unknown-pragmas",
  #   "CXX=clang++",
  #   file = MAKEVARS,
  #   sep = "\n",
  #   append = TRUE

  # )
  # Sys.setenv(R_MAKEVARS_USER = "/read-only/path/to/Makevars")


  #rt <- stanc(file='https://raw.githubusercontent.com/JavierRojasC/JavierRCam/master/oneway.stan')
  rt <- (stanc(model_code = stanmodels$onewaymodel))
  sm <- stan_model(stanc_ret = rt, verbose=FALSE)

  left_footer <- fluidRow(
    column(
      width = 6,
      align = "left",
      a(
        href = "http://www.fcnm.espol.edu.ec/",
        target = "_blank",
        img(src = "https://github.com/JavierRojasC/JavierRCam/blob/master/fcnm.png?raw=true", height = "30px"),
        class = "dropdown",
        title = "Facultad de Ciencias Naturales y Matemáticas")
    )
  )

  app <- list(
    ui = dashboardPage(
      preloader = list(html = tagList(spin_three_bounce(), h3("Espere un momento ...")), color = "#1E3A4E"),

      title =  'Senescyt 2021' ,
      dashboardHeader(title = "Análisis de Varianza",
                      titleWidth = 450),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Base de Datos", tabName = "BD", startExpanded = TRUE,icon = icon("database")),
          menuItem("Supuestos", tabName = "Supuestos", startExpanded = TRUE,icon = icon("tasks")),
          menuItem("ANOVA Clásico", tabName = "ANOVAcl", startExpanded = TRUE,icon = icon("adn")),
          menuItem("Kruskal Wallis", tabName = "KW", startExpanded = TRUE,icon = icon("kickstarter-k")),
          menuItem("ANOVA Bayesiano", tabName = "ANOVAby", startExpanded = TRUE,icon = icon("bold"))



        )),

      dashboardBody( tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #DADADA;
                                color: #2B1F57
                                }

                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #A1A1A1;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #6B94BF;
                                }

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #546A90;
                                }

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #A8A8A8;

                                }

                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #8B8989;
                                color: #151515;
                                style:"font-family:verdana";
                                }

                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #6F6F6F;
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #DDDDDD;
                                }

                             /* body */
                                 .skin-blue .main-body .content-wrapper, .right-side {
                                background-color: #F3F3F3;
                                 }

                                .box.box-solid.box-primary>.box-header{
  background: rgb(0, 129, 201);
  color: #57A184;
    font-size: 18px;
  font-weight; bold;
}

.box.box-solid.box-primary{
  font-family: OpenSans;
  font-size: 16px;
  text-align: left;
  color: #AA3B3B;
}

                                '))),
                     tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),

                     tabItems(
                       tabItem(tabName= "BD",
                               box(fluidRow(
                                 column(6,fileInput("file1", "Subir base en csv",
                                                    accept = c(
                                                      "text/csv",
                                                      "comma-separated-values,text/plain",
                                                      ".csv")
                                 )),
                                 column(6,checkboxInput("header", "Presione si la primera fila contiene los nombres de las columnas", TRUE),
                                        radioButtons(inputId="separador",label="Separador",
                                                     choices = c(Comma=',', Semicolon=";", Tab="\t", Space=''),
                                                     selected = ','))
                               ),uiOutput('var')),
                               fluidRow(width=12,
                                        box(title="Vista de la base de datos",

                                            dataTableOutput("DTable")))

                       ),
                       tabItem(tabName = "Supuestos",
                               sliderInput(inputId = 'alpha',
                                           label='Ingrese alpha (Error tipo 1)',
                                           value=0.05,
                                           min=0,
                                           max=1),
                               box(title = 'Normalidad de los residuos',collapsible = TRUE,
                                   width = 12,
                                   column(6,
                                          withSpinner(highchartOutput('normalidad',  height = "350px"), type = 7, color='#C7D5EB')
                                   ),
                                   column(6,
                                          h2(textOutput('pruebaNorm')),
                                          tableOutput('normalidadKolmog'),
                                          h3(textOutput('normalidadConclu')),
                                          h2(htmlOutput('CumpleNorm')))),
                               box(title = 'Homocedasticidad de los residuos',collapsible = TRUE,
                                   width = 12,
                                   column(6,
                                          withSpinner(highchartOutput('homocedasticidad',  height = "350px"), type = 7, color='#C7D5EB')
                                   ),
                                   column(6,
                                          h2('Homocedasticidad por prueba de Bartlett'),
                                          tableOutput('homocedasticidadBart'),
                                          h3(textOutput('homocedasticidadConclu')),
                                          h2(htmlOutput('CumpleHomoc')))),
                               box(title = 'Independencia de los residuos',collapsible = TRUE,
                                   width = 12,

                                   column(12,
                                          h2('Independencia por prueba de Durbin Watson'),
                                          tableOutput('independenciaDurbin'),
                                          h3(textOutput('independenciaConclu')),
                                          h2(htmlOutput('CumpleIndependencia')))),
                               box(title = 'Simetría de los residuos',
                                   width = 12,collapsible = TRUE,
                                   column(6,
                                          withSpinner(highchartOutput('simetria',  height = "350px"), type = 7, color='#C7D5EB')
                                   ),
                                   column(6,
                                          h2('Simetría - Coeficiente de asimetría'),
                                          tableOutput('simetriaCoef'),
                                          h3(textOutput('simetriaConclu')),
                                          h2(htmlOutput('CumpleSimet')))),
                               box(width = 12,collapsible = TRUE,
                                   withSpinner(highchartOutput('diagramaSupuestos',  height = "650px"), type = 7, color='#C7D5EB'),
                                   h2('Técnica disponible'),
                                   withSpinner(highchartOutput('eleccionTecnica'), type = 7, color='#C7D5EB'))),

                       tabItem(tabName = "ANOVAcl",
                               sliderInput(inputId = 'alpha2',
                                           label='Ingrese alpha (Error tipo 1)',
                                           value=0.05,
                                           min=0,
                                           max=1),

                               box(title = "Tabla ANOVA Clásico",collapsible = TRUE,
                                   tableOutput('Aov'),
                                   h2("Conclusión"),
                                   h3(textOutput('conclusionAov'))),
                               column(12,withSpinner(highchartOutput('Box',  height = "450px"), type = 7, color='#C7D5EB')),
                               box(title = "Post-Hoc",collapsible = TRUE,
                                   width=12,
                                   column(6,
                                          h3('TukeyHSD'),
                                          tableOutput('AovPostHoc')),
                                   column(6,
                                          withSpinner(highchartOutput('AovPostHocGraph',  height = "450px"), type = 7, color='#C7D5EB')))
                       ),
                       tabItem(tabName = "KW",
                               sliderInput(inputId = 'alphakw',
                                           label='Ingrese alpha (Error tipo 1)',
                                           value=0.05,
                                           min=0,
                                           max=1),

                               box(title = "Tabla Kruskal Wallis",collapsible = TRUE,
                                   tableOutput('kw'),
                                   h2("Conclusión"),
                                   h3(textOutput('conclusionKW'))),

                               box(title = 'Post Hoc: Pairwise comparisons using Wilcoxon rank sum exact test ',collapsible = TRUE,
                                   selectInput('padjust', 'Adjustment methods',
                                               c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY",
                                                 "fdr", "none")),
                                   h3('p-values adjusted'),
                                   tableOutput('KWpost')
                               )

                       ),
                       tabItem(tabName = "ANOVAby",

                               box(title = "Tabla ANOVA Bayesiano",collapsible = TRUE,
                                   tableOutput('AovBY'),
                                   h2("Conclusión"),
                                   h3(textOutput('conclusionaovby'))),
                               box(title = 'Centro de control',collapsible = TRUE,
                                   sliderInput(inputId = 'prior',
                                               label='Ingrese probabilidad a priori',
                                               value=0.5,
                                               min=0,
                                               max=1),
                                   numericInput(inputId = 'numberiterations',
                                                label='Ingrese el número de iteraciones',
                                                value=1000,
                                                min=500,
                                                max=3000),
                                   sliderInput(inputId = 'chainsnumber',
                                               label='Ingrese número de cadenas:',
                                               value=1,
                                               min=1,
                                               max=4)),
                               box(title = "Posterior", width=12,collapsible = TRUE,
                                   column(12, align="center",tableOutput('AovBYpost'))),
                               box(title = "MCMC",collapsible = TRUE,
                                   width = 12,
                                   column(6,
                                          selectInput("mcmcCHAIN","Seleccione MCMC",
                                                      c("Media y Varianza",
                                                        "Tratamientos")),
                                          withSpinner(highchartOutput('AovBYposmcmc',  height = "450px"), type = 7, color='#C7D5EB')),
                                   column(6,withSpinner(highchartOutput('AovBYposcurves',  height = "450px"), type = 7, color='#C7D5EB'))

                               )

                       )
                     ))),
    dashboardFooter(
      left = left_footer,
      right = NULL),

    server = function(input, output) {



      data <- reactive({


        if (dataset == FALSE){
          inFile <- input$file1

          if (is.null(inFile))
            return(NULL)

          data=read.csv2(inFile$datapath, sep=input$separador,header = input$header)
          data
        } else {
          data = dataset}






      })

      output$DTable <- renderDataTable({
        Data <- data()

      })

      output$var <- renderUI({

        if(is.null(data())){return()}

        else list (

          selectInput("y", "Variable dependiente", choices =    names(data())),
          selectInput("x", "Variable independiente", choices = names(data()))


        )
      })

      output$Aov <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        SA <- summary(aov(Depend~Factor))
        S <- as.data.frame(SA[[1]])
        S <- signif(S,4)
        S[is.na(S)] <- ' '

        S <- data.frame(c(Ind,'Residuals'),S)

        colnames(S) <- c('','Gl','SC','MC','F','Val-p')
        S
      })
      output$conclusionAov <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x

        SA <- summary(aov(as.numeric(as.matrix(Data[,Dep]))~as.factor(as.matrix(Data[,Ind]))))
        if (SA[[1]][['Pr(>F)']][1] < input$alpha2){
          response <- paste0('Existen diferencias significativas entre los grupos de ',Ind)
        } else if  (SA[[1]][['Pr(>F)']][1] > input$alpha2){
          response <- paste0('No existen diferencias significativas entre los grupos de ',Ind)}

        response
      })

      output$normalidad <- renderHighchart({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x



        SA <- (aov(as.numeric(as.matrix(Data[,Dep]))~as.factor(as.matrix(Data[,Ind]))))
        Graph <- qqnorm(SA$residuals, pch = 1, frame = FALSE)
        DataLine <- data.frame(xd=Graph[[1]],yd=Graph['y'])
        colnames(DataLine) <- c('xd','yd')
        LIN <- augment(lm(yd~xd, data=DataLine))



        yRES=SA$residuals
        distribution = qnorm
        probs = c(0.25, 0.75)
        qtype = 7

        y1 <- quantile(yRES, probs, names = FALSE, type = qtype, na.rm = TRUE)
        x1 <- distribution(probs)

        slope <- diff(y1)/diff(x1)
        int <- y1[1L] - slope * x1[1L]

        Int=int
        Slp=slope


        x=Graph[[1]]
        Recta <- Int+Slp*x
        lineQQ <- data.frame(x2=Graph[[1]], y2=Recta)
        highchart() %>%
          hc_add_series(lineQQ, "line", hcaes(x = 'x2', y = 'y2'), name='QQ line', color='#A9DEDE',
                        marker= list(symbol='url(graphic.png)'))%>%
          hc_add_series(LIN, "scatter", hcaes(x='xd', y='yd'), name='Puntos', color='#2B275A') %>%
          hc_yAxis(
            title = list(text = "Residuos Estandarizados"),
            max=max(lineQQ$y2),
            min=min(lineQQ$y2))%>%
          hc_xAxis(
            title = list(text = "Cuantiles Teóricos"))%>%
          hc_title(text='QQ plot')
      })

      output$AovPostHoc <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        SA <- (aov(Depend~Factor))
        intervals = TukeyHSD(SA)

        S <- as.data.frame(intervals[[1]])
        S <- signif(S,4)
        S <- cbind(rownames(S),S)

        names(S)[1] <- ' '

        S
      })

      output$AovPostHocGraph <- renderHighchart({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        SA <- (aov(Depend~Factor))
        intervals = TukeyHSD(SA)

        S <- as.data.frame(intervals[[1]])
        S <- signif(S,4)
        S <- cbind(rownames(S),S)


        names(S)[1] <- 'Trat'

        hchart(pointWidth=0,type = 'columnrange',S,name='Intervalo',
               hcaes(x=Trat,high=upr, low=lwr), color='#224361')%>%
          hc_add_series(S, type='scatter', hcaes(x=Trat, y=diff), name='Diferencias', color='#289B9C',
                        tooltip = list(pointFormat = "<br> Diferencia = {point.y}"))%>%
          hc_xAxis(title=list(text=('Combinaciones de tratamientos')))%>%
          hc_yAxis(title=list(text=('Diferencias')),
                   plotLines = list(list(
                     value = 0,
                     color = '#DAE0EA',
                     width = 3,
                     zIndex = 4,
                     label = list(text = "",
                                  style = list( color = '#DAE0EA', fontWeight = 'bold' )))))
      })

      output$normalidadKolmog <- renderTable({
        Data <- data()

        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x

        SA <- (aov(as.numeric(as.matrix(Data[,Dep]))~as.factor(as.matrix(Data[,Ind]))))

        if (length(SA$residuals)>30){

          Test <- lillie.test(SA$residuals)
          Tabla <- data.frame(Estadístico=signif(Test$statistic,4),
                              ValP=signif(Test$p.value,4))
          colnames(Tabla) <- c('Estadístico KS','Valor-P')
          Tabla
        } else {
          Test <- shapiro.test(SA$residuals)
          Tabla <- data.frame(Estadístico=Test$statistic,
                              ValP=Test$p.value)
          colnames(Tabla) <- c('Shapiro-Wilk statistic','p-value')
          Tabla
        }
      })


      output$normalidadConclu <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x

        SA <- (aov(as.numeric(as.matrix(Data[,Dep]))~as.factor(as.matrix(Data[,Ind]))))
        if (length(SA$residuals)>30){
          Test <- lillie.test(SA$residuals)
          if (Test$p.value >= input$alpha){
            response=paste0('Según el test de Kolmogorov-Smirnov, los residuos son normales')
          } else {
            response=paste0('Según el test de Kolmogorov-Smirnov, los residuos no son normales')
          }
          response
        } else {
          Test <- shapiro.test(SA$residuals)
          if (Test$p.value >= input$alpha){
            response=paste0('Según el test de Shapiro-Wilk, los residuos son normales')
          } else {
            response=paste0('Según el test de Shapiro-Wilk, los residuos no son normales')
          }
          response
        }
      })

      output$pruebaNorm <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x

        SA <- (aov(as.numeric(as.matrix(Data[,Dep]))~as.factor(as.matrix(Data[,Ind]))))
        if (length(SA$residuals)>30){

          response=paste0('Normalidad por prueba de Kolmogórov-Smirnov')

          response
        } else {

          response=paste0('Normalidad por prueba de Shapiro-Wilk')

          response
        }
      })

      output$CumpleNorm <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x

        SA <- (aov(as.numeric(as.matrix(Data[,Dep]))~as.factor(as.matrix(Data[,Ind]))))

        if (length(SA$residuals)>30){
          Test <- lillie.test(SA$residuals)

          if(Test$p.value >=  input$alpha ){
            return(paste("Supuesto de Normalidad: ","<span style=\"color:green;\"> Sí cumple.</span>"))

          }else{
            return(paste("Supuesto de Normalidad: ","<span style=\"color:red;\"> No cumple.</span>"))
          }} else {

            Test <- shapiro.test(SA$residuals)

            if(Test$p.value >=  input$alpha ){
              return(paste("Supuesto de Normalidad: ","<span style=\"color:green;\"> Sí cumple.</span>"))

            }else{
              return(paste("Supuesto de Normalidad: ","<span style=\"color:red;\"> No cumple.</span>"))
            }
          }
      })



      #_________________________________________________________________

      output$homocedasticidad <- renderHighchart({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x

        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Ind2','Dep2')
        SA <- (aov(Dep2 ~ Ind2, data=dataBY))

        xs=SA$fitted.values
        ys=SA$residuals
        lineAR <- data.frame(x2=xs, y2=ys)
        highchart() %>%

          hc_yAxis(
            title = list(text = "Residuos"),
            plotLines = list(list(
              value = 0,
              color = '#A9DEDE',
              width = 3,
              zIndex = 4,
              label = list(text = "",
                           style = list( color = '#1D4B5E', fontWeight = 'bold' )))),
            max=max(lineAR$y2),
            min=min(lineAR$y2))%>%
          hc_add_series(lineAR, "scatter", hcaes(x = 'x2', y = 'y2'), name='Residuos vs Ajustados', color='#2B275A'
          )%>%
          hc_xAxis(
            title = list(text = "Valores Ajustados"))%>%
          hc_title(text='Residuos vs Ajustados')
      })


      output$homocedasticidadBart <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x

        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Ind2','Dep2')

        Bart <- bartlett.test(Dep2 ~ Ind2, data=dataBY)
        Tabla <- data.frame(Estadístico=signif(Bart$statistic,4),
                            ValP=signif(Bart$p.value,4))
        colnames(Tabla) <- c('Estadístico K cuadrado de Bartlett','Valor-P')
        Tabla
      })


      output$homocedasticidadConclu <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        prio <- input$prior

        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Ind2','Dep2')

        Bart <- bartlett.test(Dep2 ~ Ind2, data=dataBY)

        if (Bart$p.value >= input$alpha){
          response=paste0('Según el test de Bartlett, las muestras presentan varianzas iguales')
        } else {
          response=paste0('Según el test de Bartlett, las muestras presentan varianzas desiguales')
        }
        response
      })


      output$CumpleHomoc <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        prio <- input$prior

        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Ind2','Dep2')

        Bart <- bartlett.test(Dep2 ~ Ind2, data=dataBY)
        if(Bart$p.value >=  input$alpha ){
          return(paste("Supuesto de Homocedasticidad: ","<span style=\"color:green;\"> Sí cumple.</span>"))

        }else{
          return(paste("Supuesto de Homocedasticidad: ","<span style=\"color:red;\"> No cumple.</span>"))
        }
      })
      #________________________________________________________________




      output$independenciaDurbin <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x


        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Ind2','Dep2')

        Aov <- aov(Dep2 ~ Ind2, data=dataBY)
        DW <- durbinWatsonTest(Aov)
        Tabla <- data.frame(Autocor=DW[1],
                            Dw=signif(as.numeric(DW[2]),4),
                            ValP=signif(as.numeric(DW[3]),4))
        colnames(Tabla) <- c('Autocorrelación','D-W Statistic',
                             'p-value')
        Tabla
      })


      output$independenciaConclu <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        prio <- input$prior

        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Ind2','Dep2')

        Aov <- aov(Dep2 ~ Ind2, data=dataBY)
        DW <- durbinWatsonTest(Aov)

        if (DW[3] >= input$alpha){
          response=paste0('Según el test de Durbin Watson, no existe presencia de autocorrelación en los residuos.')
        } else {
          response=paste0('Según el test de Durbin Watson, existe presencia de autocorrelación en los residuos.')
        }
        response
      })


      output$CumpleIndependencia <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        prio <- input$prior


        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Ind2','Dep2')
        Aov <- aov(Dep2 ~ Ind2, data=dataBY)
        DW <- durbinWatsonTest(Aov)

        if (DW[3] >= input$alpha){
          return(paste("Supuesto de Independencia: ","<span style=\"color:green;\"> Sí cumple.</span>"))

        }else{
          return(paste("Supuesto de Independencia: ","<span style=\"color:red;\"> No cumple.</span>"))
        }
      })

      #_________________________________________________________

      output$simetria <- renderHighchart({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        # Dep <- names(Data)[2]
        #  Ind <- names(Data)[1]
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Ind2','Dep2')
        SA <- (aov(Dep2 ~ Ind2, data=dataBY))

        skewness(SA$residuals)

        histRes <- hist(SA$residuals, plot=FALSE)
        hchart(histRes, name='', color='#84DED4')%>%
          hc_title(text='Histograma de los residuos')
      })


      output$simetriaCoef <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x

        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Ind2','Dep2')
        SA <- (aov(Dep2 ~ Ind2, data=dataBY))


        Tabla <- data.frame(Estadístico=skewness(SA$residuals))
        colnames(Tabla) <- c('Coeficiente de asimetría')
        Tabla
      })


      output$simetriaConclu <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x

        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Ind2','Dep2')
        SA <- (aov(Dep2 ~ Ind2, data=dataBY))

        if (skewness(SA$residuals) > 0){
          response=paste0('Según el coeficiente de asimetría, la distribución de los residuos tiene una asimetría positiva (Sesgo a la derecha)')
        } else if (skewness(SA$residuals) < 0){
          response=paste0('Según el coeficiente de asimetría, la distribución de los residuos tiene una asimetría negativa (Sesgo a la izquierda)')
        } else {
          response=paste0('Según el coeficiente de asimetría, la distribución de los residuos es simétrica ')

        }
        response
      })


      output$CumpleSimet <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x

        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Ind2','Dep2')
        SA <- (aov(Dep2 ~ Ind2, data=dataBY))

        if(skewness(SA$residuals) ==  0 ){
          return(paste("Supuesto de Simetría: ","<span style=\"color:green;\"> Sí cumple.</span>"))

        }else{
          return(paste("Supuesto de Simetría: ","<span style=\"color:red;\"> No cumple.</span>"))
        }
      })

      #__________________________________________________




      output$Box <- renderHighchart({

        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x

        Means <- aggregate(as.matrix(Data[,Dep]) ~ as.factor(as.matrix(Data[,Ind])), data = Data, mean)
        colnames(Means) <- c('Nombres', 'Media')

        hcboxplot(x=as.numeric(as.matrix(Data[,Dep])), var=as.factor(as.matrix(Data[,Ind])), name = "Diagrama de cajas", color = "#0E1142", outliers = FALSE,
                  showInLegend=TRUE)%>%
          hc_yAxis(title = list(text = Dep))%>%
          hc_xAxis(title = list(text = "Niveles"))%>%
          hc_chart(type = "column")%>%
          hc_plotOptions(showInLegend=TRUE,dataLabels=TRUE)%>%
          hc_add_series(Means, type='bubble', hcaes(x =Nombres,y=Media),maxSize = "7%",
                        tooltip=list(pointFormat='<br> {point.y} ',headerFormat='<b> Media'), name='Medias',
                        showInLegend=TRUE)
      })
      output$AovBY <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        prio <- input$prior





        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Tratamiento','Dep2')
        #str(dataBY)
        Anovabyy <- (anovaBF(Dep2 ~ Tratamiento, data=dataBY, whichRandom = "ID",
                             rscaleFixed = prio,iterations = input$numberiterations))



        S <- data.frame(Priori=(prio), BF=Anovabyy[1][1])
        TabBY <- S[,1:3]
        TabBY$BF.bf <- round(TabBY$BF.bf,3)
        TabBY$BF.error <- signif(TabBY$BF.error,3 )

        colnames(TabBY) <- c('Priori','BF10','Error')
        TabBY <- rbind(TabBY, c(1-prio,1,''))

        rownames(TabBY) <- c('Modelo Alternativo', 'Modelo Nulo')
        TabBY <- cbind(rownames(TabBY),TabBY)

        names(TabBY)[1] <- ''
        TabBY
      })

      output$AovBYpost <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        prio <- input$prior



        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Tratamiento','Dep2')
        lambda <- -log(0.01)/(3*sd(dataBY$Dep2))



        pulpdat <- list(N=length(dataBY$Dep2),J=length(unique(dataBY$Tratamiento)),response=dataBY$Dep2,predictor=as.numeric(dataBY$Tratamiento),lambda=lambda)

        fit <- sampling(sm, data=pulpdat, chains=input$chainsnumber,  seed = 12345,iter=input$numberiterations)
        fit.sum <- summary(fit, pars=c("mu","sigmaalpha","sigmaepsilon","a") )

        TablaPos <- data.frame(fit.sum$summary)
        rownames(TablaPos) <- c('Mu','Sigma Alpha','Sigma Epsilon',unique(as.character(dataBY$Tratamiento)))
        TablaPos2 <- data.frame(rownames(TablaPos),TablaPos)
        colnames(TablaPos2) <- c('','Mean','SE Mean', 'SD', '2.5%','25%','50%','75%','97.5%','n eff','R hat')
        TablaPos2
        #str(dataBY)
        #Anovabyy <- (anovaBF(Dep2 ~ Tratamiento, data=dataBY, whichRandom = "ID",
        #                     rscaleFixed = prio,iterations = input$numberiterations))
        #
        #post <- summary(posterior(Anovabyy,iterations = input$numberiterations))
        #
        #S <- data.frame(post[1][1])
        #colnames(S) <- c('Media Posterior','Desv. Posterior','Naive SE','Time Series SE')
        #rownames(S)[1] <- 'Media General'
        #S <- cbind(rownames(S),S)
        #names(S)[1] <- ''
        #S
      })




      output$AovBYposmcmc <- renderHighchart({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        prio <- input$prior


        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Tratamiento','Dep2')
        #str(dataBY)
        Anovabyy <- (anovaBF(Dep2 ~ Tratamiento, data=dataBY, whichRandom = "ID",
                             rscaleFixed = prio,iterations = input$numberiterations))

        post <- (posterior(Anovabyy,iterations = input$numberiterations))
        MCMC <- data.frame(Iteración=1:input$numberiterations,post[,])

        if (input$mcmcCHAIN=="Media y Varianza"){

          highchart()%>%
            hc_yAxis_multiples( list(top = "0%", height = "50%", title = list(text = "Media"),opposite=FALSE),
                                list(top = "50%", height = "50%", title = list(text = "Sigma2") ,opposite=TRUE))%>%
            hc_add_series(MCMC, type='line', hcaes(x=Iteración,y=mu),yAxis=0, name='Media',color='#24509C')%>%
            hc_add_series(MCMC, type='line', hcaes(x=Iteración,y=sig2),yAxis=1, name='Sigma2',color='#31999C')
        } else {

          MCMCCom <- MCMC[,-c(2,ncol(MCMC),ncol(MCMC)-1)]
          rownames(MCMCCom) <- MCMC[,1]
          MCMCCom2 <- as.matrix(MCMCCom)
          MCMCMer <- melt(MCMCCom, id.vars="Iteración")
          highchart()%>%
            hc_add_series(MCMCMer, type='line', hcaes(x=Iteración, y=value, group=variable))%>%
            hc_title(text='MCMC chains')%>%
            hc_exporting(enabled = TRUE,
                         filename = paste0('Cadenas de Marcov'))

        }
      })


      #   output$AovBYposchains <- renderHighchart({
      #     Data <- data()
      #     Data <- na.omit(Data)
      #     Dep <- input$y
      #     Ind <- input$x
      #     prio <- input$prior
      #
      #
      #     Factor <- as.factor(as.matrix(Data[,Ind]))
      #     Depend <- as.numeric(as.matrix(Data[,Dep]))
      #
      #     dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
      #     colnames(dataBY) <- c('Tratamiento','Dep2')
      #     #str(dataBY)
      #     Anovabyy <- (anovaBF(Dep2 ~ Tratamiento, data=dataBY, whichRandom = "ID",
      #                          rscaleFixed = prio,iterations = input$numberiterations))
      #
      #     post <- (posterior(Anovabyy,iterations = input$numberiterations))
      #
      #     MCMC <- data.frame(Iteración=1:input$numberiterations,post[,])
      #     MCMCCom <- MCMC[,-c(2,ncol(MCMC),ncol(MCMC)-1)]
      #     rownames(MCMCCom) <- MCMC[,1]
      #     MCMCCom2 <- as.matrix(MCMCCom)
      #     MCMCMer <- melt(MCMCCom, id.vars="Iteración")
      #     highchart()%>%
      #      hc_add_series(MCMCMer, type='line', hcaes(x=Iteración, y=value, group=variable))%>%
      #       hc_title(text='MCMC chains')
      #   })
      #
      output$AovBYposcurves <- renderHighchart({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        prio <- input$prior


        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Tratamiento','Dep2')
        #str(dataBY)
        Anovabyy <- (anovaBF(Dep2 ~ Tratamiento, data=dataBY, whichRandom = "ID",
                             rscaleFixed = prio,iterations = input$numberiterations))

        post <- (posterior(Anovabyy,iterations = input$numberiterations))

        MCMC <- data.frame(Iteración=1:input$numberiterations,post[,])
        MCMCCom <- MCMC[,-c(2,ncol(MCMC),ncol(MCMC)-1)]
        MCMCMer <- melt(MCMCCom, id.vars="Iteración")


        ds <- map(levels(MCMCMer$variable), function(x){
          MCMCMer <- density(MCMCMer$value[MCMCMer$variable == x])[1:2]
          MCMCMer <- list_parse2(as.data.frame(MCMCMer))
          list(data = MCMCMer, name = x)
        })

        highchart() %>%
          hc_add_series_list(ds)%>%
          hc_yAxis(title=list(text='Density'))%>%
          hc_exporting(enabled = TRUE,
                       filename = paste0('Curvas de densidad - Distribuciones marginales posteriores.'))
      })



      output$conclusionaovby <- renderText({

        Data <- data()

        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        prio <- input$prior

        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Ind2','Dep2')
        str(dataBY)
        Anovabyy <- (anovaBF(Dep2 ~ Ind2, data=dataBY, whichRandom = "all",
                             rscaleFixed = prio))


        # posterior(Anovabyy,iterations = 1000)
        # plot(Anovabyy)


        S <- data.frame(Priori=prio, BF=Anovabyy[1])
        FB <- S[,2]
        if (FB <= 3 & FB > 1 ){
          response <- paste0('Evidencia débil a favor del rechazo de la hipótesis nula ')
        } else if  (FB <= 10 & FB > 3 ) {
          response <- paste0('Evidencia moderada a favor del rechazo de la hipótesis nula ')
        } else if  (FB <= 30 & FB > 10 ){
          response <- paste0('Evidencia fuerte a favor del rechazo de la hipótesis nula ')
        }else if  (FB > 30 ){
          response <- paste0('Evidencia decisiva  a favor del rechazo de la hipótesis nula')
        }else if  (FB < 1 & FB > 1/3 ){
          response <- paste0('Evidencia moderada a favor de la hipótesis nula ')
        }else if  (FB <= 1/3 & FB > 1/10 ){
          response <- paste0('Evidencia fuerte a favor de la hipótesis nula ')
        }else if  (FB <= 1/30 & FB > 1/100){
          response <- paste0('Evidencia decisiva  a favor de de la hipótesis nula')
        }else if  (FB == 1){
          response <- paste0('No existe evidencia')}

        response
      })


      output$diagramaSupuestos <- renderHighchart({
        Data <- data()
        #Data = Datas
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        #Ind <- "boneDev"
        #Dep <- "growth"
        alph <- input$alpha
        #alph <- 0.05
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind2=Factor, Dep2=Depend)
        colnames(dataBY) <- c('Ind2','Dep2')
        SA <- (aov(Dep2 ~ Ind2, data=dataBY))

        Test <- lillie.test(SA$residuals)

        if(Test$p.value >=  alph ){
          col_normalidad= "#77DA85"
          col_normalidad_si= "#77DA85"
          col_normalidad_no= "#D5D5D5"

        }else{
          col_normalidad= "#D5D5D5"
          col_normalidad_si= "#D5D5D5"
          col_normalidad_no= "#77DA85"
        }

        Bart <- bartlett.test(Dep2 ~ Ind2, data=dataBY)

        if(Bart$p.value >=  alph ){
          col_homocedasticidad= "#77DA85"
          col_homocedasticidad_si= "#77DA85"
          col_homocedasticidad_no= "#D5D5D5"

        }else{
          col_homocedasticidad= "#D5D5D5"
          col_homocedasticidad_si= "#D5D5D5"
          col_homocedasticidad_no= "#77DA85"
        }

        if(skewness(SA$residuals) ==  0 ){
          col_simetria= "#77DA85"
          col_simetria_si= "#77DA85"
          col_simetria_no= "#D5D5D5"

        }else{
          col_simetria= "#D5D5D5"
          col_simetria_si= "#D5D5D5"
          col_simetria_no= "#77DA85"
        }

        if(durbinWatsonTest(SA)[3] >=  alph){
          col_independencia= "#77DA85"
          col_independencia_si= "#77DA85"
          col_independencia_no= "#D5D5D5"

        }else{
          col_independencia= "#D5D5D5"
          col_independencia_si= "#D5D5D5"
          col_independencia_no= "#77DA85"
        }



        if (col_simetria_si == "#77DA85"){
          col_kw="#77DA85"
        } else {col_kw="#D5D5D5" }

        if (col_homocedasticidad_si == "#77DA85"){
          col_independencia="#77DA85"
        } else {col_independencia="#D5D5D5" }

        #  if (col_independencia_no == "#77DA85" | col_independencia_si == "#77DA85"){
        #    col_independencia="#77DA85"
        #  } else {col_independencia="#D5D5D5" }

        if (col_normalidad_si== "#77DA85" & col_homocedasticidad_si== "#77DA85" ){
          col_anova="#77DA85"
        }else {col_anova="#D5D5D5" }


        if (col_simetria_si=="#77DA85" | col_simetria_no=="#77DA85"){
          col_simetria= "#77DA85"
        }
        if (col_homocedasticidad_si=="#77DA85" | col_homocedasticidad_no=="#77DA85"){
          col_homocedasticidad= "#77DA85"
        }


        highchart() %>%
          hc_chart(type = 'organization', inverted = TRUE) %>%
          hc_add_series(name='Diagrama de técnicas según cumplimiento de supuestos',
                        data = list(
                          list(from = 'Comparación de medias por grupo', to = '¿Cumple supuesto de normalidad?'),
                          list(from = '¿Cumple supuesto de normalidad?', to = 'Sí, cumple normalidad'),
                          list(from = 'Sí, cumple normalidad', to = '¿Cumple supuesto de homocedasticidad?'),
                          list(from = '¿Cumple supuesto de normalidad?', to = 'No cumple normalidad'),
                          list(from = '¿Cumple supuesto de homocedasticidad?', to = 'Sí, cumple homocedasticidad'),
                          list(from = 'Sí, cumple homocedasticidad', to = '¿Cumple supuesto de independencia?'),
                          list(from = '¿Cumple supuesto de independencia?', to = 'Sí, cumple independencia'),
                          list(from = '¿Cumple supuesto de independencia?', to = 'No cumple independencia'),

                          list(from = '¿Cumple supuesto de homocedasticidad?', to = 'No cumple homocedasticidad'),
                          list(from = '¿Cumple supuesto de simetría?', to = 'Sí, cumple simetría'),
                          list(from = '¿Cumple supuesto de simetría?', to = 'No cumple simetría'),
                          #  list(from = 'Sí, cumple homocedasticidad', to = 'ANOVA Clásico'),
                          #list(from = 'No cumple normalidad', to = '¿Cumple supuesto de simetría?'),
                          list(from = 'No cumple homocedasticidad', to = '¿Cumple supuesto de simetría?')
                          #list(from = 'Sí, cumple simetría', to = 'Kruskal Wallis'),
                          #list(from = 'No cumple simetría', to = 'ANOVA Bayesiano'),
                          # list(from = 'Comparación de medias por grupo', to = 'ANOVA Bayesiano')






                        ),
                        nodes=  list(
                          list(id = 'Comparación de medias por grupo', color="#77D0DA"),
                          list(id = '¿Cumple supuesto de normalidad?', color=col_normalidad),
                          list(id = 'Sí, cumple normalidad', color=col_normalidad_si),
                          list(id = 'No cumple normalidad', color=col_normalidad_no),
                          list(id = '¿Cumple supuesto de homocedasticidad?', color=col_homocedasticidad),
                          list(id = 'Sí, cumple homocedasticidad', color=col_homocedasticidad_si),
                          list(id = 'No cumple homocedasticidad', color=col_homocedasticidad_no),
                          list(id = '¿Cumple supuesto de simetría?', color=col_simetria),
                          list(id = 'Sí, cumple simetría', color=col_simetria_si),
                          list(id = 'No cumple simetría', color=col_simetria_no),
                          list(id = '¿Cumple supuesto de independencia?', color=col_independencia),
                          list(id = 'Sí, cumple independencia', color=col_independencia_si),
                          list(id = 'No cumple independencia', color=col_independencia_no)
                          #list(id = 'ANOVA Clásico', color=col_anova),
                          #list(id = 'Kruskal Wallis', color=col_kw),
                          #list(id = 'ANOVA Bayesiano', color='#77DA85'))
                        ))

      })



      output$eleccionTecnica <- renderHighchart({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x

        Factor <- as.factor(as.matrix(Data[,Ind]))
        dataBY <- data.frame(Ind2=Factor, Dep2=Data[,Dep])
        colnames(dataBY) <- c('Ind2','Dep2')
        SA <- (aov(Dep2 ~ Ind2, data=dataBY))

        Test <- lillie.test(SA$residuals)

        Bart <- bartlett.test(Dep2 ~ Ind2, data=dataBY)



        if(Test$p.value >=  input$alpha & Bart$p.value >=  input$alpha){
          col_anova="#77DA85"
        } else {col_anova="#DC7676"}

        if(skewness(SA$residuals) ==  0){
          col_kw="#77DA85"
        } else {col_kw="#DC7676"}

        highchart() %>%
          hc_chart(type = 'organization', inverted=TRUE) %>%
          hc_add_series(name='Diagrama de técnicas según cumplimiento de supuestos',
                        data = list(

                          list(from = 'Kruskal Wallis', to = 'Kruskal Wallis'),
                          list(from = 'ANOVA Clásico', to = 'ANOVA Clásico'),
                          list(from = 'ANOVA Bayesiano', to = 'ANOVA Bayesiano')

                        ),
                        nodes=  list(

                          list(id = 'ANOVA Clásico', color=col_anova),
                          list(id = 'Kruskal Wallis', color=col_kw),
                          list(id = 'ANOVA Bayesiano', color='#77DA85')
                        ))

      })


      output$kw <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        SA <-kruskal.test(Depend~Factor, data = Data)

        S <- data.frame(SA$statistic,SA$parameter,signif(SA$p.value,4))

        colnames(S) <- c('Kruskal-Wallis chi-squared','Gl','Val-p')
        S
      })
      output$conclusionKW <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))
        SA <-kruskal.test(Depend~Factor, data = Data)
        if (SA$p.value < input$alphakw){
          response <- paste0('Existen diferencias significativas entre los grupos de ',Ind)
        } else if  (SA$p.value > input$alphakw){
          response <- paste0('No existen diferencias significativas entre los grupos de ',Ind)}

        response
      })

      output$KWpost <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind <- input$x
        Factor <- as.factor(as.matrix(Data[,Ind]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        Pares <- pairwise.wilcox.test(x = Depend, g = Factor, p.adjust.method = input$padjust )
        Pv <- Pares$p.value
        Pv[is.na(Pv)] <- ' - '
        Pv <- cbind(rownames(Pv),Pv)
        Pv
      })

    })
  runApp(app)
}
