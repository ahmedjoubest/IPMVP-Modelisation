credentials <- data.frame(
  user = c("xqi", "ajo","ero","aca"), # mandatory
  password = c("xqi", "ajo","ero","aca"), # mandatory
  start = c("2019-04-15"), # optinal (all others)
  expire = c(NA, NA, NA, NA),
  admin = c(FALSE, TRUE, FALSE, FALSE),
  comment = "Simple and secure authentification mechanism 
  for single 'Shiny' applications.",
  stringsAsFactors = FALSE
)
library(shinymanager)
library(scales)

library(shiny)
library(readxl)
library(stringr)
library(ggplot2)
library(shinydashboard)
source("helpers.R")

ui <- dashboardPage(
  dashboardHeader(title = "Application IPMVP"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("1- Charger les données", tabName = "uppload", icon = icon("upload", class = NULL, lib = "font-awesome")),
      menuItem("2- Choix du modèle", tabName = "modelisation", icon = icon("chart-bar", class = NULL, lib = "font-awesome")),
      menuItem("3- Evaluation d'économies", tabName = "prevision", icon = icon("chart-line", class = NULL, lib = "font-awesome"))

      
    )
  ),
  dashboardBody(tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: 	#006400
                              
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #3fb64b;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #3fb64b;
                              }        

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #3fb64b
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #3ab450;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: 	#006400;
                              color: #ffffff;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #3ab450;
                              }
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #ee5c22;
                              }
                              
                              
        .content-wrapper, .right-side {
                                background-color: #ffffff;                        
                                
                                
                                '))),
    tabItems(
      # First tab content
      tabItem( tabName = "uppload",
               
               fluidPage(align = 'center',
                         
                         fileInput("file1", "Charger les données",
                                   multiple = FALSE,
                                   accept = c("text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv")),
                         HTML('<hr width=45% style="border: 1px solid grey;" >'),
                         h3("En chargeant vos données, votre fichier doit respecter les ", strong("Consignes"), " suivante :"),
                         p(strong("1. "),"être en format Excel ",strong(".xslx")),
                         p(strong("2. "),"contenir deux colonnes temporelles représentatives des deux périodes de référence et de suivi ", tags$i("(en pas mensuel)"),", celles ci doivent être nommées respectivement :",strong("Periode.de.Reference"), " et ", strong("Periode.de.suivi"),  " et respecter le format standard de dates : ", strong("dd/mm/yyyy")),
                         p(strong("3. "),"Contenir pour les deux périodes les données de consommations ainsi que les données des grandeurs jugées influençant sur la consommation, ces dernières allant jusqu'à trois : Regression linéaire triple "),
                         p(strong("4. "),"Aucune condition d'intitulé ou d'ordre n'est imposées pour les autres colonnes, ces dernières peuvent aussi être de nombre de lignes différentes entre les deux périodes"),
                         p(strong("5. "),"Un jeu de données exemple", tags$i("(fig. ci-dessous)")," est téléchargeable ", strong(tags$a("ICI",href = "https://github.com/ahmedjoubest/IPMVP-Modelisation/raw/main/Test_IPMVP.xlsx"))),
                         HTML('<hr width=45% style="border: 1px solid grey;" >'),
                         h3("Exemple de données"),
                         img(src = "exmeple_donner.PNG",width = 920),br(),br(),br(),
                         p(img(src = "cimes.jpg", width = 80,align = "center"),align = "center")
                         
               )
      ),
      
      # Second tab content
      tabItem(tabName = "modelisation",
              fillPage(
                fillRow(uiOutput("predict"),
                        uiOutput("explic1"),
                        radioButtons("regression","Régression linéaire",choices = c("simple" = 1, "double" = 2, "tripple" = 3), selected = 1,inline =T),
                        uiOutput("explic2"),
                        uiOutput("explic3"),
                        radioButtons("resid","Régression",choices = c("Avec Résidus" = T, "Sans Résidus" = F), selected = T)
                ),br(),br(),br(),br(),br(),
                HTML('<hr width=45% style="border: 1px solid grey;" >'),
                h2("Evaluation de la modélisation",align ="center"),
                HTML('<hr width=45% style="border: 1px solid grey;" >'),
                fluidRow(
                  box(title = "Modélisation", background = "green", plotOutput("modelisation"),collapsible	= T,collapsed = T,width = "12",solidHeader = T)
                  )
                

              )
      ),
      tabItem(tabName = "prevision",
              fillPage(
                fillRow(uiOutput("new.explic.1"),
                        uiOutput("new.explic.2"),
                        uiOutput("new.explic.3"),
                        uiOutput("comparaison.n")
                ),br(),br(),br(),br(),
                HTML('<hr width=45% style="border: 1px solid grey;" >'),
                h2("Evaluation des économies et Génération du rapport technique",align ="center"),
                HTML('<hr width=45% style="border: 1px solid grey;" >'),
                fluidRow(
                  box(title = "Prévision", background = "green", plotOutput("prevision"),collapsible	= T,
                      collapsed = T,width = "9",solidHeader = T),
                  box(title = "Calcul d'économie d'énérgie", background = "green",
                      uiOutput("debutfin"),
                      collapsible	= T,collapsed = T,width = "3",solidHeader = T,
                      textOutput("economie2"),
                      textOutput("economie3"),
                      textOutput("economie1"),
                      textInput("prix", label = "Prix unitaire d'énergie en €", value = "0"),
                      tags$u(h4("Télécharger un rapport")),
                      radioButtons("typerapport","Choisir le format",choices = c("PDF" = T, "WORD" = F), selected = T,inline =T),
                      downloadButton("report", "Générer le rapport")
                      

                ),
                
                br(),br()

              )
      )
    )
  ),
  
),


)

ui <- secure_app(ui)

server <- function(input, output) {
  
    res_auth <- secure_server(check_credentials = check_credentials(credentials))
    output$predict <- renderUI({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$file1)
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    df <- as.data.frame(read_excel(input$file1$datapath))
    
    colnames(df) =  gsub(' +',' ',sub('\r\n',' ',colnames(df)))
    selectInput("predict","Choisir la variable à prédire", choices = colnames(df))
    
  })
  
  
  
  
  output$debutfin <- renderUI({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$file1)
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    df <- as.data.frame(read_excel(input$file1$datapath))
    if(sum(is.na(df$Periode.de.Reference)) == 0 & sum(is.na(df$Periode.de.suivi))== 0){
      max_new = nrow(df)
      max_ref = nrow(df)
    } else if (sum(is.na(df$Periode.de.suivi)) != 0){
      max_new = which(is.na(df$Periode.de.suivi))[1] - 1
      max_ref = nrow(df)
    } else if (sum(is.na(df$Periode.de.Reference)) != 0){
      max_new = nrow(df)
      max_ref = which(is.na(df$Periode.de.Reference))[1] - 1
    }
    sliderInput(inputId = "debutfin",
                label = "Choisir un intervalle de temps",
                min = 1,
                max = max_new,
                value = c(1, max_new),
                step = 1)    
  })
  
  
  

  
  
  
  
  
  output$explic1 <- renderUI({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$file1)
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    df <- as.data.frame(read_excel(input$file1$datapath))
    
    colnames(df) =  gsub(' +',' ',sub('\r\n',' ',colnames(df)))
    selectInput("explic1","1ière variable explicative", choices = colnames(df))
    
  })
  
  
  
  
  output$explic2 <- renderUI({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$file1)
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    df <- as.data.frame(read_excel(input$file1$datapath))
    
    colnames(df) =  gsub(' +',' ',sub('\r\n',' ',colnames(df)))
    selectInput("explic2","2ière variable explicative", choices = colnames(df))
    
  })
  
  output$explic3 <- renderUI({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$file1)
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    df <- as.data.frame(read_excel(input$file1$datapath))
    
    colnames(df) =  gsub(' +',' ',sub('\r\n',' ',colnames(df)))
    selectInput("explic3","3ième variable explicative", choices = colnames(df))
    
  })
  
  
  output$modelisation <- renderPlot({
    
    req(input$file1)
    df <- as.data.frame(read_excel(input$file1$datapath))
    
    colnames(df) =  gsub(' +',' ',sub('\r\n',' ',colnames(df)))
    model(df,input$regression,input$predict,input$explic1,input$explic2,input$resid,input$explic3)
  })
  
  output$new.explic.1 <- renderUI({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$file1)
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    df <- as.data.frame(read_excel(input$file1$datapath))
    
    colnames(df) =  gsub(' +',' ',sub('\r\n',' ',colnames(df)))
    selectInput("new.explic.1","1ère variable de la période de suivi (régression simple)", choices = colnames(df))
    
  })
  
  output$new.explic.2 <- renderUI({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$file1)
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    df <- as.data.frame(read_excel(input$file1$datapath))
    
    colnames(df) =  gsub(' +',' ',sub('\r\n',' ',colnames(df)))
    selectInput("new.explic.2","2ème variable de la période de suivi (Si régression double)", choices = colnames(df))
    
  })
  
  output$new.explic.3 <- renderUI({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$file1)
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    df <- as.data.frame(read_excel(input$file1$datapath))
    
    colnames(df) =  gsub(' +',' ',sub('\r\n',' ',colnames(df)))
    selectInput("new.explic.3","3ième variable de la période de suivi (Si la régression tripple)", choices = colnames(df))
    
  })
  
  output$comparaison.n <- renderUI({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$file1)
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    df <- as.data.frame(read_excel(input$file1$datapath))
    
    colnames(df) =  gsub(' +',' ',sub('\r\n',' ',colnames(df)))
    selectInput("comparaison.n","Consommations réelles mesurées après l'Action de Performance Energetique", choices = colnames(df))
    
  })
  
  output$prevision <- renderPlot({
    
    req(input$file1)
    df <- as.data.frame(read_excel(input$file1$datapath))
    
    
    colnames(df) =  gsub(' +',' ',sub('\r\n',' ',colnames(df)))
    prev(df,input$regression,input$predict,input$explic1,input$explic2,input$resid,input$new.explic.1,input$new.explic.2,input$comparaison.n,input$debutfin[1],input$debutfin[2],input$explic3,input$new.explic.3)
  })
  
  
  ## RApport automatique
  
  observe({
    state <- input$typerapport
    if(length(state) == 0){
      state <- 3
    } else {
    if(state){
    output$report <- downloadHandler(
                              # For PDF output, change this to "report.pdf"
                              filename = "report.pdf",
                              content = function(file) {
                                # Copy the report file to a temporary directory before processing it, in
                                # case we don't have write permissions to the current working dir (which
                                # can happen when deployed).
                                req(input$file1)
                                df <- as.data.frame(read_excel(input$file1$datapath))
                                colnames(df) =  gsub(' +',' ',sub('\r\n',' ',colnames(df)))
                                
                                prix <- as.numeric(input$prix)
                                
                                
                                tempReport <- file.path(tempdir(), "report.Rmd")
                                file.copy("report.Rmd", tempReport, overwrite = TRUE)
                                
                                # Set up parameters to pass to Rmd document
                                params <- list(df = df, regression = input$regression,predict = input$predict,explic1 = input$explic1,explic2 = input$explic2,resid = input$resid,
                                               new.explic.1 = input$new.explic.1,new.explic.2 = input$new.explic.2,comparaison.n = input$comparaison.n,debutfin_1 = input$debutfin[1],
                                               debutfin_2=input$debutfin[2],explic3 = input$explic3,new.explic.3 = input$new.explic.3, prix = prix,
                                               pdf = input$typerapport)
                                
                                # Knit the document, passing in the `params` list, and eval it in a
                                # child of the global environment (this isolates the code in the document
                                # from the code in this app).
                                rmarkdown::render(tempReport, output_file = file,
                                                  params = params,
                                                  envir = new.env(parent = globalenv())
                                )
                              }
                            )
    } else{
        output$report <- downloadHandler(
          # For PDF output, change this to "report.pdf"
          filename = "report.Doc",
          content = function(file) {
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            req(input$file1)
            df <- as.data.frame(read_excel(input$file1$datapath))
            colnames(df) =  gsub(' +',' ',sub('\r\n',' ',colnames(df)))
            
            prix <- as.numeric(input$prix)
            
            tempReport <- file.path(tempdir(), "report_word.Rmd")
            file.copy("report_word.Rmd", tempReport, overwrite = TRUE)
            
            # Set up parameters to pass to Rmd document
            params <- list(df = df, regression = input$regression,predict = input$predict,explic1 = input$explic1,explic2 = input$explic2,resid = input$resid,
                           new.explic.1 = input$new.explic.1,new.explic.2 = input$new.explic.2,comparaison.n = input$comparaison.n,debutfin_1 = input$debutfin[1],
                           debutfin_2=input$debutfin[2],explic3 = input$explic3,new.explic.3 = input$new.explic.3, prix = prix,
                           pdf = input$typerapport)
            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
          }
        )
      
    }
     

         
  }
#
  })
  


  output$economie2 <- renderText({
    

    req(input$file1)
    df <- as.data.frame(read_excel(input$file1$datapath))
    colnames(df) =  gsub(' +',' ',sub('\r\n',' ',colnames(df)))
    
    df$Periode.de.suivi = as.Date(df$Periode.de.suivi, tryFormats = c("%d/%m/%Y"))
    i.debut = input$debutfin[1]
    i.fin = input$debutfin[2]
  
    n.Periode.de.Reference = i.fin - i.debut + 1
      
    return(paste("Pour une période de :", as.character(n.Periode.de.Reference), "mois",
                 sep = " ")
          )
  })
  
  output$economie3 <- renderText({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$file1)
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    
    df <- as.data.frame(read_excel(input$file1$datapath))
    colnames(df) =  gsub(' +',' ',sub('\r\n',' ',colnames(df)))
    df$Periode.de.suivi = as.Date(df$Periode.de.suivi, tryFormats = c("%d/%m/%Y"))
    
    eco = economie(df,input$regression,input$predict,input$explic1,input$explic2,input$resid,input$new.explic.1,input$new.explic.2,input$comparaison.n,input$debutfin[1],input$debutfin[2],input$explic3,input$new.explic.3)

    #return(paste("Des économies de ", as.character(economie$eco), "ont été faite", sep = " "))
    return(paste(as.character(eco$conso)," unité de ", input$predict, " a été économisée"))
    
  })
  
  
  output$economie1 <- renderText({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$file1)
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    
    df <- as.data.frame(read_excel(input$file1$datapath))
    colnames(df) =  gsub(' +',' ',sub('\r\n',' ',colnames(df)))
    df$Periode.de.suivi = as.Date(df$Periode.de.suivi, tryFormats = c("%d/%m/%Y"))
    
    eco = economie(df,input$regression,input$predict,input$explic1,input$explic2,input$resid,input$new.explic.1,input$new.explic.2,input$comparaison.n,input$debutfin[1],input$debutfin[2],input$explic3,input$new.explic.3)
    
    #return(paste("Des économies de ", as.character(economie$eco), "ont été faite", sep = " "))
    return(paste("Soit une économie moyenne de : ", as.character(round(eco$moyenne))," / Mois")
           )
    
  })
  

}

shinyApp(ui, server)
