shinyUI(fluidPage(
  
  includeCSS("styles.css"),
  
  headerPanel(h1("Cornélien")),
  sidebarPanel(
    selectInput("Ville", h5("Choisissez votre ville"), 
                choices = c("Lille", "Paris", "Marseille")),
    fileInput('file1', h5('Importer votre chiffre d \'affaire quotidien'),
              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
    tags$hr(),
    checkboxInput('header', 'En-tête', FALSE),
    radioButtons('sep', 'Séparateur',
                 c(Virgule=',',
                   Point_virgule =';',
                   Tabulation='\t'),
                 'Point_virgule')
    #,radioButtons('quote', 'Quote',c(None='','Double Quote'='"','Single Quote'="'"),'Double Quote')
                ),
  mainPanel(code("Voici la météo  de votre ville des trois derniers jours:"),
    tableOutput("view2"),
     #verbatimTextOutput("summary"),
    code("Voici un résumé de votre chiffre d'affaires:"),
      tableOutput("view"),
      tableOutput("contents"),
     plotOutput("Plot"),
    code("Prévision:"),
   verbatimTextOutput("prevision")
    )
  )
)