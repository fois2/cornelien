shinyServer(function(input, output) {
  
  villeInput <- reactive({
    switch(input$Ville,
           "Lille" <- read.csv("/Users/medjahedfouad/Dropbox/recherche/recherche/cornelien/cornelien/meteolille.csv", 
                             sep = ";"),
           "Paris" = pressure,
           "Marseille" = cars)
  })
  # Show the first "n" observations
  output$view2 <- renderTable({
    head(villeInput()[dim(villeInput())[1]:1,],3)# affichage inverse
  })
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    summary(villeInput(), 3)
  })
  
  output$view <- renderTable({
  inFile <- input$file1
  # input$file1 will be NULL initially. After the user selects and uploads a 
  # file, it will be a data frame with 'name', 'size', 'type', and 'datapath' 
  # columns. The 'datapath' column will contain the local filenames where the 
  # data can be found.
    if (is.null(inFile))
      return(NULL)
    inFile<-read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
  head(inFile)
    })
  
  output$Plot <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    inFile<-read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    plot(ts(inFile[,5]),ylab="Chiffre d'affaire quotitien")
    plot(ts(inFile[,5]),ylab="Chiffre d'affaire quotitien prédit")
    fusion<-cbind(inFile,villeInput()) 
    reg<-lm(inFile[,5]~inFile[,2]+inFile[,3]+inFile[,4]+villeInput()[,2]+villeInput()[,3]+villeInput()[,4]+villeInput()[,5],data=fusion)
    lines(reg$fitted.values,col="red")
  })
  output$prevision <- renderPrint({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    inFile<-read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    meteoprevu<-c(dim(inFile)[1])
    demain<-as.POSIXlt(inFile[,1][length(inFile[,1])])# pour ajouter journée
    demain$mday<-demain$mday+1
    new<-as.data.frame(cbind(weekdays(demain),months(demain),format(demain, format = "%Y"),15,18,6,1))
    colnames(new)<-c("jour","mois","annee","Temp.min","Temp.max","Duree.ensoleil","Hauteur.precip")
    new$Temp.min<-as.numeric(new$Temp.min)
    new$Temp.max<-as.numeric(new$Temp.max)
    new$Duree.ensoleil<-as.numeric(new$Duree.ensoleil)
    new$Hauteur.precip<-as.numeric(new$Hauteur.precip)
    reg<-lm(chiffre_affaires12~jour+mois+annee+Temp.min+Temp.max+Duree.ensoleil+Hauteur.precip,data=fusion)
    paste("Vu la météo, votre chiffre d'affaire du ",new$jour,format(demain, format = "%d"), new$mois,new$annee  ," est estimé à ", round(predict(reg,new),2))
  })
  
})