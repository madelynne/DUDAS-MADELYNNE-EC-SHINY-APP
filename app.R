library(shiny)
library(curl)
library(ggplot2)
f <- curl("https://raw.githubusercontent.com/madelynne/DUDAS-MADELYNNE-EC-SHINY-APP/master/TROIforapp.csv")
TROI <- read.csv(f, header = TRUE, sep = ",", stringsAsFactors = TRUE)
g <- curl("https://raw.githubusercontent.com/madelynne/DUDAS-MADELYNNE-EC-SHINY-APP/master/MROIforapp.csv")
MROI <-read.csv(g, header = TRUE, sep = ",", stringsAsFactors = TRUE)
h <- curl("https://raw.githubusercontent.com/madelynne/DUDAS-MADELYNNE-EC-SHINY-APP/master/LROIforapp.csv")
LROI <-read.csv(h, header = TRUE, sep = ",", stringsAsFactors = TRUE)
i <- curl("https://raw.githubusercontent.com/madelynne/DUDAS-MADELYNNE-EC-SHINY-APP/master/CROIforapp.csv")
CROI <-read.csv(i, header = TRUE, sep = ",", stringsAsFactors = TRUE)
# Define the UI ----
ui <- fluidPage(
  titlePanel(h1("Hominid Navicular Trabeculae PCA"),
             h4("This shiny app allows the user to view a comparative sample of hominid naviculars using PCA. The dataset contains trabecular variables (trabecular thickness, trabecular spacing, bone volume/total volume & Degree of Anisotropy) in 4 regions of interests(ROIs) across the navicular. The user can select which ROI to view.")), 
  sidebarLayout(                         
      sidebarPanel((
      selectInput("ROI",
                  label = "Choose one of the following Regions of Interest...",
                  choices = c("Tuberosity", "Medial", "Central_Facet", "Lateral")
                               ))), 
  mainPanel(plotOutput("ROIplot")
          #,h4("Examples of Chimp Trabeculae for Four Regions of Interest"),
          #video(src = "tuberosity.mp4", height = 508, width = 506, type = "video/mp4", controls = "controls"),
          #video(src = "medial.mp4", height = 508, width = 506, type = "video/mp4", controls = "controls"),
          #video(src = "central.mp4", height = 508, width = 506, type = "video/mp4", controls = "controls"),
          #video(src = "lateral.mp4", height = 508, width = 506, type = "video/mp4", controls = "controls"
                                                                   )))
 
# Define server logic ----
server <- function(input, output) {
 
  
  datasetInput <- reactive({#this switches the dataset to the correct one for the ROI selected in the dropdown menu
    switch(input$ROI,
                   "Tuberosity" = TROI,
                   "Medial" = MROI,
                   "Central_Facet" = CROI,
                   "Lateral" = LROI
                   )
    
    
  })
  
  

  output$ROIplot <- renderPlot({#creates the chosen plot
    PCA <- prcomp(input$ROI[, 2:5], center = TRUE, scale. = TRUE)#this is the line it where the problem seems to be 
    #I seem to have the incorret number of dimensions somewhere
    data2 <- as.data.frame(PCA$x)# Extract the PCA scores for each sample into a new dataframe
    scoreData <- PCA$x
    LoadsData <- PCA$rotation# the relationship between the initial variables and the principal components
    newData <- cbind(scoreData,data.frame(datasetInput[,1])) # column 1 from our original dataset represents the grouping variable, Genus 
    colnames(newData) <- c(colnames(newData[1:5]), "Genus")
    head(newData)
    newData_Homo <- subset(newData, newData[,5]=="Homo") 
    newData_Pan <- subset(newData, newData[,5]=="Pan") 
    newData_Gorilla <- subset(newData, newData[,5]=="Gorilla")
    newData_Pongo <- subset(newData, newData[,5]=="Pongo") 
    lmin <- min(scoreData[,c(1,2)])
    lmax <- max(scoreData[,c(1,2)])
    #plotting PC1 to PC2
    plot(scoreData[,c(1,2)], type="n", xlim=c(lmin,lmax), ylim=c(lmin,lmax))
    points(newData_Homo[,1:2],pch=19, col="blue") 
    points(newData_Pan[,1:2],pch=19, col="green") 
    points(newData_Gorilla[,1:2],pch=19, col="red")
    points(newData_Pongo[,1:2], pch=19,col="yellow")
    
    text(LoadsData[,1]*11,LoadsData[,2]*11, labels=rownames(LoadsData))
    legend("topleft", legend=c("Homo", "Pan", "Gorilla", "Pongo"), col=c("blue", "green", "red", "yellow"), pch=16)
    
    
  })
  
}
#runs the app
shinyApp(ui = ui, server = server)