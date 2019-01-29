

setwd("C:/Users/ugrayca/Documents/Pearson/Training/R Shiny/2018 Fantasy Basebal ADP")
batting <- read.table("2017 Batting Stats BaseballReference.txt",header=T, stringsAsFactors =F,sep = "\t",comment.char = "",quote = "\"")
adp <- read.table("FantasyPros_2018_Hitter_MLB_ADP_Rankings.txt",header=T, stringsAsFactors =F,sep = "\t",comment.char = "",quote = "\"")

library(stringr)
batting$ID <- sub('(^.*\\\\)(.*$)', '\\2', batting$Name)
batting$Name <- sub("\\\\.*", "", batting$Name)
batting$Name <- gsub("[#*].*$","", batting$Name)
batting <- batting[which(batting$Lg == "AL" | batting$Lg == "NL"),] # Only want MLB players, assuming 'MLB' means minor leagues

# Create ID for batting data
batting$first <- gsub("\\s+\\S+\\S+$", '', batting$Name) #remove all info before space
batting$first <- substr(gsub("[[:punct:]]","",batting$first),1,2) #remove punctuation from first names ie A. J. 
batting$last <- sub("^\\S+\\s+", '', batting$Name) # remove all info after space will use middle name if there. 
batting$last <- substr(gsub("[[:punct:][:space:]]", '', batting$last),1,6) #remove punctuation and spaces
batting$ID <- tolower(paste0(batting$last,batting$first))
batting$ID  <- paste0(batting$ID,as.numeric(with(batting, ave(as.character(ID), ID, FUN = seq_along))))
table(duplicated(batting$ID))



adp$ADP <- round(as.numeric(rowMeans(adp[,6:11], na.rm = T)),2) #average round is a difficult string, so I just create my own average

# Separate names and create and ID variable to merge
adp$first <- gsub("\\s+\\S+$", '', adp$Player) #remove all info before space
adp$first <- substr(gsub("[[:punct:]]","",adp$first),1,2) #remove punctuation from first names ie A. J. 
adp$last <- sub("^\\S+\\s+", '', adp$Player) # remove all info after space will use middle name if there. 
adp$last <- substr(gsub("[[:punct:][:space:]]", '', adp$last),1,6) #remove punctuation and spaces
adp$ID <- tolower(paste0(adp$last,adp$first))
adp$ID  <- paste0(adp$ID,as.numeric(with(adp, ave(as.character(ID), ID, FUN = seq_along))))
duplicated(adp$ID)

adp$Positions <- gsub("LF|CF|RF", "OF", adp$Positions) #Just use OF rather than actual positions


library(dplyr)
library(tidyr)
adp <- adp %>%
  separate(Positions, paste0("P",1:7),sep=",") # Separate mixed combined positions into separate variables, 7 is the most. 

# I want to be able to be able to sort by position, so I keep duplicate records of players if they have more than 1 position
adplong <- gather(adp,Position, c("ID","Hitters","Overall","Player","Team","ESPN","CBS","Yahoo","RTS","NFBC","FT","AVG","ADP"),P1:P7, factor_key = T)
adplong$Position <- NULL
colnames(adplong)[which(names(adplong) == "c(...)")] <- "Position"
adplong <- unique(adplong[!(is.na(adplong$Position) | adplong$Position==""), ])
colnames(adplong)[which(names(adplong) == "Player")] <- "Name"

library(plyr)
batting_summary <- ddply(batting,.(Name,Age,ID),summarize,
                         G=sum(G),
                         PA=sum(PA),
                         AB=sum(AB),
                         R=sum(R),
                         H=sum(H),
                         X2B=sum(X2B),
                         X3B=sum(X3B),
                         HR=sum(HR),
                         RBI=sum(RBI),
                         SB=sum(SB),
                         CS=sum(CS),
                         BB=sum(BB),
                         SO=sum(SO),
                         TB=sum(TB),
                         GDP=sum(GDP),
                         HBP=sum(HBP),
                         SH=sum(SH),
                         SF=sum(SF),
                         IBB=sum(IBB),
                         BA=round(H/AB,3),
                         OBP=round((H+BB+HBP)/(AB+BB+HBP+SF),3),
                         SLG=round(TB/AB,3),
                         OPS=round(OBP+SLG,3),
                         number=length(ID))

# Remove non-qualified batters and pitchers
batting_qual <- batting_summary[which(!is.na(batting_summary$BA) & batting_summary$AB>31 & batting_summary$H>20),]


# setwd("C:/Users/ugrayca/Documents/Pearson/Other Research/Baseball")
# library(xlsx)
# write.xlsx(batting_qual,"batting_qual.xlsx")
adp_batting <- merge(adplong,batting_qual, by = "ID", all.x=T)

library(dplyr)
adp_batting <- adp_batting %>% mutate_each(funs(as.numeric(.)),"Age","G","PA","AB","R","H","X2B","X3B","HR","RBI","SB","CS","BB","SO","TB","GDP","HBP",
                                           "SH","SF","IBB","BA","OBP","SLG","OPS","number")
adp_batting <- adp_batting[complete.cases(adp_batting[18:41]), ]


adp_batting <- read.table("adp_batting.txt",header=T, stringsAsFactors =F,sep = ",",comment.char = "",quote = "\"")


library(shiny)

# This is only the face of the app, there is no data entered in this phase
# Define UI for miles per gallon app ----
# Define UI for miles per gallon app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("2017 Baseball Stats and 2018 Average Draft Position (ADP)"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for variable to plot against adp ----
      selectInput("Position", "Position",c(adp_batting$Position,"All")),
      # selectInput("Statistic","Statistic",choices=colnames(adp_batting[18:41])),
      selectInput("Statistic","Statistic",choices=colnames(adp_batting[18:41]))),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # # Output: Formatted text for caption ----
      # h3(textOutput("caption")),
      # 
      # Output: Plot of the requested variable against adp ----
      plotOutput("adpplot")
      
    )
  )
)


# Begin entering the acutal input and outputs (data) here
# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  require(ggplot2)
  require(dplyr)
  devtools::source_gist("524eade46135f6348140", filename = "ggplot_smooth_func.R")
  adpbatts <- reactive({if(input$Position == 'All') {
    adp_batting
  } else {
    adp_batting %>% filter(Position == input$Position)
  }})
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$mpgPlot functions
  # formulaText <- reactive({
  #   paste("ADP by ", input$Statistic)
  # })
  
  regFormula <- reactive({
    as.formula(paste(input$Statistic, '~ Position'))
  })
  
  # Return the formula text for printing as a caption ----
  # output$caption <- renderText({
  #   formulaText()
  # })
  # 
  
  # Generate a plot of the requested variable against mpg ----
  # and only exclude outliers if requested
  output$adpplot <- renderPlot({
    ggplot(adpbatts(),aes(x=ADP,y=adpbatts()[[input$Statistic]])) +
      stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) +
      geom_point(colour='red') + geom_smooth(method = "lm") + labs(x = "ADP", y = input$Statistic)
  })
  
}


shinyApp(ui, server)


