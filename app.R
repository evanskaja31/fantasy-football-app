#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(googlesheets)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
    h1(
      textOutput("nominatedPlayer")
    ),
   titlePanel("Draft Day 2K18"),
   
   # Sidebar with a slider input for number of bins 
   tabsetPanel(
     tabPanel("Overall Money Sheet",
              fluidRow(
                column(width = 8,
              dataTableOutput("overallMoneyTable")
                ), column(width = 4, 
                    conditionalPanel(
                      condition = "input.overallMoneyTable_rows_selected > 0",
                      textOutput("rosterTitle"),
                      tableOutput("roster")
                    )
                )
              )
              
              
              ),
     # tabPanel("Individual Rosters",
     #          selectInput("ownerToDisplay", label = "Select an Owner", choices = c("Evan Skaja", "Luke Zibley", "Jayden Roehl", "Jordan Albers", "Joe Wallick",
     #                                                                               "Devin Graham", "Matt Erickson", "Jack Bristol",
     #                                                                               "Brett Schulze", "Isaac Collins", "Lukas Burrington", "Max Zwach",
     #                                                                               "Max Marshall", "Tyler Knutson", "Dylan Herr", "Jared Schweiss"))
     # ),
  
              
     tabPanel("Pick History", 
              DTOutput("pickHistory")
              ),
     tabPanel("Players",
              # selectInput("players_team", "Team", choices = c("All", "ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", 
              #                                                 "CLE", "DAL", "DEN", "DET", "FA", "GB", "HOU", "IND", "JAC", "KC", "LAC", "LAR", "MIA", "MIN",
              #                                                 "NE", "NO", "NYG", "NYJ", "OAK", "PHI", "PIT", "SEA", "SF", "TB", "TEN", "WAS")),
              # selectInput("players_position", "Position", choices = c("QB", "RB", "WR", "TE", "K", "DST")),
              # checkboxInput("players_available", "Include Drafted Players"),
              DTOutput("playersTable")
              ),
     tabPanel("Admin Menu",
              conditionalPanel(condition = "output.loggedIn",
                fluidRow(
                  column(width = 4,
                    selectInput("addToOwner", label = "Select Owner", choices = c("Evan Skaja", "Luke Zibley", "Jayden Roehl", "Jordan Albers", "Joe Wallick",
                                                                                 "Devin Graham", "Matt Erickson", "Jack Bristol",
                                                                                 "Brett Schulze", "Isaac Collins", "Lukas Burrington", "Max Zwach",
                                                                                 "Max Marshall", "Tyler Knutson", "Dylan Herr", "Jared Schweiss")),
                    selectizeInput("playerName", label = "Select Player", choices = c("Select")),
                    # textInput("playerName", label = "Player Name"),
                    selectInput("position", label = "Position", choices = c("QB", "RB", "WR", "TE", "K", "DST")),
                    numericInput("price", label = "Price", min = 1, value = 1),
                    actionButton("addPlayer", label = "Add Player")),
                  column(width = 4, actionButton("resetPlayerList", label = "Reset player list"),
                         selectizeInput("playerToNominate", label = "Select Player", choices = c("Select")),
                                        actionButton("nominate", label = "Nominate Player")
                         
                  #      selectInput("removeFromOwner", label = "Select owner to remove player from", 
                  #                  choices = c("Evan Skaja", "Luke Zibley", "Jayden Roehl", "Jordan Albers", "Joe Wallick",
                  #                               "Devin Graham", "Matt Erickson", "Jack Bristol",
                  #                               "Brett Schulze", "Isaac Collins", "Lukas Burrington", "Max Zwach",
                  #                               "Max Marshall", "Tyler Knutson", "Dylan Herr", "Jared Schweiss")),
                  #      selectInput("playerToRemove", label = "Select player to remove", choices = c("Select"))
                  #     
                  #      
                        )
                       )
                
                ),
              
              
              
              conditionalPanel(condition = "!output.loggedIn", 
                               textInput("username", "Username"),
                               textInput("password", "Password"),
                               actionButton("login", "Login")
                          )
              )
   )
)


##############################################################################################




sheet <- gs_title("FantasyFootball2018");
loadData <- function(){
  data <- data.frame(gs_read_csv(sheet))
  lst <- data.frame(Position = character(), Price = integer(), Team = character(), stringsAsFactors = FALSE)
  for(index in 1:nrow(data)){
    if(index != 1){
    player <- data[index,];
    print(player)
    lst[player[,"Name"],] <- list(player[,"Position"], player[,"Price"], player[,"Team"])
    }
  }
  return(lst)
}
totalPlayers <- read.csv("PlayerList.csv")
totalPlayers[,"Available"] <- T
totalNames <- totalPlayers[,"Player"]
availablePlayers <- totalPlayers

ownerList <- c("Evan Skaja", "Luke Zibley", "Jayden Roehl", "Jordan Albers", "Joe Wallick",
               "Devin Graham", "Matt Erickson", "Jack Bristol",
               "Brett Schulze", "Isaac Collins", "Lukas Burrington", "Max Zwach",
               "Max Marshall", "Tyler Knutson", "Dylan Herr", "Jared Schweiss")
# playerList <- data.frame(Position = character(), Price = integer(), Team = character(), stringsAsFactors = FALSE)
playerList <- loadData()
num <- nrow(playerList);

react <- reactiveValues();
react$totalNumberOfPlayers <- num;
react$nominatedPlayer <- "";
names <- rownames(playerList)
for(i in 1:num){
  name <- names[i];
  totalPlayers[which(totalPlayers[,"Player"] == name), "Available"] <- F
}




# playerList["Antonio Brown",] = list("WR", 55, "Evan Skaja")
# playerList["Todd Gurley",] = list("RB", 60, "Isaac Collins")
# playerList["Russell Wilson",] = list("QB", 22, "Joe Wallick")
# playerList["Christian McCaffery",] = list("RB", 35, "Evan Skaja")







#####################################################################################################





# Define server logic required to draw a histogram
server <- function(input, output, session) {
  updateSelectizeInput(session, "playerName", choices = totalNames)
  updateSelectizeInput(session, "playerToNominate", choices = totalNames)
  
  saveData <- function(data){
    rnames <- rownames(data);
    data[,"Name"] <- rnames;
    data[,"Order Drafted"] <- c(1:nrow(data));
    data <- data[,c(5,4,1,2,3)]
    gs_add_row(sheet, 1, data)
    
  }
  user <- reactiveValues();
  #Change to FALSE to require login for adding players
  user$loggedIn <- F;

   output$overallMoneyTable <- renderDataTable({
     i <- react$totalNumberOfPlayers
     moneyTable <- data.frame(Owner = character(), MoneyRemaining = integer(), 
                              NumberOfPlayers = integer(), MaxBid = integer(), stringsAsFactors = FALSE);
     for (owner in ownerList){
       players <- playerList[which(playerList[,"Team"] == owner),];
       numberOfPlayers <- nrow(players);
       if(numberOfPlayers == 0){
          moneySpent <- 0;
       } else {
         moneySpent <- sum(players["Price"]);
       }
       moneyRemaining <- (200 - moneySpent);
       maxBid <- moneyRemaining - (12 - numberOfPlayers);
       moneyTable[nrow(moneyTable) + 1,] <- list(owner, moneyRemaining, numberOfPlayers, maxBid);
     }
     colnames(moneyTable) <- c("Owner", "Money Remaining", "Number of Players", "Max Bid");
     return(moneyTable);
   
     
   },width = 1400, rownames = F, options = list(dom = "t", pageLength = -1), selection = "single")
   #digits = 0, striped = true
   
   output$roster <- renderTable({
     i <- react$totalNumberOfPlayers;
     players <- playerList[which(playerList[,"Team"] == ownerList[input$overallMoneyTable_rows_selected]),];
     players <- players[order(-players$Price),];
     lineup <- data.frame(Position = character(), Name = character(), Price = integer(), stringsAsFactors = F);
     positionNames <- c("QB", "RB1", "RB2", "WR1", "WR2", "TE", "K", "DST", "BE1", "BE2", "BE3", "BE4", "BE5");
     positions <- c("QB", "RB", "RB", "WR", "WR", "TE", "K", "DST", "BE", "BE", "BE", "BE", "BE");
     for(i in 1:13){
       if(positions[i] == "BE"){
         if(nrow(players) == 0){
           lineup[positionNames[i],] <- list(positions[i], "", 0);
         } else {
           player <- players[1,]
           players <- players[-1,]
           lineup[positionNames[i],] <- list(positions[i], rownames(player), player$Price);
         }
       } else {
         subset <- which(players$Position == positions[i])
         if(length(subset) == 0){
           lineup[positionNames[i],] <- list(positions[i], "", 0);
         } else {
           player <- players[subset[1],]
           players <- players[-subset[1],]
           lineup[positionNames[i],] <- list(positions[i], rownames(player), player$Price);
         }
         
       }
     }
     return(lineup);
     
   }, digits = 0)
   
   observeEvent(input$addPlayer, {
     playerList[input$playerName,] <<- list(input$position, input$price, input$addToOwner);
     data <- data.frame(Position = character(), Price = integer(), Team = character(), stringsAsFactors = FALSE);
     data[input$playerName,] <- list(input$position, input$price, input$addToOwner);
     react$totalNumberOfPlayers <<- react$totalNumberOfPlayers + 1;
     ind <- which(totalPlayers$Player == input$playerName)
     # availablePlayers <<- availablePlayers[-ind,]
     totalPlayers[ind,"Available"] <<- F
     saveData(data)
   }, ignoreInit = T)
   
   
   output$loggedIn <- reactive({
     return(user$loggedIn);
   }
   )
   
   observeEvent(input$login, {
     if(input$username == "evanskaja31"){
       if(input$password == "Vikings31!"){
         user$loggedIn <- TRUE;
       }
     }
   })
   
   output$pickHistory <- renderDataTable({
     i <- react$totalNumberOfPlayers;
     if(i == 0){
       return(playerList);
     }
     playerList;
     lst <- playerList;
     lst[,"Player"] <- rownames(playerList);
     lst[,"Order Drafted"] <- c(1:nrow(lst));
     lst <- lst[,c(5,4,1,2,3)]
     return(lst);
     
   }, rownames = F)
   
   
   output$playersTable <- renderDT({
     i <- react$totalNumberOfPlayers;
     return(totalPlayers)
   }, rownames = FALSE, options = list(pageLength = 25,lengthMenu = list(c(25,50,100, -1), list("25","50","100","All"))), filter = "top")
   
   output$rosterTitle <- renderText(
     paste(ownerList[input$overallMoneyTable_rows_selected], "'s roster", sep = "")
   )
   
   
   observeEvent(input$removeFromOwner,{
     players <- rownames(playerList[which(playerList[,"Team"] == input$removeFromOwner),])
     updateSelectInput(session, "playerToRemove", choices = players)
     print(players)
   })
   
   observeEvent(input$resetPlayerList,
                {
                 playerList <<- loadData();
                 react$totalNumberOfPlayers <<- nrow(playerList);
                })
   
   
   outputOptions(output, "loggedIn", suspendWhenHidden = F)
   
   output$nominatedPlayer <- renderText(
     paste("Currently Nominated:", react$nominatedPlayer)
   )
   
   observeEvent(input$nominate,
                {react$nominatedPlayer <- input$playerToNominate
                })
   
   
   
}

#https://docs.google.com/spreadsheets/d/e/2PACX-1vQKbtm70uEY3hwmH8rvOBV1WZeYXxkl71mQ8tVqgAKUBEVpTYkt0Bbg2hph9rfrPOLC-43c_m9f2VAu/pubhtml

# Run the application 
shinyApp(ui = ui, server = server)

