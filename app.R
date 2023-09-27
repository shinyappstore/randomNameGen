# Random name generator

# prepare environment
quickcode::clean()
quickcode::libraryAll(shiny, nextGenShinyApps, shinyjs, shinyStorePlus, dplyr, magrittr, clear = TRUE)

# app.version
app.version <- 0.2

# data import 80,000 person data
.data.01 <- read.csv("www/fakenames.csv")
names(.data.01) <- c("Gender",names(.data.01)[2:14])
.data.02 <- .data.01 %>%
  select(Username,GivenName,Surname,Gender,Color,Age, BloodType, NameSet, Kilograms, Pounds, Centimeters, FeetInches, CountryFull,Country)

# application UI object

ui <- fluidPage(
  # Theme: Select color style from 1-13
  style = "2",

  # add custom background
  # custom.bg.color = "brown",

  # add scripts
  tags$head(
    tags$script(src = "script.js"),
    tags$link(rel = "stylesheet", href = "style.css")
  ),

  # Header: Insert header content using titlePanel ------------
  header = titlePanel(left = paste0("Random person generator from database of 80,000 v",app.version), right = "@rpkg.net"),
  useShinyjs(), #use shiny js
  initStore(), #use shinyStorePlus
  row(
    altPanel(
      card(
        title = "Key settings",
        numericInput("num_names", "Number of Persons:", value = 1, min = 1, max = 10),
        selectInput("gender", "Gender:", choices = c("Any", "Male", "Female")),
        selectInput("raceset", "Race:", choices = c("Any", unique(.data.02$NameSet))),
        hr(),
        div("Note: ShinyStorePlus preserves your entries from previous sessions. These are not real people, just a generated mixture based on www.fakenamegenerator.com")
      ),
      card(
        title = "Other settings",
        collapsed = TRUE,
        numericInput("agemin", "Minimum Age:", value = 15, min = 1, max = 200),
        numericInput("agemax", "Maximum Age:", value = 100, min = 2, max = 200),
        selectInput("bloodtype", "Blood type:", choices = c("Any", unique(.data.02$BloodType))),
        numericInput("weightavg", "Average weight (kg):", value = 50, min = min(.data.02$Kilograms ), max = max(.data.02$Kilograms )),
        numericInput("weightht", "Average height (cm):", value = 50, min = min(.data.02$Centimeters ), max = max(.data.02$Centimeters ))
      )

    ),
    mainPanel(
      actionButton("generate", "▶ Generate random persons", size = "l", bg.type = "warning"),
      downloadButton("downloadData", "Download", style="background-color: #666!important; color: white; padding: 14px!important; border-width: 1px; border-radius: 0px; background: unset;"),
      card(
      title = "Outputs of Random Names",
      h2(tableOutput("names")),
      shiny::tags$span(id="messageRandomNames")
      )

    )
  )
)





# application Server function

server <- function(input, output, session) {
  # Declare variable to hold results
  resultsVar <- reactiveVal(.data.02 %>% filter(Kilograms == 999))
  remData <- data.frame()

  # Generate random names when the Generate button is clicked
  observeEvent(input$generate, {
    #shuffle data
    data_shuffle(.data.02)
    #gender
    if(tolower(input$gender) == "any"){
         remData <- .data.02
       }else{
         remData <- .data.02 %>% filter(Gender == tolower(input$gender))
       }

    #filter for Age
    meanage = mean(c( as.integer(input$agemin) ,as.integer(input$agemax)))
    sdage = sd(c( as.integer(input$agemin) ,as.integer(input$agemax)))
    remData <- remData %>% mutate(Age =abs(as.integer(rnorm(n(),meanage,sdage)+1)))


    #filter for race
    if(input$raceset == "Any")
      remData <- remData
    else
      remData <- remData %>% filter(NameSet == tolower(input$raceset))

    #filter for bloodtype
    if(input$bloodtype == "Any")
      remData <- remData %>% mutate(BloodType == sample(.data.02$BloodType,n()))
    else
      remData <- remData %>% mutate(BloodType == input$bloodtype)

    #filter for weight
    remData <- remData %>% mutate(Kilograms == round(rnorm(n(),as.integer(input$weightavg),10),2 ))

    #filter for height
    remData <- remData %>% mutate(Centimeters == round(rnorm(n(),as.integer(input$weightht),10),2 ))

    #reduce to number needed
    if(not.null(input$num_names))
    remData <- remData[1:as.integer(input$num_names),]

    #retrurn results
    resultsVar(remData)
  })

  # show results
  output$names <- renderTable({
    resultsVar() %>% select(Username,GivenName,Age,Gender,Color,BloodType, Kilograms, Centimeters, CountryFull)
  })


  # download it
  output$downloadData <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0("randomperson_",Sys.Date(), "_data.csv")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(resultsVar(), file)
    }
  )

  #insert at the bottom  !!!IMPORTANT
  appid = "randomnamesapp"
  setupStorage(appId = appid,inputs = TRUE)

}



shinyApp(ui, server)
