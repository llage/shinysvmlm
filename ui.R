library(shiny)

shinyUI(fluidPage(
  
  # App title
  titlePanel("Support vector machines vs linear models - mtcars data"),
  
  # Sidebar
  sidebarLayout(
  sidebarPanel(
       selectInput("choosemodel",
                   "Model to use",
                   c("svm","lm"),
                   selected = "svm",
       	    selectize=F),
       selectInput("predictors",
       	    "Predictors to use (hold Ctrl or Shift to select multiple)",
       	    colnames(mtcars)[-c(6,7)],
       	    multiple = TRUE,
       	    selected = c("mpg","gear"),
       	    size=9,
       	    selectize=F
	)
  ),
    
    mainPanel(
    	tabsetPanel(type="tabs",
	tabPanel("Plot", plotOutput("shownplot",width="90%",height="700px"),
		 textOutput("howto")),
       	tabPanel("mtcars",tableOutput("full")),
	tabPanel("Help",textOutput("help")),
	tabPanel("Credits",textOutput("credits"))
                )
       	
       )
    )
  )
)
