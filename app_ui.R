library(shiny)

data <- read.csv('data/covidvaccine.csv')
select_values = colnames(data)




sidebar_content <- sidebarPanel(
  selectInput(
    "y_var",
    label = "Y Variable",
    choices = select_values,
    selected = "Speed"
  )
)
main_content <- mainPanel(
  #plotOutput("plot")
)




intro_panel <- tabPanel(
  "[Tab Title]",
  
  titlePanel("[Page Title]"),
  
  img(src = "[img source]"),
  
  p("[Summary text for page]")
)

second_panel <- tabPanel(
  "[Tab Title]",
  titlePanel("[Page Title]"),
  sidebarLayout(
    sidebar_content, main_content
  )
)





ui <- navbarPage(
  "[Nav Bar Title]",
  intro_panel,
  second_panel
)
