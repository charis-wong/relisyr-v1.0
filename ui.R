shinyUI(fluidPage(
  div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
  
  # add login panel UI function
  shinyauthr::loginUI(id = "login"),

  
  tags$head(
    tags$style(
      "
      .inlineText * {
      display: inline;
      }
      .buff * {
      padding:50px 5px 30px 5px}
      "
    )
  ),
  theme = shinytheme("flatly"),
  uiOutput("header"),
  uiOutput("body")
 
  
  ))
