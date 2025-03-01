shinyUI(fluidPage(
  
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
