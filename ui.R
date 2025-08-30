ui <- function(request) {
  
  navbarPage("FinnPRIO-Assessor",
             theme = shinythemes::shinytheme("sandstone"),
             
             tabPanel("New Feature Placeholder",
                      
                      # fluidPage(
                        
                        sidebarLayout(
                          
                          # Sidebar with inputs: ----
                          sidebarPanel(
                            
                            h3(strong(style = "font-size:24px;", "Selection criteria")),
                            
                                     tags$h4(strong("Placeholder Section A"), style = "color:#7C6A56"),
                                     
                                     selectInput("user", "Select User", choices = NULL),
                                     actionButton("new_user", "+ Add User"),
                                     selectInput("pest", "Select Pest Species", choices = NULL),
                                     actionButton("new_pest", "+ Add Pest"),
                                     h4("Threatened Sectors"),
                                     uiOutput("threat_checkboxes"),
                                     actionButton("save", "Save Assessment"),
                                     
                                     tags$hr(style = "border-color: gray;"),
                                     tags$h4(strong("Placeholder Section B"), style = "color:#7C6A56"),
                                     
                                     # Another placeholder input
                                     radioButtons("placeholder_radio_1",
                                                  label = "Placeholder radio buttons",
                                                  choices = c("Yes", "No"),
                                                  selected = "Yes"
                                     )
                          ),
                          
                          # Main panel for outputs: ----
                          mainPanel(
                            # Modal UI placeholders
                            uiOutput("modal_ui"),
                            uiOutput("questionarie")
                          )
                        # )
                      )
             ),
             tabPanel("Assessments summary",
                      sidebarLayout(
                        # Sidebar with filters: ----
                        sidebarPanel(
                          
                        ),
                        mainPanel(
                          tableOutput("assessments")
                        )
                      )
             )
  )
}
