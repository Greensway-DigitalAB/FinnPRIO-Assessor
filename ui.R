navbarPage("FinnPRIO-Assessor",
            tabPanel("New Feature Placeholder",
                        # fluidPage(
                          
                          sidebarLayout(
                            
                            # Sidebar with inputs: ----
                            sidebarPanel(
                              tags$h4(strong("User information"), style = "color:#7C6A56"),
                              selectInput("user", "Select User", choices = NULL),
                              actionButton("new_user", "+ Add User"),
                              
                              h3(strong(style = "font-size:24px;", "Section Title placeholder")),
                              tags$h4(strong("Pest Information"), style = "color:#7C6A56"),
                              
                              selectInput("pest", "Select Pest Species", choices = NULL),
                              actionButton("new_pest", "+ Add Pest"),
                              uiOutput("species_summary"),
                              br(), 
                              
                              tags$hr(style = "border-color: gray;"),
                              tags$h4(strong("Entry Pathways"), style = "color:#7C6A56"),
                                       
                              # Entry pathways 
                              textAreaInput("pot_entry_path_text",
                                            label = "Potential entry pathways",
                                            resize = "vertical"),
                              checkboxGroupInput("pot_entry_path",
                                           label = "Select potential entry pathways to assess",
                                           choices = NULL,
                                           inline = FALSE)
                              ),
                            
                            # Main panel for outputs: ----
                            mainPanel(
                              # Modal UI placeholders
                              uiOutput("modal_ui"),
                              uiOutput("questionarie"),
                              actionButton("save", "Save Assessment")
                            )
                          # )
                        )
            ),
            tabPanel("Assessments summary",
                        sidebarLayout(
                          # Sidebar with filters: 
                          sidebarPanel(

                          ),
                          mainPanel(
                            tableOutput("assessments"),
                            uiOutput("assessmentSummary")
                          )
                        )
            ),
            tabPanel("Species data",
                    fluidPage(
                        tableOutput("pests"),
                        uiOutput("pestsSummary")
                      )
            ),
           header = tagList(
             # Initialize shinyjs
             useShinyjs(),
             fluidRow(
               # style = "margin:20px; padding:10px; border:1px solid #ccc;",
               style = "margin:20px",
               column(width = 4,
                 uiOutput("file_input_ui")
                 # uiOutput("file_path_ui")
                 )
               ) 
             ),
           # footer = tagList(
           #   fluidRow(
           #     style = "margin:20px",
           #     column(width = 4,
           #            
           #            )
           #     )
           #   ),
           theme = shinythemes::shinytheme("sandstone")
)
