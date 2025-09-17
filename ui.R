navbarPage("FinnPRIO-Assessor",
            tabPanel("New Assessment",
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
## TODO Specify ----
                              ),
                            
                            # Main panel for outputs: ----
                            mainPanel(
                              # Modal UI placeholders
                              uiOutput("questionarie"),
                              uiOutput("modal_ui"),
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
            tabPanel("Simulation",
                      fluidPage(
                        uiOutput("simulations")
                      )
            ),
            tabPanel("All pest-species data",
                    fluidPage(
                        tableOutput("pests"),
                        uiOutput("pestsSummary")
                      )
            ),
            tabPanel("Instructions",
                     fluidPage(
                       load_ui_content("ui/instructions.R"),
                     )
            ),
           header = tagList(
             # Initialize shinyjs
             useShinyjs(),
             fluidRow(
               # style = "margin:20px; padding:10px; border:1px solid #ccc;",
               style = "margin:20px",
               column(width = 6,
                      uiOutput("file_input_ui")
                      # uiOutput("file_path_ui")
                      ),
               column(width = 2, offset = 4,
                      actionButton("unload_db", "Unload database")
                      # actionButton("save", "Save Assessment")
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
