server <- function(input, output, session) {
  # Reactive values ####
  load <- reactiveValues(status = FALSE, timestamp = NULL)
  con <- reactiveVal()
  users <- reactiveValues(data = NULL, username = NULL)
  threats <- reactiveValues(data = NULL)
  pests <- reactiveValues(data = NULL)
  taxa <- reactiveValues(data = NULL)
  quaran <- reactiveValues(data = NULL)
  pathways <- reactiveValues(data = NULL)
  assessments <- reactiveValues(data = NULL, questionarie = NULL, selected = NULL, entry = NULL)
  questions <- reactiveValues(main = NULL, entry = NULL)
  answers <- reactiveValues(main = NULL, entry = NULL)

  
  shinyDirChoose(input, "db_folder", roots = volumes, session = session,  
                 restrictions = system.file(package = "base"), allowDirCreate = FALSE)
  shinyFileChoose(input, "db_file", roots = volumes, session = session)
  db_path <- reactiveVal(NULL)
  
  # ---- Database connection ----
  # output$file_path_ui <- renderUI({
  #   if (load$status == FALSE){
  #     tagList(
  #       shinyDirButton("db_folder", "Choose Folder", "Select folder containing SQLite DB"),
  #       br()
  #     )
  #   } else {
  #     tagList(
  #       h3("Working with", db_path(), load$timestamp),
  #       verbatimTextOutput("folder_path"),
  #       actionButton("disconnect_db", "Disconnect database")
  #     )
  #   }
  # })
  
  output$file_input_ui <- renderUI({
    if (load$status == FALSE){
      tagList(
        # fileInput("db_file", "Upload SQLite Database", accept = c(".sqlite", ".db")),
        shinyFilesButton("db_file", "Choose File", "Select file containing SQLite DB", multiple = FALSE),
        br()
      )
    } else {
      tagList(
        h3("Working with", db_path(), load$timestamp),
        actionButton("unload_db", "Unload database")
      )
    }
  })

  observeEvent(input$unload_db, {
    runjs("document.getElementById('db_file').value = ''")
    dbDisconnect(con())
    con(NULL)
    session$reload()
    load$status <- FALSE
  })
  
  
  # observeEvent(input$disconnect_db, {
  #   runjs("document.getElementById('db_file').value = ''")
  #   dbDisconnect(con())
  #   con(NULL)
  #   session$reload()
  #   load$status <- FALSE
  # })
  
  observeEvent(input$db_file, {
    file_path <- parseFilePaths(volumes, input$db_file)$datapath
    
    if (length(file_path) > 0 && file.exists(file_path)) {
      db_path(file_path)
    } else {
      db_path(NULL)
    }
    
  })
  
  # observeEvent(input$db_folder, {
  #   folder <- parseDirPath(volumes, input$db_folder)
  #   db_file <- list.files(folder, pattern = "\\.db$", full.names = TRUE)
  #   
  #   if (length(db_file) > 0) {
  #     db_path(db_file[1])  # Use the first .sqlite file found
  #   } else {
  #     db_path(NULL)
  #   }
  # })
  
  output$folder_path <- renderPrint({
    db_path()
  })
  
  # Load data from database ####
  observeEvent(db_path(), {
    # req(input$db_file)
    req(db_path())
    load$status <- TRUE
    load$timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    
    # print(db_path())
    
    withProgress({
      setProgress(.1)
      consql <- dbConnect(RSQLite::SQLite(), db_path())
      # consql <- dbConnect(RSQLite::SQLite(), dbname = input$db_file$datapath)
                          # dbname = ifelse(production,
                          #                 'data/FinnPrio_DB_v01.db',
                          #                 'data/FinnPrio_DB_v01_test.db'))
      # consqlGlobal <<- consql
      con(consql)
      if (!is.null(con())) {
        message("Connection established")
        message("You are in Neo")
        users$data <- dbReadTable(con(), "users")
        users$data$label <- paste(users$data$firstName, users$data$lastName)
        threats$data <- dbReadTable(con(), "threatenedSectors")
        pests$data <- dbReadTable(con(), "pests")
        taxa$data <- dbReadTable(con(), "taxonomicGroups")
        quaran$data <- dbReadTable(con(), "quarantineStatus")
        pathways$data <- dbReadTable(con(), "pathways")
        
        assessments$data <- dbReadTable(con(), "assessments")
        # answers$main <- dbReadTable(con(), "amswers")
        # answers$entry <- dbReadTable(con(), "pathwayAnswers")
        # entrypath$data <- dbReadTable(con(), "entryPathways")
        questions$main <- dbReadTable(con(), "questions")
        questions$entry <- dbReadTable(con(), "pathwayQuestions")
          
     } #else { stop() }
        setProgress(1)
    }, message = "Läser in bakgrund data")
  })
  
  # observe({
  #   req(con())
  # })
  
  # Load options and lists from DB
  observe({
    req(con())
    update_options(users$data, pests$data, taxa$data, quaran$data, pathways$data, session)
  })
  
  ## Pest summary ----
  # Once selected the species, show all you know about it
  output$species_summary <- renderUI({
    req(input$pest)
    
    pest_info <- pests$data |> 
      filter(idPest == input$pest)
    
    pest_threat <- dbGetQuery(con(), glue("SELECT threatGroup, name FROM threatXpest 
                                           LEFT JOIN threatenedSectors ON threatXpest.idThrSect = threatenedSectors.idThrSect
                                           WHERE idPest = {as.integer(input$pest)}"))
    pest_info$quaran <- dbGetQuery(con(), glue("SELECT name FROM quarantineStatus
                                           WHERE idQuarantineStatus = {as.integer(pest_info$idQuarantineStatus)}"))
    pest_info$taxa <- dbGetQuery(con(), glue("SELECT name FROM taxonomicGroups
                                           WHERE idTaxa = {as.integer(pest_info$idTaxa)}"))
    
    # Build card UI
    tagList(
      tags$div(class = "card", style = "padding: 20px; margin-top: 20px; border: 1px solid #ccc; border-radius: 8px;",
               tags$h3(pest_info$vernacularName),
               tags$p(strong("EPPO code: "), pest_info$eppoCode),
               tags$p(strong("GBIF uuid: "), pest_info$gbifuuid),
               tags$p(strong("Synonyms: "), pest_info$synonyms),
               tags$p(strong("Taxonomic Group: "), pest_info$taxa),
               tags$p(strong("Quarantine Status: "), pest_info$quaran),
               tags$p(strong("Precense in Europe: "), as.logical(pest_info$inEurope)),
               tags$h4("Associated Threats"),
               tags$ul(
                 if (nrow(pest_threat) > 0) {
                   lapply(1:nrow(pest_threat), function(x) {
                     tags$li(paste(pest_threat$threatGroup[x], ": ", pest_threat$name[x]))
                   })
                 }
               )
      )
    )
      
  })
  
  
  ## Modal for new user ----
  observeEvent(input$new_user, {
    showModal(modalDialog(
      title = "Add New User",
      textInput("new_name", "First Name"),
      textInput("new_last", "Last Name"),
      textInput("new_email", "Email"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_user", "Save")
      )
    ))
  })
  
  observeEvent(input$confirm_user, {
    dbExecute(con(), "INSERT INTO users(firstName, lastName, email) VALUES(?,?,?)",
              params = list(input$new_name, input$new_last, input$new_email))
    users$data <- dbReadTable(con(), "users")
    users$data$label <- paste(users$data$firstName, users$data$lastName)
    removeModal()
    # update_dropdowns()
  })
  
  
  ## Modal for new pest ----
  # Threat checkboxes (pull from DB table Threatened Sectors)
  output$threat_checkboxes <- renderUI({
    req(threats$data)
    threats <- threats$data
    
    # Assuming there's a column called 'group' to group threats
    threat_groups <- split(threats, threats$threatGroup)

    # Generate UI for each group
    group_ui <- lapply(names(threat_groups), function(group_name) {
      group_threats <- threat_groups[[group_name]]
      
      checkboxGroupInput(
        inputId = paste0("group_", group_name),
        label = group_name,
        choices = setNames(group_threats$idThrSect, group_threats$name),
        inline = FALSE
      )
    })
    
    half <- ceiling(length(group_ui) / 2)
    
    ui <- tagList(
      h4("Threatened Sectors"),
      # input_list
      fluidRow(
          column(6, group_ui[1:half]),
          column(6, group_ui[(half + 1):length(group_ui)])
      )
    )
    return (ui)
  })
  
  observeEvent(input$new_pest, {
    showModal(modalDialog(
      title = "Add New Pest Species",
      fluidRow(
        column(width = 6,
               textInput("new_sci", "Scientific Name*"),
               textInput("new_common", "Common Name"),
               textInput("new_synonyms", "Synonyms"),
               textInput("new_eppo", "EPPO code"),
               textInput("new_gbifuuid", "GBIF UUID"),
               selectInput("new_taxa", "Taxonomic Group*", choices = setNames(c("", taxa$data$idTaxa), 
                                                                              c("", taxa$data$name))),
               selectInput("new_quaran", "Quarantine Status*", choices = setNames(c("", quaran$data$idQuarantineStatus),
                                                                                  c("", quaran$data$name))),
               checkboxInput("new_ineu", "Is the pest species present in Europe?")
               ),
        column(width = 6,
               uiOutput("threat_checkboxes")
               )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_pest", "Save")
      ),
      size = "l"
    ))
  })
  
  observeEvent(input$confirm_pest, {
    # Check required fields
    required_fields <- list(
      ScientificName = input$new_sci,
      Taxa = input$new_taxa,
      QuarantineStatus = input$new_quaran
    )
    
    missing_fields <- names(required_fields)[sapply(required_fields, function(x) is.null(x) || x == "")]
    if (length(missing_fields) > 0) {
      shinyalert(
        title = "Missing Required Fields",
        text = paste("Please fill in:", paste(missing_fields, collapse = ", ")),
        type = "warning"
      )
      return()
    }
    
    # Coalesce empty inputs to NA
    new_common   <- ifelse(input$new_common == "", NA, input$new_common)
    new_eppo     <- ifelse(input$new_eppo == "", NA, input$new_eppo)
    new_gbif     <- ifelse(input$new_gbifuuid == "", NA, input$new_gbifuuid)
    new_synonyms <- ifelse(input$new_synonyms == "", NA, input$new_synonyms)
    
    # Check for duplicates
    duplicate_sci  <- tolower(input$new_sci) %in% tolower(pests$data$scientificName)
    duplicate_eppo <- (input$new_eppo != "" && input$new_eppo %in% pests$data$eppoCode)
    duplicate_gbif <- (input$new_gbifuuid != "" && input$new_gbifuuid %in% pests$data$gbifuuid)

    if (duplicate_eppo || duplicate_gbif || duplicate_sci) {
      shinyalert(
        title = "Duplicate Entry",
        text = paste(
          if (duplicate_sci) "Scientific name already exists." else NULL,
          if (duplicate_eppo) "EPPO code already exists." else NULL,
          if (duplicate_gbif) "GBIF UUID already exists." else NULL,
          sep = "\n"
        ),
        type = "error"
      )
      return()
    }

    # Insert into pests table
    res <- dbExecute(conn = con(),
                      "INSERT INTO pests(scientificName, vernacularName, eppoCode, gbifuuid,
                                        synonyms, idTaxa, idQuarantineStatus, inEurope)
                        VALUES(?,?,?,?,?,?,?,?)
                        RETURNING idPest;",
                      params = list(input$new_sci, input$new_common, input$new_eppo, input$new_gbifuuid,
                                    input$new_synonyms, input$new_taxa, input$new_quaran, as.integer(input$new_ineu)))

    pests$data <- dbReadTable(con(), "pests")
    new_id <- pests$data |> filter(scientificName == input$new_sci) |> pull(idPest)

    
    lapply(threats$threatGroup, function(group){
      print(get(glue("input$group_{group}")))
    })
    
    # idThreatSect <- c(1,2)
    # dbExecute(con(), "INSERT INTO threatXpest(idPest, idThreatSect) VALUES(?,?)",
    #           params = list(new_id, idThreatSect[1]))
    
    # Insert associated threats
    lapply(unique(threats$data$threatGroup), function(group) {
      selected_threats <- input[[paste0("group_", group)]]
      if (!is.null(selected_threats)) {
        for (threat_id in selected_threats) {
          dbExecute(con(), "INSERT INTO threatXpest(idPest, idThrSect) VALUES(?, ?)",
                    params = list(new_id, threat_id))
        }
      }
    })
    
    shinyalert(
      title = "Success",
      text = "Pest and associated threats added successfully.",
      type = "success"
    )
    
    removeModal()
    # update_dropdowns()
  })
  
  # Show species lists
  output$pests <- renderTable({
    req(pests$data)
    pests$data
  })
  
# Assessments ----
  
  # questionarie = NULL, 
  
  # Save Assessment
  observeEvent(input$save, {
    # dbExecute(con(), "INSERT INTO assessments(idUser, idPest, startDate, valid) VALUES(?,?,DATE('now'),1)",
    #           params = list(input$user, input$pest))
  })

  # Show saved assessments
  output$assessments <- renderTable({
    req(assessments$data)
    assessments$data
  })
  
  ## when selecting assessment
  # assessments$selected
  # assessments$entry <- entrypath$data <- dbReadTable(con(), "entryPathways")
  # answers$main <- dbReadTable(con(), "amswers")
  # answers$entry <- dbReadTable(con(), "pathwayAnswers")
}
