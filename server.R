server <- function(input, output, session) {
  
  # Load dropdowns from DB
  update_dropdowns()
  
  
  # Threat checkboxes (pull from DB table Threatened Sectors)
  output$threat_checkboxes <- renderUI({
    threats <- dbGetQuery(con, "SELECT idThreat, name FROM threatenedSectors")
    lapply(1:nrow(threats), function(i) {
      checkboxGroupInput(
        paste0("threat_", threats$idThreat),
        label = threats$name,
        choices = c("Most Likely", "Possible")
      )
    })
  })
  
  
  # Modal for new user
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
    dbExecute(con, "INSERT INTO users(firstName, lastName, email) VALUES(?,?,?)",
              params = list(input$new_name, input$new_last, input$new_email))
    removeModal()
    update_dropdowns()
  })
  
  
  # Modal for new pest
  observeEvent(input$new_pest, {
    showModal(modalDialog(
      title = "Add New Pest Species",
      textInput("new_sci", "Scientific Name"),
      textInput("new_common", "Common Name"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_pest", "Save")
      )
    ))
  })
  
  
  observeEvent(input$confirm_pest, {
    dbExecute(con, "INSERT INTO pests(scientificName, commonName) VALUES(?,?)",
              params = list(input$new_sci, input$new_common))
    removeModal()
    update_dropdowns()
  })
  
  
  # Save Assessment
  observeEvent(input$save, {
    dbExecute(con, "INSERT INTO assessments(idUser, idPest, startDate, valid) VALUES(?,?,DATE('now'),1)",
              params = list(input$user, input$pest))
  })
  
  
  # Show saved assessments
  output$assessments <- renderTable({
    dbGetQuery(con, "SELECT * FROM assessments")
  })
}
