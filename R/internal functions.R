update_options <- function(users, pests, taxa, quaran, pathways, session) {
  updateSelectInput(session, "user", choices = setNames(c("", users$idUser), c("", users$label)))
  updateSelectInput(session, "pest", choices = setNames(c("", pests$idPest), c("", pests$scientificName)))
  updateSelectInput(session, "new_taxa", choices = setNames(taxa$idTaxa, taxa$name))
  updateSelectInput(session, "new_quaran", choices = setNames(quaran$idQuarantineStatus, quaran$name))
  updateCheckboxGroupInput(session, "pot_entry_path", choices = setNames(pathways$idPathway, pathways$name))
}

# Helper to generate UI for a group
# renderGroupUI <- function(group_name, threat_groups) {
#   group_threats <- threat_groups[[group_name]]
#   tagList(
#     tags$h5(group_name),
#     lapply(1:nrow(group_threats), function(i) {
#       radioButtons(
#         inputId = paste0("threat_", group_threats$idThrSect[i]),
#         label = group_threats$name[i],
#         choices = c("None", "Most Likely", "Possible"),
#         inline = TRUE
#       )
#     })
#   )
# }

renderGroupUI <- function(group_name, threat_groups) {
  group_threats <- threat_groups[[group_name]]
  tagList(
    tags$h5(group_name),
    lapply(1:nrow(group_threats), function(i) {
      radioButtons(
        inputId = paste0("threat_", group_threats$idThrSect[i]),
        label = group_threats$name[i],
        choices = c("None", "Most Likely", "Possible"),
        inline = TRUE
      )
    })
  )
}