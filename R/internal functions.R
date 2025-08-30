update_dropdowns <- function() {
  users <- dbGetQuery(con, "SELECT idUser, firstName || ' ' || lastName as label FROM users")
  pests <- dbGetQuery(con, "SELECT idPest, scientificName FROM pests")
  updateSelectInput(session, "user", choices = setNames(users$idUser, users$label))
  updateSelectInput(session, "pest", choices = setNames(pests$idSpp, pests$scientificName))
}
