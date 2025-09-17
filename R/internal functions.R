# Load UI content from a file
load_ui_content <- function(file) {
  source(file, local = TRUE)$value
}

update_options <- function(users, pests, taxa, quaran, pathways, session) {
  updateSelectInput(session, "user", choices = setNames(c("", users$idUser), c("", users$label)))
  updateSelectInput(session, "pest", choices = setNames(c("", pests$idPest), c("", pests$scientificName)))
  updateSelectInput(session, "new_taxa", choices = setNames(taxa$idTaxa, taxa$name))
  updateSelectInput(session, "new_quaran", choices = setNames(quaran$idQuarantineStatus, quaran$name))
  updateCheckboxGroupInput(session, "pot_entry_path", choices = setNames(pathways$idPathway, pathways$name))
}

# Helper to generate UI for a group
# render_group_ui <- function(group_name, threat_groups) {
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

render_group_ui <- function(group_name, threat_groups) {
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

render_minlikelymax <- function(tag, qid, question, options){
  tagList(
    h4(question),
    fluidRow(
      column(width = 3,
             br(),
             tags$ul(
               lapply(options, function(x) tags$li(x))
             )
      ),
      column(width = 1,
              awesomeRadio(
                inputId = glue("{tag}Q_{qid}_min"),
                label = "Minimum", 
                choices = structure(options, names = rep(" ", length(options)) ))
             ),
      column(width = 1,
             awesomeRadio(
               inputId = glue("{tag}Q_{qid}_likely"),
               label = "Likely", 
               choices = structure(options, names = rep(" ", length(options)) ))
             ),
      column(width = 1,
             awesomeRadio(
               inputId = glue("{tag}Q_{qid}_max"),
               label = "Maximum", 
               choices = structure(options, names = rep(" ", length(options)) ))
             )
    )
    
  # radioButtons(glue("{tag}Q_{qid}_min"),
  #              "Minimum", 
  #              choices = options,
  #              inline = TRUE),
  # radioButtons(glue("{tag}Q_{qid}_likely"),
  #              "Likely", 
  #              choices = options,
  #              inline = TRUE),
  # radioButtons(glue("{tag}Q_{qid}_max"),
  #              "Maximum", 
  #              choices = options,
  #              inline = TRUE)
  )
}

render_minlikelymax2 <- function(tag, qid, question, options){
  tagList(
    h4(question),
    lapply(options, function(x){
      radioGroupButtons(
        inputId = glue("{tag}Q_{qid}_2"),
        label = x,
        choices = c("Min", "Likely", "Max"),
        justified = TRUE,
        checkIcon = list(yes = icon("ok", lib = "glyphicon"))
      )
      # radioButtons(glue("{tag}Q_{qid}_2"),
      #              x, 
      #              choices = c("Min", "Likely", "Max"),
      #              inline = TRUE)  
    })
    
    
  )
}

render_minlikelymax_tab <- function(tag, qid, question, options){
  # input_names <- glue("{tag}{qid}_{options}")  
  input_names <- glue("{tag}{qid}_{options}")  
  table_data <- data.frame(
      options = options,
      Minimum  = sapply(input_names, function(r) sprintf('<input type="radio" name="%s" value="Minimum">', r)),
      Likely   = sapply(input_names, function(r) sprintf('<input type="radio" name="%s" value="Likely">', r)),
      Maximum  = sapply(input_names, function(r) sprintf('<input type="radio" name="%s" value="Maximum">', r)),
      stringsAsFactors = FALSE)
  tagList(
    h4(glue("{tag}{qid}: {question}")),
    datatable(
      table_data,
      editable = TRUE,
      escape = FALSE,   # allow HTML rendering
      selection = "none", 
      # server = FALSE,
      rownames = FALSE,
      options = list(dom = 't', 
                     paging = FALSE, 
                     autoWidth = FALSE,
                     ordering = FALSE,
                     columnDefs = list(
                       list(width = '50px', targets = c(1,2,3)))
                     ),
      callback = JS("table.rows().every(function(i, tab, row) {
          var $this = $(this.node());
          $this.attr('id', this.data()[0]);
          $this.addClass('shiny-input-radiogroup');
        });
        Shiny.unbindAll(table.table().node());
        Shiny.bindAll(table.table().node());")
    )
  )
}