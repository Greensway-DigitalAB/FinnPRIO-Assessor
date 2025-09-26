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
    # h4(question),
    h4(glue("{tag} {qid}: {question}")),
    fluidRow(
      column(width = 5,
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
  )
}

render_quest <- function(tag, qid, question, options, type){
  if(type == "minmax"){
    tagList(
      # h4(question),
      h4(glue("{tag} {qid}: {question}")),
      fluidRow(
        column(width = 5,
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
    )
  } else {
    tagList(
      # h4(question),
      h4(glue("{tag} {qid}: {question}")),
      fluidRow(
        column(width = 5,
               br(),
               tags$ul(
                 lapply(options, function(x) tags$li(x))
               )
        ),
        column(width = 1,
               awesomeCheckboxGroup(
                 inputId = glue("{tag}Q_{qid}_min"),
                 label = "Minimum",
                 choices = structure(options, names = rep(" ", length(options)) ))
        ),
        column(width = 1,
               awesomeCheckboxGroup(
                 inputId = glue("{tag}Q_{qid}_likely"),
                 label = "Likely",
                 choices = structure(options, names = rep(" ", length(options)) ))
        ),
        column(width = 1,
               awesomeCheckboxGroup(
                 inputId = glue("{tag}Q_{qid}_max"),
                 label = "Maximum",
                 choices = structure(options, names = rep(" ", length(options)) ))
        )
      )
    )
  }
  
}



# render_minlikelymax2 <- function(tag, qid, question, options){
#   tagList(
#     h4(question),
#     lapply(options, function(x){
#       radioGroupButtons(
#         inputId = glue("{tag}Q_{qid}_2"),
#         label = x,
#         choices = c("Min", "Likely", "Max"),
#         justified = TRUE,
#         checkIcon = list(yes = icon("ok", lib = "glyphicon"))
#       )
#       # radioButtons(glue("{tag}Q_{qid}_2"),
#       #              x,
#       #              choices = c("Min", "Likely", "Max"),
#       #              inline = TRUE)
#     })
# 
# 
#   )
# }

render_minlikelymax_tab <- function(tag, qid, question, options){
  # input_names <- glue("{tag}{qid}_{options}")  
  input_names <- glue("{tag}{qid}_{options}")
  values <- c("Minimum", "Likely", "Maximum")

  table_data = matrix(
    values, nrow = length(options), ncol = length(values), byrow = TRUE,
    dimnames = list(input_names, values)
  )
  
  for (i in seq_len(nrow(table_data))) {
    table_data[i, ] = sprintf(
      '<input type="checkbox" name="%s" value="%s"/>',
      input_names[i], table_data[i, ])
  }
  
  tagList(
    # h4(glue("{tag} {qid}: {question}")),
    datatable(
      # table_data
      cbind(options,table_data),
      colnames = c("Options", "Minimum", "Likely", "Maximum"),
      editable = TRUE,
      escape = FALSE,   # allow HTML rendering
      selection = "none", 
      # server = FALSE,
      rownames = TRUE,
      options = list(dom = 't', 
                     paging = FALSE, 
                     autoWidth = FALSE,
                     ordering = FALSE,
                     columnDefs = list(
                       list(width = '50px', targets = c(2,3,4)),
                       list(visible = FALSE, targets = c(0)))
                     ),
      callback = JS("
        table.rows().every(function(i, tab, row) {
          var $this = $(this.node());
          $this.attr('id', this.data()[0]);
          $this.addClass('shiny-input-checkboxgroup');
        });

        Shiny.unbindAll(table.table().node());
        Shiny.bindAll(table.table().node());

        var limits = { Minimum: 1, Likely: 1, Maximum: 1 };
        var counts = { Minimum: 0, Likely: 0, Maximum: 0 };

        $('input[type=checkbox]').off('change').on('change', function() {
          var checkbox = this;
          var value = checkbox.value;

          var totalChecked = $('input[type=checkbox][value=' + value + ']:checked').length;

          if (totalChecked > limits[value]) {
            console.warn('Limit reached for ' + value);
            $(checkbox).prop('checked', false);
          }
        });
      ")
    )
  )
}

render_quest_tab <- function(tag, qid, question, options, texts, type = "minmax"){
  input_names <- glue("{tag}{qid}_{options}")
  input_text <- glue("{tag}{qid}_{texts}")
  values <- c("Minimum", "Likely", "Maximum")
  
  table_data = matrix(
    values, nrow = length(options), ncol = length(values), byrow = TRUE,
    dimnames = list(input_names, values)
  )
  
  for (i in seq_len(nrow(table_data))) {
    table_data[i, ] = sprintf(
      '<input type="checkbox" name="%s" value="%s"/>',
      input_names[i], table_data[i, ])
  }
  
  colnames <- if (type == "minmax") {
    c("Options", "Minimum", "Likely", "Maximum")
  } else {
    c("Sub-questions, check the box if the answer is Yes", "Minimum", "Likely", "Maximum")
  }
  # JavaScript callback: conditional based on type
  
  js_callback <- if (type == "minmax") {
    JS("
      table.rows().every(function(i, tab, row) {
        var $this = $(this.node());
        $this.attr('id', this.data()[0]);
        $this.addClass('shiny-input-checkboxgroup');
      });

      Shiny.unbindAll(table.table().node());
      Shiny.bindAll(table.table().node());

      var tableId = table.table().node().id || 'table_' + Math.random().toString(36).substr(2, 9);
      var limits = { Minimum: 1, Likely: 1, Maximum: 1 };

      $('#' + tableId + ' input[type=checkbox]').off('change').on('change', function() {
        var checkbox = this;
        var value = checkbox.value;

        var totalChecked = $('#' + tableId + ' input[type=checkbox][value=' + value + ']:checked').length;

        if (totalChecked > limits[value]) {
          console.warn('Limit reached for ' + value);
          $(checkbox).prop('checked', false);
        }
      });
    ")
  } else {
    JS("
      table.rows().every(function(i, tab, row) {
        var $this = $(this.node());
        $this.attr('id', this.data()[0]);
        $this.addClass('shiny-input-checkboxgroup');
      });

      Shiny.unbindAll(table.table().node());
      Shiny.bindAll(table.table().node());
    ")
  }

  tagList(
    # h4(glue("{tag} {qid}: {question}")),
    datatable(
      cbind(texts,table_data), #table_data,
      colnames = colnames,
      editable = TRUE,
      escape = FALSE,   # allow HTML rendering
      selection = "none", 
      # server = FALSE,
      rownames = TRUE,
      options = list(dom = 't', 
                     paging = FALSE, 
                     autoWidth = FALSE,
                     ordering = FALSE,
                     columnDefs = list(
                       list(width = '50px', targets = c(2,3,4)),
                       list(visible = FALSE, targets = c(0))
                     )
      ),
      callback = js_callback
    )
  )
}
# render_quest_tab <- function(tag, qid, question, options, type = "minmax"){
#   input_names <- glue("{tag}{qid}_{options}")
#   ifelse(type == "minmax", 
#          values <- c("Minimum", "Likely", "Maximum"),
#          values <- c("Yes", "No"))
#   # points <- c(1, 2, 3)
#   # table_data <- data.frame(
#   #     Options = options,
#   #     Minimum  = sapply(input_names, function(r) sprintf('<input type="radio" name="%s" value="Minimum">', r)),
#   #     Likely   = sapply(input_names, function(r) sprintf('<input type="radio" name="%s" value="Likely">', r)),
#   #     Maximum  = sapply(input_names, function(r) sprintf('<input type="radio" name="%s" value="Maximum">', r)),
#   #     stringsAsFactors = FALSE)
#   
#   table_data = matrix(
#     values, nrow = length(options), ncol = length(values), byrow = TRUE,
#     dimnames = list(input_names, values)
#   )
#   
#   for (i in seq_len(nrow(table_data))) {
#     table_data[i, ] = sprintf(
#       '<input type="radio" name="%s" value="%s"/>',
#       input_names[i], table_data[i, ])
#   }
#   
#   # ifelse(type == "minmax", 
#   #        table_data <- data.frame(
#   #                                 Options = options,
#   #                                 Minimum  = sapply(input_names, function(r) sprintf('<input type="radio" name="%s" value="MIN">', r)),
#   #                                 Likely   = sapply(input_names, function(r) sprintf('<input type="radio" name="%s" value="LIK">', r)),
#   #                                 Maximum  = sapply(input_names, function(r) sprintf('<input type="radio" name="%s" value="MAX">', r)),
#   #                                 stringsAsFactors = FALSE),
#   #        table_data <- data.frame(
#   #                                 Options = options,
#   #                                 Yes  = sapply(input_names, function(r) sprintf('<input type="radio" name="%s" value="TRUE">', r)),
#   #                                 No  = sapply(input_names, function(r) sprintf('<input type="radio" name="%s" value="FALSE">', r)),
#   #                                 stringsAsFactors = FALSE)
#   # )
# 
#   tagList(
#     h4(glue("{tag} {qid}: {question}")),
#     datatable(
#       table_data,
#       editable = TRUE,
#       escape = FALSE,   # allow HTML rendering
#       selection = "none", 
#       # server = FALSE,
#       rownames = TRUE,
#       options = list(dom = 't', 
#                      paging = FALSE, 
#                      autoWidth = FALSE,
#                      ordering = FALSE,
#                      columnDefs = list(
#                        list(width = '50px', 
#                             targets = ifelse(type == "minmax", c(2,3,4), c(2,3))),
#                        list(visible = FALSE, targets = c(0))
#                      )
#       ),
#       # callback = JS("table.rows().every(function(i, tab, row) {
#       #     var $this = $(this.node());
#       #     $this.attr('id', this.data()[0]);
#       #     $this.addClass('shiny-input-radiogroup');
#       #   });
#       #   Shiny.unbindAll(table.table().node());
#       #   Shiny.bindAll(table.table().node());")
#       callback = JS("
#         table.rows().every(function(i, tab, row) {
#           var $this = $(this.node());
#           $this.attr('id', this.data()[0]);
#           $this.addClass('shiny-input-radiogroup');
#         });
# 
#         Shiny.unbindAll(table.table().node());
#         Shiny.bindAll(table.table().node());
# 
#         // Create scoped counts and selections per table
#         var tableId = table.table().node().id || 'table_' + Math.random().toString(36).substr(2, 9);
#         var limits = { Minimum: 1, Likely: 1, Maximum: 1 };
#         var counts = { Minimum: 0, Likely: 0, Maximum: 0 };
#         var previousSelections = {};
# 
#         $('#' + tableId + ' input[type=radio]').on('click', function(e) {
#           var value = this.value;
#           var name = this.name;
# 
#           if (previousSelections[name] === value) return;
# 
#           if (counts[value] >= limits[value]) {
#             alert('Limit reached for ' + value);
#             e.preventDefault();
#             return false;
#           }
# 
#           if (previousSelections[name]) {
#             counts[previousSelections[name]]--;
#           }
# 
#           counts[value]++;
#           previousSelections[name] = value;
#         });
#       ")
#     )
#   )
# }

extract_answwers <- function(questions, groupTag, input){
  quesExt <- questions |> filter(group == groupTag)
  id <- quesExt$number
  input_names <- character(0)
  for(i in seq(id)){
    # options <- fromJSON(quesExt$list[i])$text
    options <- fromJSON(quesExt$list[i])$opt
    input_names <- c(input_names, glue("{groupTag}{id[i]}_{options}"))
  }
  resp <- sapply(input_names, function(i) input[[i]])
  return(resp)
}

extract_answwers_entry <- function(questions, groupTag, path, input){
  quesExt <- questions |> filter(group == groupTag)
  id <- quesExt$number
  # id <- paste0(quesExt$number, "_", rep(path, length(quesExt$number)))
  input_names <- character(0)
  for(i in seq(id)){
    # options <- fromJSON(quesExt$list[i])$text
    options <- fromJSON(quesExt$list[i])$opt
    id_p <- paste0(id[i], "_", path)
    for(p in id_p){
      input_names <- c(input_names, glue("{groupTag}{p}_{options}"))  
    }
  }
  resp <- sapply(input_names, function(i) input[[i]])
  return(resp)
}

get_points_as_table <- function(questions){
  groups <- unique(questions$group)
  # Loop over each group and parse its list column
  points_all <- lapply(groups, function(grp) {
    points <- questions |> 
      filter(group == grp) #|> 
      # pull(list)
    
    # lapply(seq_along(points), function(i) {
    #   df <- fromJSON(points[i])
    #   df$question <- paste0(grp, i)
    #   df$points <- as.character(df$points)
    #   df
    lapply(seq(1,nrow(points)), function(i) {
      df <- fromJSON(points$list[i])
      df$question <- paste0(grp, points$number[i])
      df$points <- as.character(df$points)
      df
    }) |> bind_rows()
  }) |> bind_rows()
  
  # Final formatting
  points_all <- points_all |> 
    rename(Question = question, 
           Option = opt, 
           Text = text, 
           Points = points)
  
  return(points_all)
}

get_table2_points <- function(ent2_answer, ent3_answer, table2) {
  table2 |> 
    filter(ENT2 == tolower(ent2_answer),
           ENT3 == tolower(ent3_answer)) |> 
    pull(Points)
}

get_table3_points <- function(est2_answer, est3_answer, table3) {
  table3 |> 
    filter(EST2 == tolower(est2_answer),
           EST3 == tolower(est3_answer)) |> 
    pull(Points)
}

