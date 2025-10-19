# Load UI content from a file
load_ui_content <- function(file) {
  source(file, local = TRUE)$value
}

#
capitalize_first <- function(x) {
  paste0(toupper(substr(x, 1, 1)), 
         tolower(substr(x, 2, 3)), 
         substr(x, 4, nchar(x)))
}


update_options <- function(assessors, pests, taxa, quaran, pathways, session) {
  updateSelectInput(session, "assessors", choices = setNames(c("", assessors$idAssessor), c("", assessors$fullName)))
  updateSelectInput(session, "pest", choices = setNames(c("", pests$idPest), c("", pests$scientificName)))
  updateSelectInput(session, "new_taxa", choices = setNames(taxa$idTaxa, taxa$name))
  updateSelectInput(session, "new_quaran", choices = setNames(quaran$idQuarantineStatus, quaran$name))
  updateCheckboxGroupInput(session, "pot_entry_path", choices = setNames(pathways$idPathway, pathways$name))
  updateSelectInput(session, "assessors", choices = setNames(c("", assessors$idAssessor), c("", assessors$fullName)))
  updateSelectInput(session, "filter_pest", choices = setNames(c("", pests$idPest), c("", pests$scientificName)))
  updateCheckboxGroupInput(session, "filter_entry_path", choices = setNames(pathways$idPathway, pathways$name))
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
    h5(group_name),
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

# render_minlikelymax <- function(tag, qid, question, options){
#   tagList(
#     # h4(question),
#     h4(glue("{tag} {qid}: {question}")),
#     fluidRow(
#       column(width = 5,
#              br(),
#              tags$ul(
#                lapply(options, function(x) tags$li(x))
#              )
#       ),
#       column(width = 1,
#               awesomeRadio(
#                 inputId = glue("{tag}Q_{qid}_min"),
#                 label = "Minimum",
#                 choices = structure(options, names = rep(" ", length(options)) ))
#              ),
#       column(width = 1,
#              awesomeRadio(
#                inputId = glue("{tag}Q_{qid}_likely"),
#                label = "Likely",
#                choices = structure(options, names = rep(" ", length(options)) ))
#              ),
#       column(width = 1,
#              awesomeRadio(
#                inputId = glue("{tag}Q_{qid}_max"),
#                label = "Maximum",
#                choices = structure(options, names = rep(" ", length(options)) ))
#              )
#     )
#   )
# }

# render_quest <- function(tag, qid, question, options, type){
#   if(type == "minmax"){
#     tagList(
#       # h4(question),
#       h4(glue("{tag} {qid}: {question}")),
#       fluidRow(
#         column(width = 5,
#                br(),
#                tags$ul(
#                  lapply(options, function(x) tags$li(x))
#                )
#         ),
#         column(width = 1,
#                awesomeRadio(
#                  inputId = glue("{tag}Q_{qid}_min"),
#                  label = "Minimum",
#                  choices = structure(options, names = rep(" ", length(options)) ))
#         ),
#         column(width = 1,
#                awesomeRadio(
#                  inputId = glue("{tag}Q_{qid}_likely"),
#                  label = "Likely",
#                  choices = structure(options, names = rep(" ", length(options)) ))
#         ),
#         column(width = 1,
#                awesomeRadio(
#                  inputId = glue("{tag}Q_{qid}_max"),
#                  label = "Maximum",
#                  choices = structure(options, names = rep(" ", length(options)) ))
#         )
#       )
#     )
#   } else {
#     tagList(
#       # h4(question),
#       h4(glue("{tag} {qid}: {question}")),
#       fluidRow(
#         column(width = 5,
#                br(),
#                tags$ul(
#                  lapply(options, function(x) tags$li(x))
#                )
#         ),
#         column(width = 1,
#                awesomeCheckboxGroup(
#                  inputId = glue("{tag}Q_{qid}_min"),
#                  label = "Minimum",
#                  choices = structure(options, names = rep(" ", length(options)) ))
#         ),
#         column(width = 1,
#                awesomeCheckboxGroup(
#                  inputId = glue("{tag}Q_{qid}_likely"),
#                  label = "Likely",
#                  choices = structure(options, names = rep(" ", length(options)) ))
#         ),
#         column(width = 1,
#                awesomeCheckboxGroup(
#                  inputId = glue("{tag}Q_{qid}_max"),
#                  label = "Maximum",
#                  choices = structure(options, names = rep(" ", length(options)) ))
#         )
#       )
#     )
#   }
#   
# }



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

# render_minlikelymax_tab <- function(tag, qid, question, options){
#   # input_names <- glue("{tag}{qid}_{options}")  
#   input_names <- glue("{tag}{qid}_{options}")
#   values <- c("Minimum", "Likely", "Maximum")
# 
#   table_data = matrix(
#     values, nrow = length(options), ncol = length(values), byrow = TRUE,
#     dimnames = list(input_names, values)
#   )
#   
#   for (i in seq_len(nrow(table_data))) {
#     table_data[i, ] = sprintf(
#       '<input type="checkbox" name="%s" value="%s"/>',
#       input_names[i], table_data[i, ])
#   }
#   
#   tagList(
#     # h4(glue("{tag} {qid}: {question}")),
#     datatable(
#       # table_data
#       cbind(options,table_data),
#       colnames = c("Options", "Minimum", "Likely", "Maximum"),
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
#                        list(width = '50px', targets = c(2,3,4)),
#                        list(visible = FALSE, targets = c(0)))
#                      ),
#       callback = JS("
#         table.rows().every(function(i, tab, row) {
#           var $this = $(this.node());
#           $this.attr('id', this.data()[0]);
#           $this.addClass('shiny-input-checkboxgroup');
#         });
# 
#         Shiny.unbindAll(table.table().node());
#         Shiny.bindAll(table.table().node());
# 
#         var limits = { Minimum: 1, Likely: 1, Maximum: 1 };
#         var counts = { Minimum: 0, Likely: 0, Maximum: 0 };
# 
#         $('input[type=checkbox]').off('change').on('change', function() {
#           var checkbox = this;
#           var value = checkbox.value;
# 
#           var totalChecked = $('input[type=checkbox][value=' + value + ']:checked').length;
# 
#           if (totalChecked > limits[value]) {
#             console.warn('Limit reached for ' + value);
#             $(checkbox).prop('checked', false);
#           }
#         });
#       ")
#     )
#   )
# }

# render_quest_tab <- function(tag, qid, question, 
#                              options, texts, 
#                              type = "minmax"){
#   input_names <- glue("{tag}{qid}_{options}")
#   input_text <- glue("{tag}{qid}_{texts}")
#   values <- c("Minimum", "Likely", "Maximum")
#   
#   table_data = matrix(
#     values, nrow = length(options), ncol = length(values), byrow = TRUE,
#     dimnames = list(input_names, values)
#   )
# 
# # print(table_data)
# # opt <- options[row]
# 
#   for (i in seq_len(nrow(table_data))) {
#     is_checked <- c(FALSE, FALSE, FALSE)
#     
#     table_data[i, ] = sprintf(
#       '<input type="checkbox" name="%s" value="%s" %s/>', # the last s if for adding 'checked'
#        # '<input type="checkbox" name="%s" value="%s" checked="checked"/>', #the last s if for adding 'checked'
#       input_names[i], table_data[i, ], ifelse(is_checked, ' checked="checked"', ""))
#   }
#   
#   
#   # table_data[i, ] <- sapply(values, function(val) {
#   #   checked <- if (!is.null(prechecked[[input_names[i]]]) && val %in% prechecked[[input_names[i]]]) {
#   #     'checked="checked"'
#   #   } else {
#   #     ''
#   #   }
#   #   sprintf('<input type="checkbox" name="%s" value="%s" %s/>', input_names[i], val, checked)
#   # })
#   
#   
#   colnames <- if (type == "minmax") {
#     c("Options", "Minimum", "Likely", "Maximum")
#   } else {
#     c("Sub-questions, check the box if the answer is Yes", "Minimum", "Likely", "Maximum")
#   }
#   # JavaScript callback: conditional based on type
#   
#   js_callback <- if (type == "minmax") {
#     JS("
#       table.rows().every(function(i, tab, row) {
#         var $this = $(this.node());
#         $this.attr('id', this.data()[0]);
#         $this.addClass('shiny-input-checkboxgroup');
#       });
# 
#       Shiny.unbindAll(table.table().node());
#       Shiny.bindAll(table.table().node());
# 
#       var tableId = table.table().node().id || 'table_' + Math.random().toString(36).substr(2, 9);
#       var limits = { Minimum: 1, Likely: 1, Maximum: 1 };
# 
#       $('#' + tableId + ' input[type=checkbox]').off('change').on('change', function() {
#         var checkbox = this;
#         var value = checkbox.value;
# 
#         var totalChecked = $('#' + tableId + ' input[type=checkbox][value=' + value + ']:checked').length;
# 
#         if (totalChecked > limits[value]) {
#           console.warn('Limit reached for ' + value);
#           $(checkbox).prop('checked', false);
#         }
#       });
#     ")
#   } else {
#     JS("
#       table.rows().every(function(i, tab, row) {
#         var $this = $(this.node());
#         $this.attr('id', this.data()[0]);
#         $this.addClass('shiny-input-checkboxgroup');
#       });
# 
#       Shiny.unbindAll(table.table().node());
#       Shiny.bindAll(table.table().node());
#     ")
#   }
# 
#   tagList(
#     # h4(glue("{tag} {qid}: {question}")),
#     datatable(
#       cbind(texts, table_data), #table_data,
#       colnames = colnames,
#       editable = TRUE,
#       escape = FALSE,   # allow HTML rendering
#       width = "600px",
#       selection = "none", 
#       # server = FALSE,
#       rownames = TRUE,
#       options = list(dom = 't', 
#                      paging = FALSE, 
#                      autoWidth = FALSE,
#                      ordering = FALSE,
#                      columnDefs = list(
#                        list(width = '50px', targets = c(2,3,4)),
#                        list(visible = FALSE, targets = c(0))
#                      )
#       ),
#       callback = js_callback
#     )
#   )
# }

render_quest_tab <- function(tag, qid, question, 
                             options, texts, 
                             answers = NULL,
                             type = "minmax"){
  input_names <- glue("{tag}{qid}_{options}")
  input_text <- glue("{tag}{qid}_{texts}")
  values <- c("Minimum", "Likely", "Maximum")
  
  table_data = matrix(
    values, nrow = length(options), ncol = length(values), byrow = TRUE,
    dimnames = list(input_names, values)
  )
  
# print(answers)
# a, b, c
  
  for (i in seq_len(nrow(table_data))) {
    if (!is.null(answers)) {
      is_checked <- answers |> 
        filter(ques_tag_opt == rownames(table_data)[i]) |> 
        select(Minimum, Likely, Maximum)
    } else {
      is_checked <- c(FALSE, FALSE, FALSE)
    }
    
    table_data[i, ] = sprintf(
      '<input type="checkbox" name="%s" value="%s" %s/>', # the last s if for adding 'checked'
      # '<input type="checkbox" name="%s" value="%s" checked="checked"/>', #the last s if for adding 'checked'
      input_names[i], table_data[i, ], ifelse(is_checked, ' checked="checked"', ""))
  }
  
  
  # table_data[i, ] <- sapply(values, function(val) {
  #   checked <- if (!is.null(prechecked[[input_names[i]]]) && val %in% prechecked[[input_names[i]]]) {
  #     'checked="checked"'
  #   } else {
  #     ''
  #   }
  #   sprintf('<input type="checkbox" name="%s" value="%s" %s/>', input_names[i], val, checked)
  # })
  
  
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
      cbind(texts, table_data), #table_data,
      colnames = colnames,
      editable = TRUE,
      escape = FALSE,   # allow HTML rendering
      width = "600px",
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

extract_answers <- function(questions, groupTag, input){
  quesExt <- questions |> filter(group == groupTag)
  id <- quesExt$number
  input_names <- character(0)
  
  for (i in seq(id)) {
    # options <- fromJSON(quesExt$list[i])$text
    options <- fromJSON(quesExt$list[i])$opt
    input_names <- c(input_names, glue("{groupTag}{id[i]}_{options}"))
  }
  resp <- sapply(input_names, function(i) input[[i]])
  return(resp)
}

extract_answers_entry <- function(questions, groupTag, path, input){
  quesExt <- questions |> filter(group == groupTag)
  id <- quesExt$number
  # id <- paste0(quesExt$number, "_", rep(path, length(quesExt$number)))
  input_names <- character(0)
  for (i in seq(id)) {
    # options <- fromJSON(quesExt$list[i])$text
    options <- fromJSON(quesExt$list[i])$opt
    id_p <- paste0(id[i], "_", path)
    for (p in id_p) {
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

get_inputs_as_df <- function(answers, input){ ##, points_main
  df <- tibble(
    name = names(answers),
    question = sub("_.*", "", names(answers)),
    option = sub(".*_", "", names(answers)),
    answer = answers
  ) |>
    unnest(cols = c(answer))  # This expands each vector into separate rows
  
  final_opt <- df |> 
    select(question, answer, option) |> 
    pivot_wider(names_from = answer, values_from = option) |> 
    rename_with(tolower) |> 
    as.data.frame()
print(final_opt)  
  # if (!is.null(final_opt)) {
  #   final_opt$justification <- NA
    
    ## OBS only justifications for questions with answers are mapped
    # input_names_just <- glue("just{capitalize_first(final_opt$question)}")
      input_names_just <- names(input)[grepl("^just", names(input))]
print(input_names_just)
    respJust <- sapply(input_names_just, function(i) input[[i]])
print(respJust)


# Create a full justification dataframe
just_df <- tibble(
  question = toupper(sub("^just", "", input_names_just)),
  justification = unname(respJust)
)
print(just_df)
    # final_opt$justification <- respJust
    
    # Merge with final_opt to include all justifications
    final_opt <- full_join(final_opt, just_df, by = "question")
  # }
  
  return(final_opt)
}


get_inputs_path_as_df <- function(answers, input){ ## , points_path
  df <- tibble(
    name = names(answers),
    question = sub("_.*", "", names(answers)),
    path = lapply(str_split(names(answers), "_"), function(x) x[2]) |> unlist(),
    option = sub(".*_", "", names(answers)) |> tolower(),
    answer = answers
  ) |>
    unnest(cols = c(answer))  # This expands each vector into separate rows
  
  final_opt <- df |> 
    select(path, question, answer, option) |> 
    pivot_wider(names_from = answer, values_from = option) |> 
    rename_with(tolower) |> 
    as.data.frame()
  
  if (!is.null(final_opt)) {
    final_opt$justification <- NA
  
    ## OBS only justifications for questions with answers are mapped
    input_names_just <- glue("just{capitalize_first(final_opt$question)}_{final_opt$path}")
    respJust <- sapply(input_names_just, function(i) input[[i]])
    final_opt$justification <- respJust
  }
  return(final_opt)
}


answers_2_logical <- function(df, questions) {
  
  if (nrow(df) > 0) {
    result <- data.frame()
    
    for (i in seq_len(nrow(df))) {
      wQues <- questions |> 
        filter(idQuestion == df$idQuestion[i])
      question_tag <- paste0(wQues$group, wQues$number)
      df$question_tag[i] <- question_tag
      
      row <- df[i, ]
      options <- unique(c(row$min, row$likely, row$max))
      
      for (opt in options) {
        result <- rbind(result, data.frame(
          question_tag = row$question_tag,
          option = opt,
          ques_tag_opt = paste0(row$question_tag, "_", opt),
          Minimum = opt == row$min,
          Likely = opt == row$likely,
          Maximum = opt == row$max,
          stringsAsFactors = FALSE
        ))
      }
    }
  } else {
    result <- NULL
  }
  
  return(result)
}


### Points
# df_points <- df |> 
#   left_join(points_main, by = c("Question", "Option"))

# # Step 1: Filter EST2 and EST3
# est2 <- df_points |> filter(Question == "EST2")
# est3 <- df_points |> filter(Question == "EST3")
# 
# # Step 2: Merge EST2 and EST3 by Answer type
# merged_est2_3 <- bind_rows(est2, est3) |> 
#   group_by(Answer) |> 
#   reframe(
#     est2_options = Option[Question == "EST2"],
#     est3_options = Option[Question == "EST3"]
#   ) |> 
#   mutate(
#     Points = mapply(get_table3_points, est2_options, est3_options, 
#                     MoreArgs = list(table3 = table3_lexp)),
#     Question = "EST2+3"
#   ) |> 
#   select(Question, Answer, Points)

# Step 3: Filter out EST2 and EST3 from original data
# df_clean <- df_points  |> 
#   filter(!Question %in% c("EST2", "EST3"))  |> 
#   select(Question, Answer, Points) |> 
#   mutate(Points = as.numeric(Points))
# 
# # Step 4: Combine and pivot
# final <- bind_rows(df_clean, merged_est2_3)  |> 
#   pivot_wider(names_from = Answer, values_from = Points) |> 
#   as.data.frame()
#   
#   
#   ### Points for pathways
#    
# df_points <- df |> 
#   left_join(points_path, by = c("Question", "Option"))

# Step 1: Filter ENT3
# ent2A <- df_points |> filter(Question == "ENT2A")
# ent2B <- df_points |> filter(Question == "ENT2B")
# ent3 <- df_points |> filter(Question == "ENT3")
# 
# # Step 2: Merge ENT2 and ENT3 by Answer type
# ent2a_3 <- bind_rows(ent2A, ent3) |> 
#   group_by(Answer, Path) |> 
#   reframe(
#     ent2a_options = Option[Question == "ENT2A"],
#     ent3_options = Option[Question == "ENT3"]
#   ) |> 
#   mutate(
#     Points = mapply(get_table2_points, ent2a_options, ent3_options, 
#                     MoreArgs = list(table2 = table2_lexp)),
#     Question = "ENT3A"
#   ) |> 
#   select(Path, Question, Answer, Points)
# 
# ent2b_3 <- bind_rows(ent2B, ent3) |> 
#   group_by(Answer, Path) |> 
#   summarise(
#     ent2b_options = Option[Question == "ENT2B"],
#     ent3_options = Option[Question == "ENT3"],
#     .groups = "drop"
#   ) |> 
#   mutate(
#     Points = mapply(get_table2_points, ent2b_options, ent3_options, MoreArgs = list(table2 = table2_lexp)),
#     Question = "ENT3B"
#   ) |> 
#   select(Path, Question, Answer, Points)
# 
# 
# # Step 3: Filter out ENT3 from original data
# df_clean <- df_points  |> 
#   filter(!Question %in% c("ENT3"))  |> 
#   select(Path, Question, Answer, Points) |> 
#   mutate(Points = as.numeric(Points))
# 
# # Step 4: Combine and pivot
# final <- bind_rows(df_clean, ent2a_3, ent2b_3)  |> 
#   pivot_wider(names_from = Answer, values_from = Points) |> 
#   as.data.frame()
