# (0) Documentation  ===========================================================
# 
# This script implements the papers shiny app which adds and edits
# paper entries in the paper_assign workbook.
# 
# CODE OVERVIEW
# 1. Load libraries (USER SPECIFIC)
# 2. Set up paths to data (stored on Google drive) (USER SPECIFIC)
# 3. Prepares data used by the app
# 4. Defines User Interface and Server of the "Adding Papers" module
# 5. Defines User Interface and Server of the "Screening Papers" module
# 6. Defines User Interface and Server of the "Assigning Papers" module
# 7. Defines User Interface and Server of the "Home" module
# 8. Defines the User Interface and Server of the Application
# 9. Runs the Papers Shiny Application
# 
# DATA
# 1. Google Folder Containing PDFs of All Papers in paper_assign, (dev_papers)
# 2. "paper_assign" Google Workbook Containing the Sheets: 
#    "assignment", "assign_seed_tracker", "staff"

# (1) Load libraries  ==========================================================
    
    ## Loads R Libraries
    library(shiny)
    library(shinyjs)
    library(googledrive)
    library(googlesheets4)
    library(shinydashboard)
    library(tabulapdf)
    library(tidyverse)
    library(tokenizers)
    library(textreuse)
    library(shinyWidgets)
    library(htmlwidgets)
    library(crayon)
    library(DT)
    library(readxl)
    library(bslib)
    library(xlsx)
    library(R.utils)
    library(digest)
    library(pdftools)
    library(dplyr)

# (2) Helper functions =========================================================

# evaluates a string as if a command
evaluate_str <- function(string) { 
         return (eval(parse(text=string)))
     }

# returns true if A==B
# previously used (FALSE & FALSE) which returns FALSE, but want TRUE
# do xnor(FALSE, FALSE) now, which gives TRUE
xnor <- function(a, b) { 
  return(!(a != b))  
}
    ## Run install.packages("package_name") if not already installed

# (3) Set Up file paths in Drive  ====================================================
    

    # Specify account to be used to access data

       gs4_auth()

     ### Prompts authentication process and generates token used for rest of the
     ### script to access data


    ## Specify names of directories and files on Google Drive accessible by account

  #   dev_papers_dir = "Enter Folder Directory Path Here"
  #   paper_assign_filename <- "Enter File Directory Path Here"
    
    
    dev_papers_dir = "Study Assignment/paper_assign_publicTemplate/Papers"
    paper_assign_filename <- "Study Assignment/paper_assign_publicTemplate/paper_assign_template"
    
    ### Save paths to drive directories
    dev_papers_path <- drive_get(dev_papers_dir)
  
    
# (4) Defines User Interface and Server of the "Adding Papers" module ==========

    ## UI Inputs (UI elements that need to be prepared by the server)
    ## > studies :: Table displaying studies read by the app
    ## > username_UI :: username select input
    ## > input_type :: studies input select input
    ## > input_ui :: UI for manual/csv input (add one by one via buttons or upload csv)
    ## > csv_input_help_text :: help text for csv input widget
    ## > start_version_button :: button to begin versioning
    ## > apply_action_button :: button to add/discard new studies to database
    
    ## UI Outputs (Values produced by user inputs through the UI)
    ## > username :: full name of user
    ## > input_type :: manual or csv
    ## > add_row :: user clicking "add row"
    ## > uploaded_file :: a csv of new studies uploaded by user
    ## > PDF_i :: pdf uploaded by user
    ## > confirm :: output of "confirm button"
    ## > studies_cell_edit :: user edits on the new studies table
    ## > start_version :: user clicks on "Version" button
    ## > apply_action :: user clicks on "Apply Actions" button
    ## > action_i :: action specified by user for each new study
    
    ## Reactive Values
    ## > new_studies_n() :: int :: number of new studies
    ## > version_results() :: list :: versioning results
    ## > main_react() :: df :: 'assignment' sheet in database
    ## > staff() :: df :: 'staff' sheet in database
    ## > versioning_complete() :: bool :: if versioning is completed
    ## > studies() :: df :: new studies

    ## (4A) Defining User Interface ============================================
    
    ui_adding_papers <- function(id){
        ns_adding <- NS(id)
        fluidPage(
            useShinyjs(),
            tags$script(
                HTML(
                    'setTimeout(() => $(".shiny-bound-input[type=\'file\']").css("all","unset").css("display", "none"), 750);'
                )
                ),
            tags$head(
                tags$style(HTML("table {table-layout: fixed;"))
                ),
            sidebarLayout(
                sidebarPanel(
                    uiOutput(ns_adding("username_UI")),
                    selectizeInput(inputId = ns_adding("input_type"),
                                   label = "Studies Input Method",
                                   choices = c("Manual", "csv"),
                                   multiple = FALSE,
                                   selected = "Manual"),
                    uiOutput(ns_adding("input_help_text")),
                    uiOutput(ns_adding("input_ui")),
                    uiOutput(ns_adding("csv_input_help_text")),
                    br(),
                    uiOutput(ns_adding("start_version_button")),
                    helpText("Compare uploaded PDFs content against linked database"),
                    uiOutput(ns_adding("apply_action_button")),
                    helpText("Modify linked database with respect to specified actions")
                ),
            mainPanel(
                DTOutput(ns_adding("studies")),
                uiOutput(ns_adding("studies_input_help_text")),
            )
            )
        )}

    ## (4B) Defining Server Logic ==============================================

    server_adding_papers <- function(id, shared_data){
        moduleServer(id, function(input, output, session){
            
            # Helper function to render enabled or disabled buttons
            render_button <- function(button_name,
                                      label,
                                      status){
                if (status == "enable") {
                    return(renderUI({actionButton(session$ns(button_name), 
                                                  label,
                                                  style = "width: 100%; font-size: 12px; color: white; background-color: #4e2a85;")}))
                }
                else if (status == "disable") {
                    return(renderUI({disabled(actionButton(session$ns(button_name), 
                                                           label,
                                                           style = "width: 100%; font-size: 12px;"))}))
                }
            }
            
            # Helper function to reset sidebar UI components (except username)
            reset_sidebar <- function() {
                new_studies_n(0)
                version_results(list())
                versioning_complete(FALSE)
                
                output$start_version_button = render_button("start_version",
                                                            "Version",
                                                            "disable")
                output$apply_action_button = render_button("apply_action",
                                                           "Apply Actions",
                                                           "disable")
            }

            # Helper function to render Datatable
            render_datatable <- function(df,
                                         title,
                                         editable_cols,
                                         hide_cols) {
                if (length(editable_cols)==0) {
                    return(datatable(
                        df, 
                        escape = FALSE, 
                        selection = 'none', 
                        rownames = FALSE,
                        caption = title,
                        options = list(dom = 't', 
                                       paging = FALSE,
                                       ordering = FALSE,
                                       columnDefs = list(list(targets = "_all", className = "dt-center"),
                                                         list(targets = hide_cols, visible = FALSE)),
                                       initComplete = JS("function(){$(this).addClass('compact'); $(this.api().table().header()).css({'background-color': '#4e2a85', 'color': 'white'}); }"),
                                       preDrawCallback = DT::JS("function(settings) {
                                                                 Shiny.unbindAll(this.api().table().node());
                                                                 }"),
                                       drawCallback = DT::JS("function(settings) {
                                                              Shiny.initializeInputs(this.api().table().node());
                                                              Shiny.bindAll(this.api().table().node());
                                                          }")
                                       )))
                }
                else {
                    return(datatable(
                        df, 
                        escape = FALSE, 
                        selection = 'none', 
                        rownames = FALSE,
                        caption = title,
                        editable = list(
                                target = 'cell', 
                                disable = list(columns = editable_cols)
                                ),
                        options = list(dom = 't', 
                                       paging = FALSE,
                                       ordering = FALSE,
                                       columnDefs = list(list(targets = "_all", className = "dt-center"),
                                                         list(targets = hide_cols, visible = FALSE)),
                                       initComplete = JS("function(){$(this).addClass('compact'); $(this.api().table().header()).css({'background-color': '#4e2a85', 'color': 'white'}); }"),
                                       preDrawCallback = DT::JS("function(settings) {
                                                                 Shiny.unbindAll(this.api().table().node());
                                                                 }"),
                                       drawCallback = DT::JS("function(settings) {
                                                              Shiny.initializeInputs(this.api().table().node());
                                                              Shiny.bindAll(this.api().table().node());
                                                          }")
                        )))}
            }
            
            ### Server outputs that depend on user inputs
            ### Defined as a reactive value and set as NULL at first
            studies <- reactiveVal(data.frame(`Last Name` = character(0),
                                              `Year` = character(0),
                                              `Title` = character(0),
                                              `PDF` = character(0), 
                                              `Similarity` = character(0),
                                              `Action` = character(0),
                                              check.names = FALSE))
            new_studies_n = reactiveVal(0)
            version_results = reactiveVal(list())
            main_react <- reactiveVal(isolate(shared_data$main_online))
            staff = reactiveVal(isolate(shared_data$staff_online))
            subsup <- as.vector(na.omit(staff()[staff()$position == "Sub-Supervisor", ]$name))
            sup <- as.vector(na.omit(staff()[staff()$position == "Supervisor", ]$name))
            versioning_complete <- reactiveVal(FALSE)
            
            ### Render initial UI features
            output$username_UI = renderUI({selectInput(inputId = session$ns("username"),
                                                    label = "Please input your name",
                                                    choices = c("", sup, subsup),
                                                    selected = "",
                                                    multiple = FALSE,
                                                    selectize = TRUE,
                                                    width = NULL,
                                                    size = NULL)})
            output$input_help_text = renderUI({helpText("Select ", tags$em("Manual"), " to add studies one by one or ", tags$em("csv"), " to add studies by batch")})
            

            output$start_version_button = render_button("start_version",
                                            "Version",
                                            "disable")
            output$apply_action_button = render_button("apply_action",
                                            "Apply Actions",
                                            "disable")
            
            
            # When user chooses an input type
            observeEvent(input$input_type, {
                
                versioning_complete(FALSE)
                
                if (input$input_type == "Manual") {
                    output$input_ui <- renderUI({
                        fluidRow(
                            column(6, actionButton(session$ns("add_row"), 
                                                   "Add Row",
                                                   style = "width: 100%; font-size: 11px; padding: 1px;")),
                            column(6, actionButton(session$ns("subtract_row"), 
                                                   "Delete Row",
                                                   style = "width: 100%; font-size: 11px; padding: 1px;"))
                            )
                    })
                    output$csv_input_help_text <- NULL
                }
                else if (input$input_type == "csv") {
                    output$input_ui <- renderUI({
                        fileInput(session$ns("uploaded_file"), "File upload",
                                  multiple = FALSE,
                                  accept = c(".csv"))
                    })
                    output$csv_input_help_text <- renderUI({tagList(
                        helpText("csv must satisfy the following requirements:",
                                 tags$ul(
                                     tags$li("Columns: Last name, Year, Title"),
                                     tags$li("No missing or empty values")
                                 )
                                                                     ))})
                }
                reset_sidebar()
                })
        
            # When user adds studies
            observeEvent(list(input$add_row,input$subtract_row,input$input_type), {
                
                versioning_complete(FALSE)
                
                new_studies_n(max(input$add_row - input$subtract_row, 0))
                
                if (new_studies_n() > 0 && input$input_type == "Manual") {
                    studies(data.frame(`Last Name` = character(0),
                                       `Year` = character(0),
                                       `Title` = character(0),
                                       `PDF` = character(0),
                                       `Similarity` = character(0),
                                       `Action` = character(0),
                                       check.names = FALSE))
                    
                studies_static <- studies()
                    
                # Render Datatable with file input
                # Avoid using a studies() (reactive) in render_datatable() to
                # prevent re-rendering everytime studies() is changed
                for (i in 1:new_studies_n()) {
                    studies_static <- studies_static %>%
                        add_row(
                            `Last Name` = "",
                            `Year` = "",
                            `Title` = "",
                            `PDF` = as.character(fileInput(session$ns(paste0("PDF_", i)), "Upload File", accept = c(".pdf"), width = "120px", buttonLabel = "...")),
                            `Similarity` = "",
                            `Action` = as.character(selectInput(session$ns(paste0("action_", i)), "" , c("Discard", "New Entry", "Replace"), width="100%"))
                        )
                }
                studies(studies_static)
                
                # Add conditional formatting styles
                js <- "(value === '' || value === null) ? '#e6e6e6' : (/Old /).test(value) ? '#fcf9e5' : (/New /).test(value) ? '#DEEBF7' : (/Newer /).test(value) ? '#C6DBEF' : ''"
                output$studies <- renderDT({
                    render_datatable(
                        studies_static,
                        "New Studies",
                        c(4),
                        integer(0)) %>%
                    formatStyle(
                        columns = names(studies_static),
                        fontSize = '12px'
                    ) %>%
                    formatStyle("Similarity",
                                backgroundColor=JS(js))
                })
                output$studies_input_help_text <- renderUI({
                    helpText(tags$em("You can only replace studies on which no coders have been assigned and for which a newer version has been identified"), tags$br(),
                             tags$em("No accents should be added to author names and paper titles to avoid downstream incompatibilities"))
                })
                }
                else { # don't render studies table if input$add - input$subtract <= 0
                    new_studies_n(0)
                    studies(data.frame(`Last Name` = character(0),
                                              `Year` = character(0),
                                              `Title` = character(0),
                                              `PDF` = character(0), 
                                              `Similarity` = character(0),
                                              `Action` = character(0),
                                              check.names = FALSE))
                    output$studies <- NULL
                    output$studies_input_help_text <- NULL
                }
                output$start_version_button = render_button("start_version",
                                                "Version",
                                                "disable")
                output$apply_action_button = render_button("apply_action",
                                                "Apply Actions",
                                                "disable")
                })
            
            # When user uploads a csv of new studies
            observeEvent(input$uploaded_file, {

                versioning_complete(FALSE)
                
                invalid_upload = T
                tryCatch(
                    expr = {
                        fail = "file cannot be read as csv"
                        uploads <- read.csv(input$uploaded_file$datapath)
                        fail = "file does not have 3 columns"
                        stopifnot(ncol(uploads) == 3)
                        fail = "file is not csv"
                        stopifnot(grepl(".csv", input$uploaded_file$datapath))
                        fail = "file is incompletely filled out or contains missing values"
                        stopifnot(sum(uploads == "" | is.na(uploads)) == 0)
                        invalid_upload = F
                    },
                    error = function(e) {
                        msg = sprintf("Error: %s. File is not accepted, please try again \n", fail)
                        cat(yellow(msg))
                        showNotification(msg, type = "error")
                        return()
                    }
                )
                if (invalid_upload == T) {
                    return()
                }
                # Replace all instances of " to ' to avoid unintentional ending
                # while parsing a paper title
                uploads <- data.frame(lapply(uploads, function(x) {
                    gsub("\"", "'", as.character(x))}))

                # Process uploaded csv with new studies
                colnames(uploads) = c("Last Name", "Year", "Title")
                
                uploads$Title = str_to_title(uploads$Title)

                # Update reactive variable "data" to be uploaded studies
                new_studies_n(nrow(uploads))
                
                if (new_studies_n() > 0) {
                    studies(data.frame(`Last Name` = character(0),
                                       `Year` = character(0),
                                       `Title` = character(0),
                                       `PDF` = character(0),
                                       `Similarity` = character(0),
                                       `Action` = character(0),
                                       check.names = FALSE))
                    
                studies_static <- studies()
                
                # Render Datatable with file input
                for (i in 1:new_studies_n()) {
                    studies_static <- studies_static %>%
                        add_row(
                            `Last Name` = uploads$`Last Name`[i],
                            `Year` = uploads$`Year`[i],
                            `Title` = uploads$`Title`[i],
                            `PDF` = as.character(fileInput(session$ns(paste0("PDF_", i)), "Upload File", accept = c(".pdf"), width = "120px", buttonLabel = "...")),
                            `Similarity` = "",
                            `Action` = as.character(selectInput(session$ns(paste0("action_", i)), "" , c("Discard", "New Entry", "Replace"), width="100%"))
                        )
                }
                studies(studies_static)

                # Display read studies as a table
                js <- "(value === '' || value === null) ? '#e6e6e6' : (/Old /).test(value) ? '#fcf9e5' : (/New /).test(value) ? '#DEEBF7' : (/Newer /).test(value) ? '#C6DBEF' : ''"
                output$studies <- renderDT({
                    render_datatable(
                        studies_static,
                        "New Studies",
                        c(4),
                        integer(0)) %>%
                    formatStyle(
                        columns = names(studies_static),
                        fontSize = '12px'
                    ) %>%
                    formatStyle("Similarity",
                                backgroundColor = JS(js))
                })
                output$studies_input_help_text <- renderUI({
                    helpText("Note that one can only replace studies on which no coders have been assigned and for which a newer version has been identified")
                })
                }
            })
            
            # When user edits studies table
            observeEvent(input$studies_cell_edit, {
                
                # Extract the edit information
                info <- input$studies_cell_edit
                r <- info$row
                c <- info$col + 1
                v <- info$value
                
                # Modify studies() according to edits
                tmp <- studies()
                tmp[r, c] <- v
                studies(tmp)
                versioning_complete(FALSE)
            })
    
            # Buttons enabling
            observeEvent({
              list(
                studies(),
                new_studies_n(),
                input$username,
                lapply(1:new_studies_n(), function(i) input[[paste0("PDF_", i)]]$datapath),
                versioning_complete()
              )
            }, {
                
                # If all pre-versioning conditions are met
                if (new_studies_n() > 0 &&
                    (sum((studies() %>% select(-`Similarity`))=="") == 0) &&
                    input$username != "" &
                    !versioning_complete()) {
                    
                    # Ensure all pdfs uploaded
                    num_paths <- 0
                    for (i in 1:new_studies_n()) {
                        path <- input[[paste0("PDF_",i)]]$datapath
                        if (length(path)==1) {
                            num_paths = num_paths + 1
                        }
                    }
                    if(num_paths == new_studies_n()) {
                        output$start_version_button = render_button("start_version",
                                                                    "Version",
                                                                    "enable")
                        output$apply_action_button = render_button("apply_action",
                                            "Apply Actions",
                                            "disable")
                        return()
                    }
                }
                # Keep "Version" button disabled if pre-versioning conditions unmet
                output$start_version_button = render_button("start_version",
                                                                    "Version",
                                                                    "disable")
            })

            # Start versioning, update "similarity column
            # Substitute fileinputs column with NULL and lock it
            # Re-render datatable by replaceData()
            observeEvent(input$start_version, {
                
                version_results(list())
                
                source("version.R", local=T)
                
                withProgress(message = 'Comparison Progress', value = 0, {
                
                    versioning_complete(FALSE)
                    
                    incProgress(0, detail = "Among New Papers")
                    
                    tmp <- studies()
                    tmp$`Similarity` = ""
                    
                    # Rename uploaded PDF using task name and version among new papers
                    if (new_studies_n() > 1) {
                        tmp <- studies()
                        task_names = c()
                        texts = c()
                        tmp$`Similarity` = ""
                        for (i in 1:new_studies_n()) {
                            task_name = paste(tmp$`Last Name`[i], tmp$`Year`[i], collapse = " ")
                            file.copy(input[[paste0("PDF_",i)]]$datapath, paste0(task_name, ".pdf"), overwrite = T)
                            # add text to vector
                            text <- paste(pdftools::pdf_text(paste0(task_name, ".pdf")), collapse='')
                            text <- iconv(text, from = "latin1", to = "UTF-8", sub = "byte")
                            text <- gsub("\n", "", text)
                            token_list <- tokenize_ngrams(text, n = 3)
                            hash_list <- strsplit(toString(hash_string(unlist(token_list))), ", ")
                            texts = c(texts, hash_list)
                            task_names = c(task_names, task_name)
                        }
                        # compare similarity against added new studies
                        for (i in 1:new_studies_n()) {
                            for (j in 1:new_studies_n()) {
                                if (j != i) {
                                    jac_sim <- jaccard_similarity(texts[j][[1]], texts[i][[1]])
                                    print(jac_sim)
                                    if (jac_sim > 0.025 | (abs(jac_sim - 0.025) <= 0.01)) {
                                        result = case_when(
                                            tmp$`Year`[i] > tmp$`Year`[j] ~ "Newer",
                                            tmp$`Year`[i] <= tmp$`Year`[j] ~ "Old"
                                        )
                                        tmp$`Similarity`[i] = paste("Among new studies: ", result, " version of ", task_names[j])
                                        tmp$Action[i] =  as.character(selectInput(session$ns(paste0("action_", i)), "" , c("Discard", "New Entry"), width="100%"))
                                    }
                                }
                            }
                        }
                    }
                    studies(tmp)
                    incProgress(1/(new_studies_n()+1))
                    
                    # Version new papers against existing papers
                    tmp <- studies()
                    for (i in 1:new_studies_n()) {
                        task_name = paste(tmp$`Last Name`[i], tmp$`Year`[i], collapse = " ")
                        if (new_studies_n() == 1) {
                            file.copy(input[[paste0("PDF_",i)]]$datapath, paste0(task_name, ".pdf"), overwrite = T)
                        }
                        title = tmp$`Title`[i]
                        incProgress(0, detail = task_name)
                        
                        if (nrow(main_react()) == 0) {
                          tmp$PDF[i] <- paste0("File: ", input[[paste0("PDF_",i)]]$name)
                          tmp$`Similarity`[i] <- paste(tmp$`Similarity`[i], " No database to compare to — treated as new entry")
                          tmp$Action[i] <- as.character(selectInput(session$ns(paste0("action_", i)), "", c("New Entry"), width = "100%"))
                          text <- paste(pdftools::pdf_text(paste0(task_name, ".pdf")), collapse = '')
                          text <- iconv(text, from = "latin1", to = "UTF-8", sub = "byte")
                          text <- gsub("\n", "", text)
                          
                          generated_id <- paste0(
                            toupper(substr(tmp$`Last Name`[i], 1, 3)),  
                            tmp$`Year`[i],                           
                            "_",
                            as.integer(runif(1, 1000, 9999))         
                          )
                          
                          version_results(base::append(
                            version_results(),
                            list(c("New", generated_id, paste0("PDF_", i), text, ""))
                          ))
                          
                          incProgress(1/(new_studies_n()+1), detail = task_name)
                          next
                        }
                        
                        res = c()
                        tryCatch(
                            expr = {
                                res = version(main_react(),
                                              task_name,
                                              title,
                                              ngram = 3,
                                              threshold = 0.025,
                                              margin = 0.01,
                                              override = '0',
                                              dev_papers_dir = "Study Assignment/dev_papers")
                            },
                            error = function(e) {
                                msg = sprintf("Error: versioning unsuccessful for %s.
                                         Please ensure the uploaded PDFs contain
                                         readable text.", task_name)
                                cat(yellow(msg))
                                showNotification(msg, type = "error")
                            })
                        tmp$PDF[i] = paste0("File: ", input[[paste0("PDF_",i)]]$name)
                        tmp$`Similarity`[i] = paste(tmp$`Similarity`[i], " Among database: ", res[1], res[5], collapse = " ")
                        # Only replace newer but unassigned studies
                        if (res[1] == "Newer" &&
                            is.na(main_react()$coders[which(main_react()$studyID == res[2])])) { 
                            tmp$Action[i] =  as.character(selectInput(session$ns(paste0("action_", i)), "" , c("Discard", "New Entry", "Replace"), width="100%"))
                        }
                        else {
                            tmp$Action[i] =  as.character(selectInput(session$ns(paste0("action_", i)), "" , c("Discard", "New Entry"), width="100%"))
                        }
                        version_results(base::append(version_results(), list(res)))
                        incProgress(1/(new_studies_n()+1), detail = task_name)
                    }
                    studies(tmp)
                    replaceData(
                        dt_proxy <- dataTableProxy("studies", session = session),
                        studies(),
                        resetPaging = FALSE, rownames = FALSE
                    )
                    versioning_complete(TRUE)
                    
                    
                    if (!versioning_complete()) {
                        return()
                    }
                    
                    # enable "Apply Actions" button if versioning is completed
                    output$apply_action_button <- render_button("apply_action",
                                                                "Apply Actions",
                                                                "enable")
                    
                })
            })
            
            # Update linked database
            observeEvent(input$apply_action, {
                
                # Identify studies to be uploaded
                add = c()
                for (i in 1:new_studies_n()) {
                    if (input[[paste0("action_", i)]] != "Discard") {
                        add = c(add, i)
                    }
                }
                # All discard
                if (length(add) == 0) {
                    return()
                }
                
                # Upload pdf, then paper_assign one by one
                withProgress(message = 'Upload Progress', value = 0, {
                    
                    for (STUDY in add) {
                        
                        incProgress(0, detail = paste0(studies()$`Last Name`[STUDY]," ", studies()$`Year`[STUDY]))
                        
                        #PDF to drive
                        attempt = 0
                        uploaded = F
                        while (attempt <= 3 & uploaded == F) {
                            attempt = attempt + 1
                            tryCatch(
                            expr = {
                            withTimeout({
                                        drive_upload(paste0(studies()$`Last Name`[STUDY]," ", studies()$`Year`[STUDY], ".pdf"),
                                                     dev_papers_path,
                                                     name=paste0(version_results()[[STUDY]][3], ".pdf"),
                                                     overwrite=TRUE)
                                        },
                                        timeout=60,
                                        onTimeout="error")
                            uploaded = T
                            })
                        }
                     
                        # paper_assign
                        row <- data.frame(
                            studyID = "",
                            assignment_status = "to be screened",
                            `supervisors` = "",
                            `coders` = "",
                            Intervention = "",
                            `intervention_notes` = "",
                            title = studies()$`Title`[STUDY],
                            link = drive_link(drive_find(paste0(version_results()[[STUDY]][3], ".pdf"), n_max=1)),
                            linked = "",
                            `screened_by` = "",
                            sc_RCT = "",
                            sc_ITT = "",
                            `sc_doctype` = "",
                            `sc_doctype_notes` = "",
                            `retracted` = "",
                            `date_added` = format(Sys.time()),
                            `date_screened` = "",
                            `date_first_assigned` = "",
                            `date_last_updated` = format(Sys.time()),
                            `last_updated_by` = input$username,
                            task_name = paste0(studies()$`Last Name`[STUDY]," ", studies()$`Year`[STUDY]),
                            text = version_results()[[STUDY]][4],
                            `pdf_id` = version_results()[[STUDY]][3]
                        )
                        
                        
                        # New Entry (studyID is version results sensitive)
                        if (input[[paste0("action_", STUDY)]] == "New Entry") {
                            if (version_results()[[STUDY]][1] == "New") {
                                row$studyID[1] = version_results()[[STUDY]][2]
                            }
                            else {
                                # identified as duplicated or newer but want to add as new, generate new studyID
                                row$studyID[1] = paste0(toupper(substr(studies()$`Last Name`[STUDY], 1, 3)),
                                                        studies()$`Year`[STUDY],
                                                        "_",
                                                        as.integer(runif(1, 1000, 9999)))
                            }
                          
                          
                          ss_id <- drive_get(paper_assign_filename)$id
                          assignment_data <- read_sheet(ss = ss_id, sheet = "assignment")
                          
                           if (nrow(assignment_data) == 0) {

                               row$text <- ifelse(nchar(row$text) > 50000, substr(row$text, 1, 50000), row$text)
                          
                               sheet_write(
                               data = row,
                                 ss = ss_id,
                               sheet = "assignment"
                           )
                           } else {

                            sheet_append(
                              ss = ss_id,
                              data = row,
                              sheet = "assignment")
                         }
                        }
                        
                        # Replace existing entry
                        else if (input[[paste0("action_", STUDY)]] == "Replace") {
                            
                            row$studyID[1] = version_results()[[STUDY]][2]
                            
                            # Find row number in database
                            idx = which(main_react()$studyID == version_results()[[STUDY]][2])
                            range_write(ss = drive_get(paper_assign_filename)$id,
                                        row,
                                        sheet = "assignment",
                                        range = sprintf("A%s", (idx+1)),
                                        col_names = F
                            )
                        }
                    
                        incProgress(1/length(add))
                    }
                    msg = "Reading new data \n"
                    showNotification(msg, type = "default")
                    cat(green(msg))
                    
                    # Load in new online sheets and reset UI elements
                    main_react(as.data.frame(read_sheet(drive_get(paper_assign_filename)$id, sheet = "assignment")))
                    version_results(list())
                    versioning_complete(FALSE)
                    new_studies_n(0)
                    studies(data.frame(`Last Name` = character(0),
                                       `Year` = character(0),
                                       `Title` = character(0),
                                       `PDF` = character(0),
                                       `Similarity` = character(0),
                                       `Action` = character(0),
                                       check.names = FALSE))
                    output$studies <- NULL
                    output$studies_input_help_text <- NULL
                    reset_sidebar()
                    msg = "New data loaded \n"
                    showNotification(msg, type = "message")
                    cat(green(msg))
                    return()
                })
            })
        })
        }


# (5) Defines User Interface and Server of the "Screening Papers" module =======

    ## UI Inputs (UI elements that need to be prepared by the server)
    ## > username_UI :: UI rendered after reading 'staff' sheet in database
    ## > review :: button to review changes made to papers
    ## > sync :: button to sync changes made to papers with database
    ## > papers :: table of all papers in database
    ## > changed_obs :: table of papers with edits made
    
    ## UI Outputs (Values produced by user inputs through the UI)
    ## > username :: full name of user
    ## > papers_cell_edit :: edits made by user
    ## > review_changes :: user input on 'Review' button
    ## > sync :: user input on "Sync" button
    
    ## Reactive Values
    ## > main_reactive() :: df :: 'assignment' sheet in database
    ## > table_display_reactive() :: df :: table of papers displayed in app using labels
    ## > main_edit_reactive() :: df :: table of papers displayed in apps but actual values instead of labels
    ## > idx() :: vector :: vector of indices of papers for which edits were made
    ## > staff() :: df :: 'staff' sheet in database
    
    
    ## (5A) Defining User Interface ============================================
    
    ui_screening_papers <- function(id) {
        ns_screening <- NS(id)
        fluidPage(
            sidebarLayout(
                sidebarPanel(
                    uiOutput(ns_screening("username_UI")),
                    uiOutput(ns_screening("review")),
                    helpText("Review changes before pushing to linked database"),
                    uiOutput(ns_screening("sync")),
                    helpText("Push changes to linked database"),
                    hr(),
                    h5(strong("Input Codes")),
                    helpText("Translations of numeric input codes"),
                    
            # Define other screening criteria here. Change the column names in paper_assign_template and in section 4a/b to match new screening criteria
                    
                    card(renderTable(data.frame("Intervention" = c("Intervention 1", "Intervention 2", "Intervention 3", "Intervention 4", "Intervention 5", "Stall", "Exclude"),
                                            "Code" = c("1", "2", "3", "4", "5", "-88", "-77"),
                                            check.names=FALSE))),
                    card(renderTable(data.frame("Document" = c("Publication", "Report", "Working Paper", "Stall"),
                                            "Code" = c("1", "2", "3", "-88"),
                                            check.names=FALSE))),
                    card(renderTable(data.frame("RCT/ITT" = c("No", "Yes"),
                                            "Code" = c("0", "1"),
                                            check.names=FALSE)))
                    
                ),
                mainPanel(
                    DTOutput(ns_screening("papers"),
                             height = "500", width = "100%"),
                    helpText("To input multiple intervention types, use a semicolon separated list to indicate
                             multiple intervention types (e.g. 4;1;2)"),
                    DTOutput(ns_screening("changed_obs"),
                             height = "250", width = "100%")
                )
            )
        )
    }
    
    
    ## (5B) Defining Server Logic ==============================================

    server_screening_papers <- function(id, shared_data, tab) {
        moduleServer(id, function(input, output, session) {
            
                        
            # Helper function to render enabled or disabled buttons
            render_button <- function(button_name,
                                      label,
                                      status){
                if (status == "enable") {
                    return(renderUI({actionButton(session$ns(button_name), 
                                                  label,
                                                  style = "width: 100%; font-size: 12px; color: white; background-color: #4e2a85;")}))
                }
                else if (status == "disable") {
                    return(renderUI({disabled(actionButton(session$ns(button_name), 
                                                           label,
                                                           style = "width: 100%; font-size: 12px;"))}))
                }
            }

            # Helper function to render Datatable
            render_datatable <- function(df,
                                         colnames,
                                         title,
                                         editable_cols,
                                         hide_cols,
                                         search) {
                if (length(editable_cols)==0) {
                    return(datatable(
                        df, 
                        colnames=colnames,
                        escape = FALSE, 
                        selection = 'none', 
                        fillContainer = TRUE,
                        rownames = FALSE,
                        caption = title,
                        options = list(dom = case_when(search ~ 'ft', !search ~ 't'), 
                                       paging = FALSE,
                                       autowidth = FALSE, 
                                       ordering = FALSE,
                                       columnDefs = list(list(targets = "_all", className = "dt-center"),
                                                         list(targets = hide_cols, visible = FALSE)),
                                       initComplete = JS("function() {
                                                         $(this).addClass('compact');
                                                         $(this.api().table().header()).css({'background-color': '#4e2a85', 'color': 'white'});
                                                         $(this.api().table().node()).css({'border-collapse': 'collapse'});
                                                         $(this.api().table().body()).find('td').css({'border': '1px solid #ddd'});}"
                                                        ),
                                       preDrawCallback = DT::JS("function(settings) {
                                                                 Shiny.unbindAll(this.api().table().node());
                                                                 }"),
                                       drawCallback = DT::JS("function(settings) {
                                                              Shiny.initializeInputs(this.api().table().node());
                                                              Shiny.bindAll(this.api().table().node());
                                                          }")
                                       )))
                }
                else {
                    
                    return(datatable(
                        df, 
                        colnames=colnames,
                        escape = FALSE, 
                        selection = 'none', 
                        fillContainer = TRUE,
                        rownames = FALSE,
                        caption = title,
                        editable = list(
                                target = 'cell', 
                                disable = list(columns = editable_cols)
                                ),
                        options = list(dom = case_when(search ~ 'ft', !search ~ 't'), 
                                       paging = FALSE,
                                       ordering = FALSE,
                                       autowidth = FALSE, 
                                       columnDefs = list(list(targets = "_all", className = "dt-center"),
                                                         list(targets = hide_cols, visible = FALSE)),
                                      initComplete = JS("function() {
                                                         $(this).addClass('compact');
                                                         $(this.api().table().header()).css({'background-color': '#4e2a85', 'color': 'white'});
                                                         $(this.api().table().node()).css({'border-collapse': 'collapse'});
                                                         $(this.api().table().body()).find('td').css({'border': '1px solid #ddd'});}"
                                                        ),
                                       preDrawCallback = DT::JS("function(settings) {
                                                                 Shiny.unbindAll(this.api().table().node());
                                                                 }"),
                                       drawCallback = DT::JS("function(settings) {
                                                              Shiny.initializeInputs(this.api().table().node());
                                                              Shiny.bindAll(this.api().table().node());
                                                          }")
                        )))}
            }
            
            
            ### Create reactive variables
            main_reactive <- reactiveVal(isolate(shared_data$main_online))
            table_display_reactive <- reactiveVal(isolate(shared_data$main_online))
            main_edit_reactive <- reactiveVal(isolate(shared_data$main_online))
            idx <- reactiveVal(c())
            
            staff = reactiveVal(isolate(shared_data$staff_online))
            subsup <- as.vector(na.omit(staff()[staff()$position == "Sub-Supervisor", ]$name))
            sup <- as.vector(na.omit(staff()[staff()$position == "Supervisor", ]$name))
            
            
            ### Render initial UI elements
            output$username_UI = renderUI({selectInput(inputId = session$ns("username"),
                                                       label = "Please input your name",
                                                       choices = c("", sup, subsup),
                                                       selected = "",
                                                       multiple = FALSE,
                                                       selectize = TRUE,
                                                       width = NULL,
                                                       size = NULL)})
            
            output$review <- render_button("review_changes",
                                               "Review Changes",
                                               "disable")
            output$sync <- render_button("sync",
                                         "Sync",
                                         "disable")
            
            ### Render the initial DataTable of all papers
            ### Choose and label visible columns 
            visible_columns <- c("studyID", 
                                 "assignment_status", 
                                 "Intervention",
                                 "intervention_notes",
                                 "linked",
                                 "sc_RCT",
                                 "sc_ITT",
                                 "sc_doctype",
                                 "sc_doctype_notes")
            column_names <- c("StudyID",
                              "Status",
                              "Intervention",
                              "Reason",
                              "Linked",
                              "RCT", 
                              "ITT",
                              "Document",
                              "Reason")
            
            ### use static "assignment" so it doesn't keep refreshing every time
            ### the displayed table is edited, add formating styles
            output$papers <- renderDT({
                render_datatable(main_reactive()[ ,visible_columns],
                                 column_names,
                                 "DEVIDENCE Papers",
                                 c(0, 1),
                                 integer(0),
                                 search=TRUE) %>%
                    formatStyle("assignment_status",
                            backgroundColor=c('#D4D4D4')) %>%
                    formatStyle("studyID",
                            backgroundColor=c('#D4D4D4')) %>%
                    formatStyle(
                        columns = c("linked", "intervention_notes", "sc_doctype_notes"),  
                        width = '90px') %>%
                    formatStyle(
                        columns = visible_columns,
                        fontSize = '11px',
                        border = '1px solid #ddd')
            })
            
    
            ### Every time user makes an edit on table
            observeEvent(input$papers_cell_edit, {
                
                ### Obtain proxy used to reference Datatable "papers"
                dt_proxy <- dataTableProxy("papers", session = session)
                
                ### Extract the edit information
                info <- input$papers_cell_edit
                
                r <- info$row
                c <- info$col
                v <- info$value
                
                ### Get the column name and corresponding row index
                ### Adjust column index (+1 because of 0-indexing in DT edit event)
                col_name <- visible_columns[c+1] 
                tmp_studyID <- table_display_reactive()$studyID[r]
                row_num <- which(table_display_reactive()$studyID == tmp_studyID)
                
                ### Validate input (variable by variable) and switch value with labels
                ### table_display() saves the labels to display to the user
                ### while main_edit() saves the values
                invalid_input = T
                tryCatch(expr = {
                    
                    stopifnot(input$username!="")
                    
                    save_as = ""
                    display_as = ""
                    
                    if (col_name == "linked") {
                        save_as <- v
                        display_as <- v
                    }
                    ### Intervention Type: accept values from 1-6 
                    ### and comma separated list of values 1-6
                    else if (col_name == "Intervention") {
                        
                        ### Trim leading and ending whitespaces
                        v = str_trim(v)
                        
                        ### If single value or valid stall/exclude codes
                        if (nchar(v)==1 | (nchar(v) == 3 & !grepl(";",v))) {
                            
                            ### Ensure input is number
                            stopifnot(!is.na(as.integer(v)))
                            
                            ### Ensure input is within range
                            stopifnot((as.integer(v) >= 1 & as.integer(v) <= 6)
                                      | (as.integer(v) == -77 | as.integer(v) == -88))
                            save_as <- v
                            
                            if(v == "1") {
                                display_as <- "Intervention 1"
                            } else if (v=="2") {
                                display_as <- "Intervention 2"
                            } else if (v=="3") {
                                display_as <- "Intervention 3"
                            } else if (v=="4") {
                                display_as <- "Intervention 4"
                            } else if (v == "5") {
                                display_as <- "Intervention 5"
                            } else if (v=="-88") {
                                display_as <- "stall"
                            } else if (v=="-77") {
                                display_as <- "exclude"
                            }
                            
                        } 
                        ### If multiple values
                        else if (nchar(v)>1) {
                            
                            v = strsplit(v, ";")[[1]]
                            for (num in v) {
                                tmp = str_trim(num)
                                
                                ### Ensure input is number
                                stopifnot(!is.na(as.integer(tmp)))
                                
                                ### Ensure input is within range
                                stopifnot(as.integer(tmp) >= 1 & 
                                              as.integer(tmp) <= 6)
                                save_as <- paste0(save_as, ";", tmp)
                                
                                if(tmp == "1") {
                                    display_as <- paste0(display_as, ";", "Intervention 1")
                                } else if (tmp=="2") {
                                    display_as <- paste0(display_as,";", "Intervention 2")
                                } else if (tmp=="3") {
                                    display_as <- paste0(display_as,";", "Intervention 3")
                                } else if (tmp=="4") {
                                    display_as <- paste0(display_as,";", "Intervention 4")
                                } else if (tmp=="5") {
                                    display_as <- paste0(display_as,";", "Intervention 5")
                                } 
                                else if (tmp=="-77" | tmp=="-88") {
                                    ### Should not have exclude/stall in a list
                                    stopifnot(F)
                                }
                            }
                            display_as = substr(display_as, 2, nchar(display_as))
                            save_as = substr(save_as, 2, nchar(save_as))
                        }
                        else if (nchar(v)==0) {
                            display_as = ""
                            save_as = ""
                        }
                        invalid_input = F
                    } 
                    ### RCT and ITT: accept values 0 or 1
                    else if (col_name == "sc_RCT" | col_name == "sc_ITT") {
                        
                        ### Trim leading and ending whitespaces
                        v = str_trim(v)
                        
                        if (v=="") {
                            display_as <- as.double("")
                            save_as <- as.double("")
                        }
                        else {
                            ### Ensure input is only one number
                            stopifnot(nchar(v)==1)
                            
                            ### Ensure input is number
                            stopifnot(!is.na(as.integer(v)))
                            
                            ### Ensure input is binary 
                            stopifnot(as.integer(v) == 0 |
                                          as.integer(v) == 1)
                            
                            ### RCT variable is saved as double in assignment
                            display_as <- as.double(v)
                            save_as <- as.double(v)
                        }
                        
                        invalid_input = F
                    }
                
                    ### document: accept values from 1-3 and -77
                    ### 'publication', 'report', 'working paper', 'other'
                    else if (col_name == "sc_doctype") {
                        
                        ### Trim leading and ending whitespaces
                        v = str_trim(v)
                        
                        ### If single value or valid stall/exclude codes
                        if (nchar(v)==1 | nchar(v) == 3) {
                            
                            ### Ensure input is number
                            stopifnot(!is.na(as.integer(v)))
                            
                            ### Ensure input is either within the range of
                            ### valid intervention codes or stall/exclude codes
                            stopifnot((as.integer(v) >= 1 & as.integer(v) <= 3) 
                                      | (as.integer(v) == -88))
                            save_as <- v
                            
                            if(v == "1") {
                                display_as <- "publication"
                            } else if (v=="2") {
                                display_as <- "report"
                            } else if (v=="3") {
                                display_as <- "working paper"
                            } else if (v=="-88") {
                                display_as <- "stall"
                            }
                        } 
                        else if (nchar(v)==0) {
                            display_as = ""
                            save_as = ""
                        }
                    }
                    else if (col_name == "intervention_notes" | 
                             col_name == "sc_doctype_notes") {
                        display_as = v
                        save_as = v
                    }
                    invalid_input = F
                }, 
                error = function(e) {
                    
                    if (input$username=="") {
                        msg = "Error: username is missing. Changes not saved. Please try again"
                        cat(yellow(msg))
                        showNotification(msg, 
                                         type = "error")
                    } else {
                        msg = sprintf("Error: invalid input into %s", col_name)
                        cat(yellow(msg))
                        showNotification(msg, 
                                         type = "error")
                    }
                    
                    return()
                }
                )
                if (invalid_input) {
                    replaceData(
                        dt_proxy,
                        table_display_reactive()[ , visible_columns],
                        resetPaging = FALSE, rownames = FALSE
                    )
                    return()
                }
                
                ### Save user edits in respective reactive dataframes
                tmp <- main_edit_reactive()
                tmp[row_num, col_name] <- save_as
                main_edit_reactive(tmp)
                
                tmp <- table_display_reactive()
                tmp[row_num, col_name] <- display_as
                table_display_reactive(tmp)
                
                ### Update table of papers that is displayed with labels
                replaceData(
                    dt_proxy,
                    table_display_reactive()[ , visible_columns],
                    resetPaging = FALSE, rownames = FALSE
                )
                
                
                ### Enable review button upon changes being made
                output$review <- render_button("review_changes",
                                               "Review Changes",
                                               "enable")
                
                })
            
            ### User clicks on review changes
            observeEvent(input$review_changes, {
                
                ### Flag made changes using idx() value defined at beginning
                
                ### Loop over rows and editable columns to identify changes 
                for (i in 1:nrow(main_reactive())) {
                    
                    for (c in visible_columns) {
                        
                        ### NA -> NA = no change
                        if (is.na(main_reactive()[i, c]) & is.na(main_edit_reactive()[i, c])) {
                            next
                        } 
                        ### NA -> "" = change
                        else if (is.na(main_reactive()[i, c]) & !is.na(main_edit_reactive()[i, c])) {
                            idx(c(idx(), i))
                        } 
                        ### x -> y = change
                        else if (main_reactive()[i, c] != main_edit_reactive()[i, c]) {
                            idx(c(idx(), i))
                        }
                    }
                }

                idx(unique(idx()))
                
                ### Display changed rows
                output$changed_obs <- renderDT({
                    render_datatable(
                        table_display_reactive()[idx(), visible_columns],
                        column_names,
                        "Saved Changes",
                        c(),
                        integer(0),
                        search=F) %>%
                    formatStyle(
                            columns = visible_columns,
                            fontSize = '11px')
                })
                
                ### Shows a submit button
                output$sync <- render_button("sync",
                                             "Sync",
                                             "enable")
                
            })
            
            ### User clicks on submit saved changes
            observeEvent(input$sync, {
                
                output$sync <- render_button("sync",
                                             "Sync",
                                             "disable")
                
                output$review <- render_button("review_changes",
                                               "Review Changes",
                                               "disable")
                
                ### Check if user failed screening but did not specify why
                missing_inputs = T
                tryCatch(
                    expr = {
                        for (row_num in idx()) {
                            
                            tmp = main_edit_reactive()[row_num, ]
                                
                            if (!is.na(tmp$Intervention[1])) {
                                if(tmp$Intervention[1] == "-77" |
                                   tmp$Intervention[1] == "-88") {
                                    fail = sprintf("Please specify the intervention 
                                        type of %s in the int_notes column",
                                        tmp$studyID)
                                    stopifnot(!is.na(tmp$`intervention_notes`[1]))
                                    stopifnot(tmp$`intervention_notes`[1] != "")
                                }
                            }
                            
                            if (!is.na(tmp$`sc_doctype`[1])) {
                                if(tmp$`sc_doctype`[1] == "-88") {
                                    fail = sprintf("Please specify the document 
                                            type of %s in the doc_notes column",
                                            tmp$studyID)
                                    stopifnot(!is.na(tmp$`sc_doctype_notes`[1]))
                                    stopifnot(tmp$`sc_doctype_notes`[1] != "")
                                }
                            }
                        }
                        missing_inputs = F
                    },
                    error = function(e) {
                        msg = fail
                        cat(yellow(msg))
                        showNotification(msg, type = "warning")
                        return()
                    }
                )
                if (missing_inputs == T) {
                    return()
                }
                withProgress(message = "Upload Progress: paper_assign", value = 0, {
                for (row_num in idx()) {
                    
                    tmp = main_edit_reactive()[row_num, ]
                    msg = paste0("Upload Progress: ", tmp$studyID[1])
                    incProgress(0/length(idx()), message = msg)
                    
                    # Check if pass all screening criteria
                    # Make sure relevant fields are not NA, or else
                    # errors will be thrown in the nested if() statement 
                    if (!is.na(tmp$Intervention[1]) &
                        !is.na(tmp$sc_RCT[1]) &
                        !is.na(tmp$sc_ITT[1]) &
                        !is.na(tmp$`sc_doctype`[1])) {
                        
                        if((tmp$Intervention[1]!="-77"  &
                            tmp$Intervention[1]!="-88" &
                           tmp$sc_RCT[1] == "1" &
                           tmp$sc_ITT[1] == "1" &
                           tmp$`sc_doctype`[1]!="3" &
                           tmp$`sc_doctype`[1]!="-88")| # vanilla published paper or report
                           (tmp$Intervention[1]!="-77"  &
                            tmp$Intervention[1]!="-88" &
                            tmp$sc_RCT[1] == "1" &
                            tmp$sc_ITT[1] == "1" &
                            tmp$`sc_doctype`[1]=="3" &
                            grepl("4", tmp$Intervention[1]))) { # working graduation paper
                            
                            # keep status unless status is to be screened or stall
                            if (tmp$assignment_status %in% c("to be screened", "stall")){
                                tmp$assignment_status <- "to be assigned"
                                tmp$screened_by = input$username
                                tmp$date_screened = format(Sys.time())
                            }
                        }
                    }
                    retract = F
                    # intervention type is out of range
                    if (!is.na(tmp$Intervention[1]) & 
                        (tmp$Intervention[1] == "-77" |
                         tmp$Intervention[1] == "-88")) {
                        tmp$assignment_status <- "stall"
                        retract = T
                    }
                    # not RCT
                    if (!is.na(tmp$sc_RCT[1]) & 
                        (tmp$sc_RCT[1] == "0")) {
                        tmp$assignment_status <- "stall"
                        retract = T
                    }
                    # not ITT
                    if (!is.na(tmp$sc_ITT[1]) & 
                        (tmp$sc_ITT[1] == "0")) {
                        tmp$assignment_status <- "stall"
                        retract = T
                    }
                    # working paper (and not graduation) or stall document
                    if (!is.na(tmp$`sc_doctype`[1]) &
                       (tmp$`sc_doctype`[1] == "3" |
                        tmp$`sc_doctype`[1] == "-88")) {
                           if (tmp$`sc_doctype`[1] == "-88") {
                               tmp$assignment_status <- "stall"
                               retract = T
                           }
                           if (tmp$`sc_doctype`[1] == "3" &
                               (is.na(tmp$Intervention[1]) |
                               !grepl("4", tmp$Intervention[1]))) {
                               tmp$assignment_status <- "stall"
                               retract = T
                           }
                       }
                    if (retract == T) {
                        tmp$retracted = tmp$retracted + 1
                    }
                
                tmp$date_last_updated = format(Sys.time())
                tmp$last_updated_by = input$username
                
                ### Upload changes to 'assignment' sheet in database
                ### Use range_write instead of sheet_write for speed
                range_write(ss = drive_get(paper_assign_filename)$id,
                            data = tmp[1, ],
                            sheet = "assignment",
                            range = sprintf("A%s", (row_num+1)),
                            col_names = F
                )
                msg = paste0("Upload Progress: ", tmp$studyID[1])
                cat(green(msg))
                incProgress(1/length(idx()), message = msg)
                }
                })

                ### Reset module to read in new online data by resetting UI inputs
                output$changed_obs <- NULL
                output$review <- render_button("review_changes",
                                               "Review Changes",
                                               "disable")
                output$sync <- render_button("sync",
                                             "Sync",
                                             "disable")
                
                msg = "Reading new data \n"
                showNotification(msg, type = "default")
                cat(green(msg))
                
                ### Load in new online sheets
                main_reactive(as.data.frame(read_sheet(drive_get(paper_assign_filename)$id, sheet = "assignment")))
                table_display_reactive(as.data.frame(read_sheet(drive_get(paper_assign_filename)$id, sheet = "assignment")))
                main_edit_reactive(as.data.frame(read_sheet(drive_get(paper_assign_filename)$id, sheet = "assignment")))
                idx(c())
                
                msg = "New data loaded \n"
                showNotification(msg, type = "message")
                cat(green(msg))
                return()
            })
        })
    }

    
    
# (6) Defines User Interface and Server of the "Assigning Papers" module =======

    ## UI Inputs (UI elements that need to be prepared by the server)
    ## > username_UI :: select input for username
    ## > choose_study_UI :: select input for studies to randomize
    ## > people :: 
    ## > randomize_button :: button to randomize tasks
    ## > sync_with_paper_assign_button :: button to sync assignments with database
    ## > reassignment_details :: table for user to choose coders to replace in event of re-assignments
    ## > task_load :: table for user to specify number of tasks for individual coders
    ## > assignment_conditions_helptext :: help text outlining assignment conditions
    ## > tasks :: random assignment results
    
    ## UI Outputs (Values produced by user inputs through the UI)
    ## > username :: self-explanatory, used as value for screened_by and updated_by
    ## > roles :: components user wants to randomize
    ## > assignment_type :: single/double assignment type chosen by user
    ## > studies_filter:: conditions by which user wants to filter studies to choose from
    ## > chosen_studies :: studies user chooses to randomize
    ## > chosen_s1 :: s1 coders user wants to include in randomization pool
    ## > chosen_s1c :: s1c coders user wants to include in randomization pool
    ## > chosen_s2 :: s2 coders user wants to include in randomization pool
    ## > task_load_cell_edit :: task loads specified by user in a table
    ## > assign :: "Assign" button
    ## > download_tasks_button :: "Download Tasks" button
    ## > sync :: "Sync" button
    ## > studyid :: coder to be kept in case of reassigning a study, named using its studyID
    
    ## Reactive Values
    ## > main_reactive() :: df :: 'assignment' sheet in database
    ## > seed_tracker_reactive() :: df :: 'seed_tracker' sheet in database
    ## > staff() :: df :: 'staff' sheet in database
    ## > task_load_data() :: df :: dataframe of number of tasks to be assigned to each coder (0 means no preference)
    ## > valid_loads() :: bool :: indicator if number of tasks specified is valid for randomization
    ## > chosen_s1 :: vector :: s1 coders chosen by user to be in randomization pool
    ## > chosen_s1c :: vector :: s1c coders chosen by user to be in randomization pool
    ## > chosen_s2 :: vector :: s2 coders chosen by user to be in randomization pool
    ## > studies_universe :: vector :: studyIDs of all studies that can be assigned/re-assigned
    ## > randomized_results :: df :: dataframe storing randomized assignments
    
    
    ## (6A) Defining User Interface ============================================
    
    ui_assign <- function(id) {
        ns_a <- NS(id)
        fluidPage(
            useShinyjs(),
            sidebarLayout(
                sidebarPanel(
                    uiOutput(ns_a("username_UI")),
                    selectizeInput(inputId = ns_a("studies_filter"),
                                   label = "Study Selection",
                                   choices = c("All", "To be assigned", "Assigned", "Re-assigned", "Cash Transfers", "Business Training", "Microcredit", "Savings", "Graduation"),
                                   selected = c("All"),
                                   multiple = TRUE),
                    uiOutput(ns_a("choose_study_UI")),
                    selectizeInput(inputId = ns_a("assignment_type"),
                                   label = "Coding",
                                   choices = c("Single (replace)", "Single (new)", "Double"),
                                   selected = "Double",
                                   multiple = FALSE),
                    helpText(tags$em("Single (replace)"), ": replace one Stage 2 coder per study", tags$br(),
                             tags$em("Single (new)"), ": add one Stage 2 coder per study", tags$br(),
                             tags$em("Double"), ": assign two Stage 2 coders per study"),
                    selectizeInput(inputId = ns_a("roles"),
                                   label = "Randomize",
                                   choices = c("S1", "S1 Check", "S2"),
                                   multiple = TRUE),
                    uiOutput(ns_a("people")),
                    uiOutput(ns_a("randomize_button")), 
                    br(),
                    uiOutput(ns_a("download_tasks_button")),
                    helpText("Download assignments as csv"),
                    uiOutput(ns_a("sync_with_paper_assign_button")),
                    helpText("Update linked database with assignments")
                ),
                mainPanel(
                    DTOutput(ns_a("reassignment_details")),
                    br(),
                    DTOutput(ns_a("task_load"),
                             width = "100%"),
                    uiOutput(ns_a("assignment_conditions_helptext")),
                    br(),
                    DTOutput(ns_a("tasks"),
                             width = "100%"),
                    br()
                )
            )
        )
    }
    
    ## (6B) Defining Server Logic ==============================================
    
        server_assign <- function(id, shared_data) {
        ns_a <- NS(id)
        moduleServer(id, function(input, output, session) {
            
            # Helper function to render enabled or disabled buttons
            render_button <- function(button_name,
                                      label,
                                      status){
                if (status == "enable") {
                    return(renderUI({actionButton(session$ns(button_name), 
                                                  label,
                                                  style = "width: 100%; font-size: 12px; color: white; background-color: #4e2a85;")}))
                }
                else if (status == "disable") {
                    return(renderUI({disabled(actionButton(session$ns(button_name), 
                                                           label,
                                                           style = "width: 100%; font-size: 12px;"))}))
                }
            }
            # Helper function to render enabled or disabled download buttons
            render_download_button <- function(button_name,
                                      label,
                                      status){
                if (status == "enable") {
                    return(renderUI({downloadButton(session$ns(button_name), 
                                   label,
                                   style = "width: 100%; font-size: 12px; color: white; background-color: #4e2a85;")}))
                }
                else if (status == "disable") {
                    return(renderUI({disabled(downloadButton(session$ns(button_name), 
                                   label,
                                   style = "width: 100%; font-size: 12px;"))}))
                }
            }
            # Helper function to reset assignment panel
            reset_mainpanel <- function(){
                task_load_data(data.frame())
                valid_loads(TRUE) 
                randomized_results(data.frame())
                output$tasks <- NULL
                output$task_load <- NULL
                output$reassignment_details <- NULL
            }
            # Helper function to reset side panel buttons
            reset_sidepanel_buttons <- function(){
                output$randomize_button = render_button("randomize",
                                                    "Generate Randomizations",
                                                    "disable")
                output$download_tasks_button <- render_download_button("download_tasks",
                                                                       "Download Tasks",
                                                                       "disable")
                output$sync_with_paper_assign_button = render_button("sync",
                                                                 "Sync",
                                                                 "disable")
            }
            # Helper function to render Datatable
            render_datatable <- function(df,
                                         title,
                                         editable_cols,
                                         hide_cols) {
                if (length(editable_cols)==0) {
                    return(datatable(
                        df, 
                        escape = FALSE, 
                        selection = 'none', 
                        rownames = FALSE,
                        caption = title,
                        options = list(dom = 't', 
                                       paging = FALSE,
                                       ordering = FALSE,
                                       columnDefs = list(list(targets = "_all", className = "dt-center"),
                                                         list(targets = hide_cols, visible = FALSE)),
                                       initComplete = JS("function(){$(this).addClass('compact'); $(this.api().table().header()).css({'background-color': '#4e2a85', 'color': 'white'}); }"),
                                       preDrawCallback = DT::JS("function(settings) {
                                                                 Shiny.unbindAll(this.api().table().node());
                                                                 }"),
                                       drawCallback = DT::JS("function(settings) {
                                                              Shiny.initializeInputs(this.api().table().node());
                                                              Shiny.bindAll(this.api().table().node());
                                                          }")
                                       )))
                }
                else {
                    return(datatable(
                        df, 
                        escape = FALSE, 
                        selection = 'none', 
                        rownames = FALSE,
                        caption = title,
                        editable = list(
                                target = 'cell', 
                                disable = list(columns = editable_cols)
                                ),
                        options = list(dom = 't', 
                                       paging = FALSE,
                                       ordering = FALSE,
                                       columnDefs = list(list(targets = "_all", className = "dt-center"),
                                                         list(targets = hide_cols, visible = FALSE)),
                                       initComplete = JS("function(){$(this).addClass('compact'); $(this.api().table().header()).css({'background-color': '#4e2a85', 'color': 'white'}); }"),
                                       preDrawCallback = DT::JS("function(settings) {
                                                                 Shiny.unbindAll(this.api().table().node());
                                                                 }"),
                                       drawCallback = DT::JS("function(settings) {
                                                              Shiny.initializeInputs(this.api().table().node());
                                                              Shiny.bindAll(this.api().table().node());
                                                          }")
                        )))}
            }
            
            main_reactive = reactiveVal(isolate(shared_data$main_online))
            seed_tracker_reactive = reactiveVal(isolate(shared_data$seed_tracker_online))
            staff = reactiveVal(isolate(shared_data$staff_online))
            st_nrows <- toString(nrow(seed_tracker_reactive()))
            set.seed(as.numeric(st_nrows))
            seed_value <- as.integer(runif(1, min=1000, max=9999))
            
            coder <- as.vector(na.omit(staff()[staff()$position == "Coder", ]$name))
            subsup <- as.vector(na.omit(staff()[staff()$position == "Sub-Supervisor", ]$name))
            sup <- as.vector(na.omit(staff()[staff()$position == "Supervisor", ]$name))
            
            task_load_data <- reactiveVal(data.frame(`Name` = character(0),
                                       `Role` = character(0),
                                       `Assign` = integer(0),
                                       `Valid` = integer(0),
                                       check.names = FALSE)) 
            
            valid_loads <- reactiveVal(TRUE) 
            chosen_s1 <- reactiveVal(c())
            chosen_s1c <- reactiveVal(c())
            chosen_s2 <- reactiveVal(c())
            studies_universe <- reactiveVal(c())
            studies_universe(main_reactive() %>% filter(assignment_status == 'to be assigned' |
                                                            assignment_status == 'assigned' |
                                                            assignment_status == 're-assigned') %>%
                                 dplyr::pull(studyID))
            
            randomized_results = reactiveVal(data.frame())
            
            output$username_UI = renderUI({selectizeInput(inputId = session$ns("username"),
                                                          label = "Supervisor Name",
                                                          choices = c(sup, subsup),
                                                          multiple = FALSE)})

            output$choose_study_UI = renderUI({selectizeInput(inputId = session$ns("chosen_studies"),
                                                              label = "Study ID",
                                                              choices = c(studies_universe()),
                                                              multiple = TRUE)})
            
            output$randomize_button = render_button("randomize",
                                                    "Generate Randomizations",
                                                    "disable")
            output$download_tasks_button <- render_download_button("download_tasks",
                                                                   "Download Tasks",
                                                                   "disable")
            
            output$sync_with_paper_assign_button = render_button("sync",
                                                                 "Sync",
                                                                 "disable")
            
            # Determine initial validity and update "Randomization" button
            observe({
                if (length(input$chosen_studies) > 0 &&
                    length(input$roles)>0 &&
                    xnor("S1" %in% input$roles, length(chosen_s1())>0) &&
                    xnor("S1 Check" %in% input$roles, length(chosen_s1c())>0) &&
                    xnor("S2" %in% input$roles, length(chosen_s2())>=(2+case_when(grepl("Single", input$assignment_type) ~ 0, input$assignment_type == "Double" ~ 1))) &&
                    valid_loads() &&
                    input$username != "" &&
                    nrow(randomized_results())==0) {
                    output$randomize_button = render_button("randomize",
                                                    "Generate Randomizations",
                                                    "enable")
                    output$download_tasks_button <- render_download_button("download_tasks",
                                                                           "Download Tasks",
                                                                           "disable")
                    output$sync_with_paper_assign_button <- render_button("sync",
                                                                          "Sync",
                                                                          "disable")
                }
                else {
                    output$randomize_button = render_button("randomize",
                                                    "Generate Randomizations",
                                                    "disable")
                    if (nrow(randomized_results())>0) {
                        output$download_tasks_button <- render_download_button("download_tasks",
                                                                       "Download Tasks",
                                                                       "enable")
                        output$sync_with_paper_assign_button <- render_button("sync",
                                                         "Sync",
                                                         "enable")
                    }
                }
            })
            
            # Filter studies
            observeEvent(input$studies_filter, {
                if ("All" %in% input$studies_filter) {
                    studies_universe(main_reactive() %>% filter(assignment_status == 'to be assigned' |
                                                                    assignment_status == 'assigned' |
                                                                    assignment_status == 're-assigned') %>%
                                         dplyr::pull(studyID))
                    return()
                }
                studies_universe(c())
                if ("To be assigned" %in% input$studies_filter) {
                    studies_universe(c(studies_universe(), 
                                       main_reactive() %>% filter(assignment_status == 'to be assigned') %>%
                                           dplyr::pull(studyID)))
                }
                if ("Assigned" %in% input$studies_filter) {
                    studies_universe(c(studies_universe(), 
                                       main_reactive() %>% filter(assignment_status == 'assigned') %>%
                                           dplyr::pull(studyID)))
                }
                if ("Re-assigned" %in% input$studies_filter) {
                    studies_universe(c(studies_universe(), 
                                       main_reactive() %>% filter(assignment_status == 're-assigned') %>%
                                           dplyr::pull(studyID)))
                }
                if ("Cash Transfers" %in% input$studies_filter) {
                    studies_universe(c(studies_universe(), 
                                       main_reactive() %>% filter(grepl("1",Intervention)) %>%
                                           dplyr::pull(studyID)))
                }
                if ("Business Training" %in% input$studies_filter) {
                    studies_universe(c(studies_universe(), 
                                       main_reactive() %>% filter(grepl("2",Intervention)) %>%
                                           dplyr::pull(studyID)))
                }
                if ("Microcredit" %in% input$studies_filter) {
                    studies_universe(c(studies_universe(), 
                                       main_reactive() %>% filter(grepl("3",Intervention)) %>%
                                           dplyr::pull(studyID)))
                }
                if ("Savings" %in% input$studies_filter) {
                    studies_universe(c(studies_universe(), 
                                       main_reactive() %>% filter(grepl("5",Intervention)) %>%
                                           dplyr::pull(studyID)))
                }
                if ("Graduation" %in% input$studies_filter) {
                    studies_universe(c(studies_universe(), 
                                       main_reactive() %>% filter(grepl("4",Intervention)) %>%
                                           dplyr::pull(studyID)))
                }
            })
            
            # Study choosing and Re-assignments inputs
            observeEvent(list(input$chosen_studies, input$assignment_type), {
                
                double_code <- case_when(grepl("Single", input$assignment_type) ~ 0,
                                      input$assignment_type == "Double" ~ 1)
                
                reassign_studies = main_reactive() %>% 
                    filter(studyID %in% input$chosen_studies &
                               assignment_status %in% c("assigned", "re-assigned")) %>%
                               pull(studyID)
                
                # Omit re-assignment input table if none of chosen studies were assigned/re-assigned
                if (length(input$chosen_studies) == 0 ||
                    input$assignment_type != "Single (replace)" ||
                    length(reassign_studies) == 0) {
                    output$reassignment_details = DT::renderDataTable(
                        render_datatable(data.frame(),
                                         "Re-assignment Matrix",
                                         c(),
                                         integer(0)))
                    
                    task_load_static <- task_load_data()
                    if (nrow(task_load_static)>0) {
                        output$task_load <- renderDT({
                            render_datatable(task_load_static,
                                             "Task Load Matrix",
                                             c(0, 1),
                                             c(3))})
                        output$assignment_conditions_helptext <- renderUI(
                            helpText("Assignment conditions:",
                                     tags$ul(
                                         tags$li("Number of tasks assigned must not exceed tasks available"),
                                         tags$li("S1/S1C/S2 tasks cannot be assigned to one coder entirely"),
                                         tags$li("The same person cannot be assigned more than half the S1 and S1C simultaneously")
                                     )))
                    }
                    else {
                        output$task_load <- NULL
                        output$assignment_conditions_helptext <- NULL
                    }
                    # Reset if needed
                    randomized_results(data.frame())
                    return()
                }
                                                                                                                          
                # Render re-assignment Datatable with selectize input
                reassign_studies_input_table = data.frame(`Status` = character(0),
                                                          `Study ID` = character(0),
                                                          `Keep` = character(0),
                                                          check.names = FALSE)
                for (i in reassign_studies) {
                    coders = strsplit(main_reactive()$coders[which(main_reactive()$studyID==i)], ';')[[1]]
                    reassign_studies_input_table <- reassign_studies_input_table %>%
                        add_row(
                            `Status` = main_reactive()$status[which(main_reactive()$studyID==i)],
                            `Study ID` = i,
                            `Keep` = as.character(selectInput(session$ns(i), "" , c(coders[1], coders[2]), selected=coders[1], width="100%"))
                        )
                }
                
                output$reassignment_details = DT::renderDataTable(
                    render_datatable(reassign_studies_input_table,
                                     "Re-assignment Matrix",
                                     c(),
                                     integer(0)) %>%
                        formatStyle(
                            columns = names(reassign_studies_input_table),
                            fontSize = '12px') %>%
                        formatStyle("Status",
                                    backgroundColor="#e6e6e6") %>%
                        formatStyle("Study ID",
                                    backgroundColor="#e6e6e6"))

                # Render task load dataframe using non-reactive values to
                # avoid re-rendering table everytime reactive value is updated
                task_load_static <- task_load_data()
                output$task_load <- renderDT({
                    render_datatable(task_load_static,
                                     "Task Load Matrix",
                                     c(0, 1),
                                     c(3))})
                if (nrow(task_load_static)>0) {
                    output$task_load <- renderDT({
                        render_datatable(task_load_static,
                                         "Task Load Matrix",
                                         c(0, 1),
                                         c(3))})
                    output$assignment_conditions_helptext <- renderUI(
                        helpText("Assignment conditions:",
                                 tags$ul(
                                     tags$li("Number of tasks assigned must not exceed tasks available"),
                                     tags$li("S1/S1C/S2 tasks cannot be assigned to one coder entirely"),
                                     tags$li("The same person cannot be assigned more than half the S1 and S1C simultaneously")
                                 )))
                }
                else {
                    output$task_load <- NULL
                    output$assignment_conditions_helptext <- NULL
                }
                # Reset if needed
                randomized_results(data.frame())
                
            }, ignoreNULL = FALSE)
            
            # Render selectizeinputs for randomization components 
            observeEvent(input$roles, {
              people_elements <- list() 
                
                if ("S1" %in% input$roles) {
                    people_elements <- list(people_elements, selectizeInput(
                        inputId = session$ns("chosen_s1"),
                        label = "S1",
                        choices = c(sup, subsup),
                        multiple = TRUE
                    ))
                }
                if ("S1 Check" %in% input$roles) {
                    people_elements <- list(people_elements, selectizeInput(
                        inputId = session$ns("chosen_s1c"),
                        label = "S1 Check",
                        choices = c(sup, subsup),
                        multiple = TRUE
                    ))
                }
                if ("S2" %in% input$roles) {
                    people_elements <- list(people_elements, selectizeInput(
                        inputId = session$ns("chosen_s2"),
                        label = "S2",
                        choices = coder,
                        multiple = TRUE
                    ))
                }
                output$people <- renderUI({
                    tagList(people_elements)
                })

                output$tasks <- NULL
                output$task_load <- NULL
                
                # Reset if needed
                randomized_results(data.frame())
            })
            
            # Render Datatable based on available s1/s1c/s2
            observeEvent(list(input$chosen_s1, input$chosen_s1c, input$chosen_s2), {
              
              if ("S1" %in% input$roles) {
                  chosen_s1(input$chosen_s1)
              }
              else {
                  chosen_s1(NULL)
              }
              if ("S1 Check" %in% input$roles) {
                  chosen_s1c(input$chosen_s1c)
              }
              else {
                  chosen_s1c(NULL)
              }
              if ("S2" %in% input$roles) {
                  chosen_s2(input$chosen_s2)
              }
              else {
                  chosen_s2(NULL)
              }
              
              task_load_data(data.frame(`Name` = character(0),
                                       `Role` = character(0),
                                       `Assign` = integer(0),
                                       `Valid` = integer(0),
                                       check.names = FALSE))
                
                if ("S1" %in% input$roles & length(chosen_s1()) > 0 & length(input$chosen_studies) > 0) {
                        task_load_data(rbind(task_load_data(),
                            data.frame(
                                Name = sort(chosen_s1()),
                                Role = rep("S1", length(chosen_s1())), 
                                Assign = 0,
                                `Valid` = 1
                                #Assign = pmax(0, length(input$chosen_studies) %/% length(chosen_s1()))
                            )
                        ))
                }
                if ("S1 Check" %in% input$roles & length(chosen_s1c()) > 0 & length(input$chosen_studies) > 0) {
                        task_load_data(rbind(task_load_data(),
                            data.frame(
                                Name = sort(chosen_s1c()),
                                Role = rep("S1C", length(chosen_s1c())), 
                                Assign = 0,
                                `Valid` = 1
                                #Assign = pmax(0, length(input$chosen_studies) %/% length(chosen_s1c()))
                            )
                        ))
                }
                if ("S2" %in% input$roles & length(chosen_s2()) > 0 & length(input$chosen_studies) > 0) {
                        task_load_data(rbind(task_load_data(),
                            data.frame(
                                Name = sort(chosen_s2()),
                                Role = rep("S2", length(chosen_s2())), 
                                Assign = 0,
                                `Valid` = 1
                                #Assign = pmax(0, length(input$chosen_studies) %/% length(chosen_s2()))
                            )
                        ))
                }
                task_load_static <- task_load_data()
                output$task_load <- renderDT({
                    render_datatable(task_load_static,
                                     "Task Load Matrix",
                                     c(0, 1),
                                     c(3))})
                if (nrow(task_load_static)>0) {
                    output$task_load <- renderDT({
                        render_datatable(task_load_static,
                                         "Task Load Matrix",
                                         c(0, 1),
                                         c(3))})
                    output$assignment_conditions_helptext <- renderUI(
                        helpText("Assignment conditions:",
                                 tags$ul(
                                     tags$li("Number of tasks assigned must not exceed tasks available"),
                                     tags$li("S1/S1C/S2 tasks cannot be assigned to one coder entirely"),
                                     tags$li("The same person cannot be assigned more than half the S1 and S1C simultaneously")
                                 )))
                }
                else {
                    output$task_load <- NULL
                    output$assignment_conditions_helptext <- NULL
                }
                # Reset if needed
                randomized_results(data.frame())
            })
            
            # Update Datatable based on task load + validation
            observeEvent(input$task_load_cell_edit, {

                # Extract the edit information
                info <- input$task_load_cell_edit
                r <- info$row
                c <- info$col + 1
                v <- info$value
                
                # Validate inputs (important to maintain valid randomization)
                if (is.na(as.integer(v))) {
                    valid_loads(FALSE)
                    return()
                }
                else {
                    valid_loads(TRUE)
                }
                
                num_studies = length(input$chosen_studies)
                task_load_data_BEND <- task_load_data()
                task_load_data_BEND[r, c] = v
                task_load_data(task_load_data_BEND)
                s1 = task_load_data()[which(task_load_data()$Role == "S1"), ]
                s1c = task_load_data()[which(task_load_data()$Role == "S1C"), ]
                s2 = task_load_data()[which(task_load_data()$Role == "S2"), ]
                double_code <- case_when(grepl("Single", input$assignment_type) ~ 0,
                                      input$assignment_type == "Double" ~ 1)
                msg = ""
                
                if (task_load_data()[r, "Role"] == "S1") {
                    if (sum(s1$Assign) > num_studies | # num assigned < num available
                        (nrow(s1[which(s1$Assign!=0), ])==1 & sum(s1$Assign)==num_studies))  { # num assigned to one person < num available
                        valid_loads(FALSE)
                        msg = "Exceeding Tasks Per S1 Coder"
                    }
                    else {
                        valid_loads(TRUE)
                    }
                }
                else if (task_load_data()[r, "Role"] == "S1C") {
                    if (sum(s1c$Assign) > num_studies |
                        (nrow(s1c[which(s1c$Assign!=0), ])==1 & sum(s1c$Assign)==num_studies))  {
                        valid_loads(FALSE)
                        msg = "Exceeding Tasks Per S1C Coder"
                    }
                    else {
                        valid_loads(TRUE)
                    }
                }
                else if (task_load_data()[r, "Role"] == "S2") {
                    if ((sum(s2$Assign) > (num_studies*(double_code+1))) | # num assigned < available
                        (nrow(s2[which(s2$Assign!=0), ])==1 & sum(s2$Assign)>=(num_studies+double_code) |
                         (sum(s2$Assign>=num_studies) > 1)))  {
                        valid_loads(FALSE)
                        msg = "Exceeding Tasks Per S2 Coder"
                    }
                    else {
                        valid_loads(TRUE)
                    }
                }
                
                # Validate if someone is assigned both S1 and S1C, they are not
                # specified to have more than half the task in each component
                for (ind in s1$Name) {
                    if (!(ind %in% chosen_s1c())) {
                        next
                    }
                    else {
                        if ((s1$Assign[which(s1$Name == ind)] > length(input$chosen_studies) %/% 2) &
                                (s1c$Assign[which(s1c$Name == ind)] > length(input$chosen_studies) %/% 2)) {
                            valid_loads(FALSE)
                            msg = "Invalid S1/S1C configuration: same S1 and S1C would be assigned to study"
                        }
                        else if ((s1$Assign[which(s1$Name == ind)] == length(input$chosen_studies)) &
                                (s1c$Assign[which(s1c$Name == ind)] > 0)) {
                            valid_loads(FALSE)
                            msg = "Invalid S1/S1C configuration: same S1 and S1C would be assigned to study"
                        }
                        else if ((s1c$Assign[which(s1c$Name == ind)] == length(input$chosen_studies)) &
                                (s1$Assign[which(s1$Name == ind)] > 0)) {
                            valid_loads(FALSE)
                            msg = "Invalid S1/S1C configuration: same S1 and S1C would be assigned to study"
                        }
                    }
                }
                
                if (!valid_loads()) {
                    task_load_data_BEND <- task_load_data()
                    task_load_data_BEND[r, c+1] = 0
                    task_load_data(task_load_data_BEND)
                    output$task_load <- renderDT({
                        render_datatable(task_load_data_BEND,
                                         "Task Load Matrix",
                                         c(0, 1),
                                         c(3)) %>%
                            formatStyle("Assign", "Valid",
                                        backgroundColor = styleEqual(c(0, 1),c('#fcf9e5',"")))})
                    showNotification(msg, type = "warning")
                }
                else {
                    task_load_data_BEND <- task_load_data()
                    task_load_data_BEND[r, c+1] = 1
                    task_load_data(task_load_data_BEND)
                    task_load_data_BEND <- task_load_data()
                    output$task_load <- renderDT({
                        render_datatable(task_load_data_BEND,
                                         "Task Load Matrix",
                                         c(0, 1),
                                         c(3))})
                }
                # Reset if needed
                randomized_results(data.frame())
            })
            
            # All information ok, generate randomized assignments
            observeEvent(input$randomize, {
                
                set.seed(seed_value)
                
                # Helper function to generate S1-S1C-S2 draws
                # (decides number of tasks each person has), returns linked database assignment
                # if component not in randomization list
                draw <- function(task_load, components, num_studies, studyIDs) {
                  fixed_s1 <- task_load %>%
                    filter(Role == "S1") %>%
                    arrange(desc(Assign))
                  fixed_s1c <- task_load %>%
                    filter(Role == "S1C") %>%
                    arrange(desc(Assign))
                  fixed_s2 <- task_load %>%
                    filter(Role == "S2") %>%
                    arrange(desc(Assign))

                  s1 = c()
                  s1c = c()
                  s2 = c()
                  
                  if ("S2" %in% components) {
                    for (i in fixed_s2$Name[which(fixed_s2$Assign != 0)]) {
                        assign = fixed_s2$Assign[which(fixed_s2$Name == i)]
                        s2 = c(s2, rep(i, assign))
                    }
                    remain = (num_studies * (double_code+1)) - length(s2)
                    s2 = c(s2, sample(fixed_s2$Name[which(fixed_s2$Assign == 0)], 
                                      remain, 
                                      replace=TRUE))
                    s2 = sample(s2)
                    }
                    else {
                        s2 <- unlist(lapply(studyIDs, function(x) main_reactive()$coders[which(main_reactive()$studyID == x)]))
                        if (sum(is.na(s2))>0) {
                            s2[is.na(s2)] = "unassigned_s2"
                        }
                    }
                    # Generate vector of randomized S1C
                    if ("S1 Check" %in% components) {
                    for (i in fixed_s1c$Name[which(fixed_s1c$Assign != 0)]) {
                        assign = fixed_s1c$Assign[which(fixed_s1c$Name == i)]
                        s1c = c(s1c, rep(i, assign))
                    }
                    remain = num_studies - length(s1c)
                    s1c = c(s1c, sample(fixed_s1c$Name[which(fixed_s1c$Assign == 0)], 
                                      remain, 
                                      replace=TRUE))
                    s1c = sample(s1c)
                    }
                    else {
                        s1c <- unlist(lapply(studyIDs, function(x) strsplit(main_reactive()$supervisors[which(main_reactive()$studyID == x)], ';')[[1]][1]))
                        if (sum(is.na(s1c))>0) {
                            s1c[is.na(s1c)] = "unassigned_s1c"
                        }
                    }
                    # Generate vector of randomized S1
                    if ("S1" %in% components) {
                    for (i in fixed_s1$Name[which(fixed_s1$Assign != 0)]) {
                        assign = fixed_s1$Assign[which(fixed_s1$Name == i)]
                        s1 = c(s1, rep(i, assign))
                    }
                    remain = num_studies - length(s1)
                    s1 = c(s1, sample(fixed_s1$Name[which(fixed_s1$Assign == 0)], 
                                      remain, 
                                      replace=TRUE))
                    s1 = sample(s1)
                    }
                    else {
                        s1 <- unlist(lapply(studyIDs, function(x) strsplit(main_reactive()$supervisors[which(main_reactive()$studyID == x)], ';')[[1]][2]))
                        if (sum(is.na(s1))>0) {
                            s1[is.na(s1)] = "unassigned_s1"
                        }
                    }
                  
                  return (list(s1, s1c, s2))
                  
                }
                
                double_code <- case_when(grepl("Single", input$assignment_type) ~ 0,
                                      input$assignment_type == "Double" ~ 1)
                studyIDs = sort(input$chosen_studies)
                task_names = unlist(lapply(studyIDs, function(x) main_reactive()$task_name[which(main_reactive()$studyID == x)]))
                titles = unlist(lapply(studyIDs, function(x) main_reactive()$title[which(main_reactive()$studyID == x)]))
                num_studies = length(studyIDs)
                valid_randomized_results = F
                counter = 0
                
                # Randomize tasks
                while (!valid_randomized_results & counter < 500) {
                    
                    # Draw S1-S1C-S2
                    res = draw(task_load_data(), 
                               input$roles,
                               num_studies,
                               studyIDs)
                    s1 = res[[1]]
                    s1c = res[[2]]
                    s2 = res[[3]]
                    
                    if (double_code == 0 | !("S2" %in% input$roles)) {
                        assignments <- cbind(s1, s1c, s2)
                    }
                    else if (double_code == 1) {
                        shuffled_s2 = sample(s2)
                        s2_first_half = shuffled_s2[1:num_studies]
                        s2_second_half = shuffled_s2[(num_studies+1):(num_studies*2)]
                        assignments <- cbind(s1, s1c, s2_first_half, s2_second_half)
                    }
                    # Check if anyone is assigned on more than component (e.g. both S1 and S1C)
                    duplicated <- apply(assignments, 1, function(row) length(unique(row)) != ncol(assignments))
                    if (any(duplicated)) {
                        counter = counter + 1
                    }
                    else {
                        valid_randomized_results = T
                    }
                }
                if (!valid_randomized_results) {
                    msg = "Unable to generate valid assignments within 500 shuffles"
                    showNotification(msg, type = "warning")
                    randomized_results(data.frame())
                    return()
                }
                seed_tracker_new_rows = data.frame(`Task Name` = task_names,
                                                   `Generated Study ID` = studyIDs,
                                                   `Full Title` = titles,
                                                   `Coder Assignment` = if (double_code == 1) apply(assignments[, 3:ncol(assignments), drop = FALSE], 1, function(row) paste(row, collapse = ";")) else assignments[, 3],
                                                   `Sub-Supervisor Assignment` = assignments[, "s1"],
                                                   `Supervisor Assignment` = assignments[, "s1c"],
                                                   `Stage 1` = 1,
                                                   check.names=FALSE)
                tasks <- separate_rows(seed_tracker_new_rows, 
                                       `Coder Assignment`, 
                                       sep = ";")

                output$tasks <- renderDT({
                    render_datatable(tasks[ , c("Task Name", "Coder Assignment", "Sub-Supervisor Assignment", "Supervisor Assignment")],
                                     "Random Assignment Results",
                                     c(),
                                     integer(0))

                    })
                
                randomized_results(seed_tracker_new_rows)
                
                # Add content to download button
                output$download_tasks <- downloadHandler(
                    filename = function() {
                        paste0("tasks (",Sys.Date(),").csv")
                    },
                    content=function(file) {
                        write.csv(tasks, file)
                                    })
                
            })
            
            # Update linked databases
            observeEvent(input$sync, {
                double_code <- case_when(grepl("Single", input$assignment_type) ~ 0,
                                         input$assignment_type == "Double" ~ 1)  
                
                seed_tracker_new_rows <- randomized_results() %>%
                    dplyr::rename(
                        studyID = `Generated Study ID`,
                        coders = `Coder Assignment`
                        ) %>%
                    rowwise() %>%
                    mutate(
                      date = format(Sys.time()),
                      `session` = nrow(seed_tracker_reactive()),
                      supervisors = paste(`Supervisor Assignment`, `Sub-Supervisor Assignment`, sep=';'),
                      seed = seed_value,
                      action = case_when(
                          main_reactive()$assignment_status[which(main_reactive()$studyID == studyID)] == "to be assigned" ~ "assigned",
                          main_reactive()$assignment_status[which(main_reactive()$studyID == studyID)] %in% c("assigned", "re-assigned") ~ "re-assigned"
                          ),
                      staff = toString(task_load_data()[, 1:3])
                      ) %>%
                    dplyr::select(
                        date,
                        `session`,
                        studyID,
                        supervisors,
                        coders,
                        seed,
                        action,
                        staff)
              
              # Update seed_tracker
              withProgress(message = 'Upload Progress: seed_tracker', value = 0, {
                  sheet_append(ss = drive_get(paper_assign_filename)$id,
                                 data = seed_tracker_new_rows,
                                 sheet = "seed_tracker")
                  incProgress(1)
              })
              
              # Update paper_assign
              withProgress(message = 'Upload Progress: paper_assign', value = 0, {
              for (i in 1:nrow(seed_tracker_new_rows)) {
                  
                  study_id <- seed_tracker_new_rows$studyID[i]
                  incProgress(0, detail = study_id)
                  
                  # Find the row in paper_assign with the matching studyID
                  match_idx <- which(main_reactive()$studyID == study_id)

                  # Match found
                  if (length(match_idx) == 1) {
                      tmp = main_reactive()
                      tmp$assignment_status[match_idx] = seed_tracker_new_rows$action[i]
                      tmp$coders[match_idx] = case_when(
                          input$assignment_type == "Single (replace)" ~ paste0(input[[study_id]], ";", seed_tracker_new_rows$coders[i]),
                          input$assignment_type == "Single (new)" ~ paste0(main_reactive()$coders[match_idx], ";", seed_tracker_new_rows$coders[i]),
                          input$assignment_type == "Double" ~ seed_tracker_new_rows$coders[i]
                      )
                      tmp$supervisors[match_idx] <- seed_tracker_new_rows$supervisors[i]
                      tmp$date_first_assigned[match_idx] <- format(Sys.time())
                      tmp$last_updated_by[match_idx] <- input$username
                      main_reactive(tmp)
                      
                      range_write(ss = drive_get(paper_assign_filename)$id,
                                data = tmp[match_idx, ],
                                sheet = "assignment",
                                range = sprintf("A%s", (match_idx+1)),
                                col_names = F)
                      
                  }
                  incProgress(1/nrow(seed_tracker_new_rows), detail = study_id)
              }
              })
              })

        })
    }
    
# (7) Defines the User Interface of the "Home" Module =========================

    ui_home <- function(id) {
        fluidPage(
            h4("Overview"),
            p("The DEVIDENCE Paper Assignment application allows users to make pre-approved
              edits to the paper_assign google workbook and two of its three sheets: 'assignment', 
              and 'assign_seed_tracker'. The 'assignment' sheet keeps track of the most updated 
              information of each study while 'assign_seed_tracker' records all activity involving 
              randomization. One will be able to infer all activity regarding a paper by 
              combining the two sheets. Below, each module's functionality is outlined."),
            h4("Adding Papers"),
            p("This module allows users to add new papers to the 'assignment' sheet, by
              uploading necessary study information and its PDF. The module then
              compares the proposed new study with existing studies in the database
              and determines eligibility."),
            h4("Screening/Editing Study Info"),
            p("This module allows users to edit high level information of studies in the database
              with high flexibility, by 'directly' editting the 'assignment' sheet like a simple spreadsheet. 
              However, only valid edits are saved and can be uploaded."),
            h4("Assigning Papers"),
            p("This module allows users to assign 'ready to be assigned' papers
              to the staff with improved flexibility. Upon clicking 'assign', the module then randomizes 
              paper assignment and generates valid randomizations based on the user provided parameters."),
            hr(),
            h4("App Development Team"),
            p("Irena Petryk, Makayla Barker, Ka Wui Jimmy Yung")
        )
    }
    
    
    
# (8) Defines the User Interface and Server of the Application ================

    app_ui <- function(request) {
        fluidPage(
            navbarPage(
                title = div("DEVIDENCE Paper Assignment",
                            img(src = "Global_Poverty_Research_Lab_Lockup_VERT__Purple.png", height = "35px", width = "auto", 
                                style = "position: absolute;
                                top: 7.5px;
                                right: 2%;")),
                id = "main_tabs", # Add an id to track the current tab
                tabPanel("Home", ui_home("home")),
                tabPanel("Adding", uiOutput("adding_ui")),
                tabPanel("Screening/Edit Study Info", uiOutput("screening_ui")),
                tabPanel("Assigning", uiOutput("assigning_ui")),
            )
        )
    }
    
    app_server <- function(input, output, session) {
        shared_data <- reactiveValues(
            main_online = NULL,
            seed_tracker_online = NULL,
            staff_online = NULL
        )
        token <- reactiveVal(0)
        
        observeEvent(input$main_tabs, {
            
            shinyjs::disable("main_tabs")
            
            if (input$main_tabs != "Home") {
                msg = "Loading module \n"
                showNotification(msg, type = "default")
                cat(green(msg))
                
                
                # Dynamically recreate modules for each tab
                if (input$main_tabs == "Adding") {
                    tmp = token() + 1
                    token(tmp)
                    shared_data$main_online <- as.data.frame(read_sheet(drive_get(paper_assign_filename)$id, sheet = "assignment"))
                    shared_data$staff_online <- as.data.frame(read_sheet(drive_get(paper_assign_filename)$id, sheet = "staff"))
                    output$adding_ui <- renderUI({
                        ui_adding_papers(as.character(token()))
                    })
                    server_adding_papers(as.character(token()), shared_data)
                }
                
                if (input$main_tabs == "Screening/Edit Study Info") {
                    tmp = token() + 1
                    token(tmp)
                    shared_data$main_online <- as.data.frame(read_sheet(drive_get(paper_assign_filename)$id, sheet = "assignment"))
                    shared_data$staff_online <- as.data.frame(read_sheet(drive_get(paper_assign_filename)$id, sheet = "staff"))
                    output$screening_ui <- renderUI({
                        ui_screening_papers(as.character(token()))
                    })
                    server_screening_papers(as.character(token()), shared_data)
                }
                
                if (input$main_tabs == "Assigning") {
                    tmp = token() + 1
                    token(tmp)
                    shared_data$main_online <- as.data.frame(read_sheet(drive_get(paper_assign_filename)$id, sheet = "assignment"))
                    shared_data$seed_tracker_online <- as.data.frame(read_sheet(drive_get(paper_assign_filename)$id, sheet = "seed_tracker"))
                    shared_data$staff_online <- as.data.frame(read_sheet(drive_get(paper_assign_filename)$id, sheet = "staff"))
                    output$assigning_ui <- renderUI({
                        ui_assign(as.character(token()))
                    })
                    server_assign(as.character(token()), shared_data)
                }
                shinyjs::enable("main_tabs")
            }
        })
    }
    
    
# (9) Runs the Papers Shiny Application =======================================
    
    options(shiny.maxRequestSize = 100*1024^2)
    shinyApp(ui = app_ui, server = app_server)
    
