################################################################################
#
# Server content
#
################################################################################
server <- function(input, output, session) {
  session$onSessionEnded(shiny::stopApp)
  
  # Connection to database
  availableCohorts <-
    getCohorts(conn, dbms = connectionDetails$dbms, resultsSchema = cdmResultsSchema)
  
  ## Choose target cohort
  output$targetCohort <- renderUI({
    cohorts <- availableCohorts
    radioButtons("ctargetCohort", label = "", choices = cohorts)
  })
  
  ## Choose state cohorts
  output$stateCohorts <- renderUI({
    cohorts <- availableCohorts
    checkboxGroupInput("cstateCohorts", label = "", choices = cohorts)
  })
  
  
  
  
  ################################################################################
  #
  # Initiating reactive values
  #
  ################################################################################
  v <-
    reactiveValues(
      data = NULL,
      nrStatesJSON = NULL,
      insertedJSONs =
        if (studyHasBeenSaved) {
          insertedJSONs
        } else {
          NULL
        },
      customisedStates =
        if (studyHasBeenSaved) {
          stateNamesJSON
        } else {
          NULL
        },
      customisedTarget = NULL,
      patientData = NULL,
      profileStochasticPlot = NULL,
      patientCostInfo = NULL,
      kmData = NULL,
      kmAgeAnalysis = FALSE,
      kmIntervalIds = NULL,
      generatedData = NULL,
      trajectoryTable1 = NULL,
      trajectoryTable2 = NULL,
      allowedTransitions = NULL
    )
  ################################################################################
  #
  # Import selected data from ATLAS. We will also nullify the customized labels
  # because otherwise we will counter a length mismatch
  #
  ################################################################################
  observeEvent(input$importDataButton, {
    v$data <-
      getCohortData(
        conn,
        dbms = connectionDetails$dbms,
        resultsSchema = cdmResultsSchema,
        cdmSchema = cdmSchema,
        selectedTarget = input$ctargetCohort,
        selectedStates = input$cstateCohorts,
        baseUrl = baseUrl,
        pathToResults = pathToResults
      )
    v$customisedStates <- input$cstateCohorts
    v$customisedTarget <- 0
    save_object(
      path =  paste(pathToResults, "/tmp/datasets/importedData.csv", sep = ""),
      object = v$data
    )
  })
  
  ##############################################################################
  #
  # Import using JSON
  #
  ##############################################################################
  
  observeEvent(input$importInstButtonJSON, {
    stateNamesJSON <<- list.files(
      path = paste(pathToResults, "/inst/JSON/", sep = ""),
      pattern = NULL,
      all.files = FALSE,
      full.names = FALSE
    )
    stateNamesJSON <<-
      substr(stateNamesJSON, 1, nchar(stateNamesJSON) - 5)
    
    if (length(stateNamesJSON) > 0) {
      jsonFiles <<- list.files(
        path = paste(pathToResults, "/inst/JSON/", sep = ""),
        pattern = NULL,
        all.files = FALSE,
        full.names = TRUE
      )
      
      targetIndex <<- which(stateNamesJSON == "0")
      stateNamesJSON <<- stateNamesJSON[-targetIndex]
      
      targetJSON <<-
        if (identical(jsonFiles[targetIndex], character(0)))
          ""
      else
        paste(readLines(jsonFiles[targetIndex]), collapse = "\n")
      jsonFiles <<-  jsonFiles[-targetIndex]
      
      insertedJSONs <<- c()
      
      for (jsonFile in jsonFiles) {
        insertedJSONs <<-
          c(insertedJSONs, paste(readLines(jsonFile), collapse = "\n"))
      }
      
      v$insertedJSONs <- insertedJSONs
      v$nrStatesJSON <- 1:length(stateNamesJSON)
      
      updateTextAreaInput(session,
                          "targetJSON",
                          label = "Target cohort JSON",
                          value = targetJSON)
      output$statesJSON <- renderUI({
        tagList(lapply(1:length(v$nrStatesJSON), function(i) {
          shinydashboard::box(
            width = 12,
            title = paste("State cohort ", v$nrStatesJSON[i], " JSON:"),
            status = "primary",
            solidHeader = TRUE,
            shiny::textInput(
              inputId = paste0('statesJSONLabel', v$nrStatesJSON[i]),
              label = "State cohort label",
              value = stateNamesJSON[i],
              placeholder = paste("State ", v$nrStatesJSON[i])
            ),
            shiny::textAreaInput(
              inputId = paste0('statesJSON', v$nrStatesJSON[i]),
              label = "State cohort JSON",
              value = v$insertedJSONs[i],
              placeholder = "STATE cohort JSON...",
              height = '250px'
            )
          )
        }))
      })
    }
  })
  observeEvent(input$addButtonJSON, {
    if (is.null(v$nrStatesJSON)) {
      v$nrStatesJSON <<- 1
    } else {
      v$nrStatesJSON <<- c(v$nrStatesJSON, max(v$nrStatesJSON) + 1)
      v$insertedJSONs <- c(v$insertedJSONs, "")
      v$customisedStates <- c(v$customisedStates, "")
    }
    output$statesJSON <- renderUI({
      tagList(lapply(1:length(v$nrStatesJSON), function(i) {
        shinydashboard::box(
          width = 12,
          title = paste("State cohort ", v$nrStatesJSON[i], " JSON:"),
          status = "primary",
          solidHeader = TRUE,
          shiny::textInput(
            inputId = paste0('statesJSONLabel', v$nrStatesJSON[i]),
            label = "State cohort label",
            value = v$customisedStates[i],
            placeholder = paste("State ", v$nrStatesJSON[i])
          ),
          shiny::textAreaInput(
            inputId = paste0('statesJSON', v$nrStatesJSON[i]),
            label = "State cohort JSON",
            value = v$insertedJSONs[i],
            placeholder = "STATE cohort JSON...",
            height = '250px'
          )
        )
      }))
    })
  })
  observeEvent(input$rmButtonJSON, {
    if (is.null(v$nrStatesJSON) | length(v$nrStatesJSON) == 1) {
      v$nrStatesJSON <- NULL
      output$statesJSON <- renderUI({
        NULL
      })
    } else {
      v$insertedJSONs <-
        v$insertedJSONs[v$nrStatesJSON < max(v$nrStatesJSON)]
      v$customisedStates <-
        v$customisedStates[v$nrStatesJSON < max(v$nrStatesJSON)]
      v$nrStatesJSON <-
        v$nrStatesJSON[v$nrStatesJSON < max(v$nrStatesJSON)]
      output$statesJSON <- renderUI({
        ifelse(is.null(v$nrStatesJSON), NULL,
               tagList(lapply(1:length(v$nrStatesJSON), function(i) {
                 shinydashboard::box(
                   width = 12,
                   title = paste("State cohort ", v$nrStatesJSON[i], " JSON:"),
                   status = "primary",
                   shiny::textInput(
                     inputId = paste0('statesJSONLabel', v$nrStatesJSON[i]),
                     label = "State cohort label",
                     value = v$customisedStates[i],
                     placeholder = paste("State ", v$nrStatesJSON[i])
                   ),
                   shiny::textAreaInput(
                     inputId = paste0('statesJSON', v$nrStatesJSON[i]),
                     label = "State cohort JSON",
                     value = v$insertedJSONs[i],
                     placeholder = "STATE cohort JSON...",
                     height = '250px'
                   )
                 )
                 
               })))
      })
    }
  })
  
  
  observeEvent(input$importDataButtonJSON, {
    v$insertedJSONs <- c(input$targetJSON)
    names <- c("0")
    for (id in 1:length(v$nrStatesJSON)) {
      v$insertedJSONs <-
        c(v$insertedJSONs, input[[paste("statesJSON", id, sep = "")]])
      names <-
        c(names, input[[paste("statesJSONLabel", id, sep = "")]])
    }
    v$customisedStates <- names[2:length(names)]
    v$customisedTarget <- names[1]
    v$data <-
      getJSONData(
        connection = conn,
        connectionDetails = connectionDetails,
        jsons = v$insertedJSONs ,
        names = v$customisedStates,
        cdmDataSchema =  cdmSchema,
        cdmTempSchema = cdmTmpSchema,
        studyName = studyName
      )
    
    save_object(
      path =  paste(pathToResults, "/tmp/datasets/importedData.csv", sep = ""),
      object = v$data
    )
    dropRelation(
      connection = conn,
      dbms = dbms,
      schema = cdmTmpSchema,
      relationName = studyName
    )
    dropRelation(
      connection = conn,
      dbms = dbms,
      schema = cdmTmpSchema,
      relationName = paste(studyName, "_censor_stats", sep = "")
    )
    dropRelation(
      connection = conn,
      dbms = dbms,
      schema = cdmTmpSchema,
      relationName = paste(studyName, "_inclusion", sep = "")
    )
    dropRelation(
      connection = conn,
      dbms = dbms,
      schema = cdmTmpSchema,
      relationName = paste(studyName, "_inclusion_stats", sep = "")
    )
    dropRelation(
      connection = conn,
      dbms = dbms,
      schema = cdmTmpSchema,
      relationName = paste(studyName, "_inclusion_result", sep = "")
    )
    dropRelation(
      connection = conn,
      dbms = dbms,
      schema = cdmTmpSchema,
      relationName = paste(studyName, "_summary_stats", sep = "")
    )
    ############################################################################
    #
    # Delete previous JSONs
    #
    ############################################################################
    do.call(file.remove, list(list.files(
      paste(pathToResults, "/inst/JSON/", sep = ""), full.names = TRUE
    )))
    ############################################################################
    #
    # Saving the JSONS
    #
    ############################################################################
    
    for (i in 1:length(names)) {
      fileConn <-
        file(paste(pathToResults, "/inst/JSON/", names[i], ".json", sep = ""))
      writeLines(v$insertedJSONs[i], fileConn)
      close(fileConn)
      #save_object(object = v$insertedJSONs[i], path = paste(getwd(),"/inst/JSON/",names[i], ".json", sep = ""))
    }
  })
  
  
  output$totalPatientsTargetJSON <- shinydashboard::renderInfoBox({
    if (input$importDataButtonJSON == 0)
      return(shinydashboard::infoBox(
        "Sample",
        "0",
        icon = shiny::icon("edit"),
        color = "yellow"
      ))
    isolate(
      shinydashboard::infoBox(
        "Sample",
        toString(length(unique(
          dplyr::filter(v$data,
                        COHORT_DEFINITION_ID == "0")$SUBJECT_ID
        ))),
        icon = shiny::icon("edit"),
        color = "yellow"
      )
    )
  })
  
  
  output$totalPatientsJSON <- shinydashboard::renderInfoBox({
    if (input$importDataButtonJSON == 0)
      return(shinydashboard::infoBox(
        "Unique",
        "0",
        icon = shiny::icon("address-card"),
        color = "light-blue"
      ))
    isolate(
      shinydashboard::infoBox(
        "Unique",
        toString(length(unique(
          v$data$SUBJECT_ID
        ))),
        icon = shiny::icon("address-card"),
        color = "light-blue"
      )
    )
  })
  
  output$TargetFilledJSON <- shinydashboard::renderInfoBox({
    if (input$importDataButtonJSON == 0)
      return(
        shinydashboard::infoBox(
          "Represented",
          "0 %",
          icon = shiny::icon("bullseye"),
          color = "purple"
        )
      )
    isolate(
      shinydashboard::infoBox(
        "Represented",
        paste(toString(round(
          length(intersect(
            unique(
              dplyr::filter(v$data,
                            COHORT_DEFINITION_ID != "0")$SUBJECT_ID
            ),
            unique(
              dplyr::filter(v$data,
                            COHORT_DEFINITION_ID == "0")$SUBJECT_ID
            )
          )) / length(unique(
            dplyr::filter(v$data,
                          COHORT_DEFINITION_ID == "0")$SUBJECT_ID
          )) * 100, 2
        )), " %"),
        icon = shiny::icon("bullseye"),
        color = "purple"
      )
    )
  })
  
  
  ##############################################################################
  #
  # Apply customized state names
  # Import data only if v$data is still NULL
  #
  ##############################################################################
  
  observeEvent(input$customiseButton, {
    # Add strip functionality
    v$customisedTarget <-
      stringi::stri_trim_both(input$customisedTarget)
    v$customisedStates <-
      stringi::stri_trim_both(unlist(strsplit(input$customisedStates, ",")))
    
    # Test: Check if the input is of same length as number of state
    
    if (length(v$customisedStates) != length(input$cstateCohorts)) {
      print(
        "Please add as many labels (separated by commas) as there are imported state cohorts!"
      )
      v$customisedStates <- NULL
    }
  })
  
  observeEvent(input$customiseButton, {
    if (!is.null(v$customisedStates)) {
      if (is.null(v$data)) {
        v$data <-
          getCohortData(
            conn,
            dbms = connectionDetails$dbms,
            cdmSchema = cdmSchema,
            resultsSchema = cdmResultsSchema,
            selectedTarget = input$ctargetCohort,
            selectedStates = input$cstateCohorts,
            baseUrl = baseUrl,
            pathToResults = pathToResults
          )
        v$data$COHORT_DEFINITION_ID <- plyr::mapvalues(
          x = v$data$COHORT_DEFINITION_ID,
          from = c("0", as.character(input$cstateCohorts)),
          to = c("0", v$customisedStates),
          warn_missing = FALSE
        )
        save_object(
          path = paste(
            pathToResults,
            "/tmp/datasets/importedData.csv",
            sep = ""
          ),
          object = v$data
        )
        Sys.sleep(0.5)
      }
      else {
        v$data$COHORT_DEFINITION_ID <- plyr::mapvalues(
          x = v$data$COHORT_DEFINITION_ID,
          from = c("0", as.character(input$cstateCohorts)),
          to = c("0", v$customisedStates),
          warn_missing = FALSE
        )
        
        for (i in 1:length(v$customisedStates)) {
          file.rename(
            paste(
              pathToResults,
              "/inst/JSON/",
              input$cstateCohorts[i],
              ".json",
              sep = ""
            ),
            paste(
              pathToResults,
              "/inst/JSON/",
              v$customisedStates[i],
              ".json",
              sep = ""
            )
          )
          file.rename(
            paste(
              pathToResults,
              "/inst/SQL/",
              input$cstateCohorts[i],
              ".sql",
              sep = ""
            ),
            paste(
              pathToResults,
              "/inst/SQL/",
              v$customisedStates[i],
              ".sql",
              sep = ""
            )
          )
        }
        
        save_object(
          path =  paste(
            pathToResults,
            "/tmp/datasets/importedData.csv",
            sep = ""
          ),
          object = v$data
        )
      }
    }
  })
  
  
  ##############################################################################
  #
  # Prioritization for states
  #
  ##############################################################################
  observeEvent(input$customiseButton | input$importDataButtonJSON, {
    output$inputStatePrioritization <- renderUI({
      sortable::rank_list(
        text = "Drag the states in the desired prioritization order (ascending)",
        labels =  if (is.null(v$customisedStates))
          unique(v$patientData$STATE)
        else
          v$customisedStates,
        input_id = "rankListPriority",
        
      )
    })
  })
  
  
  ################################################################################
  #
  # Output chosen cohorts and statistics
  #
  ################################################################################
  output$cohortsChosen <- renderText({
    if (input$importDataButton == 0) {
      return()
    }
    isolate(paste(
      "You have chosen target cohort",
      toString(input$ctargetCohort),
      " and state cohorts ",
      toString(input$cstateCohorts)
    ))
  })
  
  output$totalPatientsTarget <- shinydashboard::renderInfoBox({
    if (input$importDataButton == 0) {
      return(shinydashboard::infoBox(
        "Sample",
        "0",
        icon = shiny::icon("edit"),
        color = "yellow"
      ))
    }
    isolate(
      shinydashboard::infoBox(
        "Sample",
        toString(length(unique(
          dplyr::filter(v$data,
                        COHORT_DEFINITION_ID == "0")$SUBJECT_ID
        ))),
        icon = shiny::icon("edit"),
        color = "yellow"
      )
    )
  })
  
  
  output$totalPatients <- shinydashboard::renderInfoBox({
    if (input$importDataButton == 0) {
      return(
        shinydashboard::infoBox(
          "Unique",
          "0",
          icon = shiny::icon("address-card"),
          color = "light-blue"
        )
      )
    }
    isolate(
      shinydashboard::infoBox(
        "Unique",
        toString(length(unique(
          v$data$SUBJECT_ID
        ))),
        icon = shiny::icon("address-card"),
        color = "light-blue"
      )
    )
  })
  
  output$targetFilled <- shinydashboard::renderInfoBox({
    if (input$importDataButton == 0) {
      return(
        shinydashboard::infoBox(
          "Represented",
          "0 %",
          icon = shiny::icon("bullseye"),
          color = "purple"
        )
      )
    }
    isolate(
      shinydashboard::infoBox(
        "Represented",
        paste(toString(round(
          length(intersect(
            unique(
              dplyr::filter(v$data,
                            COHORT_DEFINITION_ID != "0")$SUBJECT_ID
            ),
            unique(
              dplyr::filter(v$data,
                            COHORT_DEFINITION_ID == "0")$SUBJECT_ID
            )
          )) / length(unique(
            dplyr::filter(v$data,
                          COHORT_DEFINITION_ID == "0")$SUBJECT_ID
          )) * 100, 2
        )), " %"),
        icon = shiny::icon("bullseye"),
        color = "purple"
      )
    )
  })
  
  
  ## Create inclusion table
  
  inclusionTable <- reactive({
    validate(need(!is.null(v$data), "Please select relevant cohorts!"))
    getInclusionTable(
      cohortData = v$data,
      selectedCohorts = if (is.null(v$customisedStates)) {
        c("0", as.character(input$cstateCohorts))
      }
      else {
        c("0", v$customisedStates)
      },
      cohortLabels = c(v$customisedTarget, v$customisedStates)
    )
  })
  
  output$cohortbyinclusion <- shiny::renderTable({
    if (input$importDataButton == 0 &
        input$customiseButton == 0  &
        input$importDataButtonJSON == 0) {
      return()
    }
    isolate(inclusionTable())
  },
  rownames = T, digits = 2)
  
  ## Create chronological table
  
  chronologicalTransitions <- reactive({
    validate(need(!is.null(v$data), "Please select relevant cohorts!"))
    DT::datatable(
      getChronologicalMatrix(
        cohortData = v$data,
        stateCohorts = if (is.null(v$customisedStates)) {
          input$cstateCohorts
        }
        else {
          v$customisedStates
        }
      ) ,
      options = list(pageLength = 50)
    )
  })
  
  output$generatedTrajectoriesStatistics1 <- shiny::renderTable({
    if (is.null(v$patientData)) {
      return()
    }
    isolate(v$trajectoryTable1)
  })
  
  
  output$generatedTrajectoriesStatistics2 <- shiny::renderTable({
    if (is.null(v$patientData)) {
      return()
    }
    isolate(v$trajectoryTable2)
  })
  
  
  
  
  
  output$chronologicalTransitions <- DT::renderDT({
    if (input$importDataButton == 0 &
        input$customiseButton == 0  &
        input$importDataButtonJSON == 0) {
      return()
    }
    isolate(chronologicalTransitions())
  })#,
  #  rownames = T)
  
  ################################################################################
  #
  # Create heatmap for finding possible disruptive combinations (eg drugs that are
  #  mostly prescribed together)
  #
  ################################################################################
  stateOverlapHeatmap = reactive({
    validate(need(!is.null(v$data), "Please select relevant cohorts!"))
    visualiseStateOverlap(patientData = v$data,
                          stateLabels = if (is.null(v$customisedStates)) {
                            input$cstateCohorts
                          }
                          else {
                            v$customisedStates
                          })
  })
  
  output$stateOverlapHeatmap <- renderPlot({
    if (input$heatmapButton == 0) {
      return()
    }
    
    isolate(stateOverlapHeatmap())
    
  })
  
  ################################################################################
  #
  # Trajectories generation content
  #
  ################################################################################
  
  observeEvent(input$trajectoriesButton, {
    v$stateLength <- as.numeric(input$stateDuration)
    isolate(trajectoriesGeneration())
  })
  
  output$absorbingStateChoices <- renderUI({
    shiny::checkboxGroupInput("absorbingStates",
                              "Choose absorbing states:",
                              as.list(c("No absorbing state",
                                        if (is.null(v$customisedStates)) {
                                          input$cstateCohorts
                                        }
                                        else {
                                          v$customisedStates
                                        })),
                              selected = if (studyHasBeenSaved) {
                                savedAbsorbingStates
                              }
                              else {
                                c("No absorbing state")
                              })
  })
  
  
  output$mandatoryStateChoices <- renderUI({
    shiny::checkboxGroupInput("mandatoryStates",
                              "Choose mandatory states:",
                              as.list(c("No mandatory state",
                                        if (is.null(v$customisedStates)) {
                                          input$cstateCohorts
                                        }
                                        else {
                                          v$customisedStates
                                        })),
                              selected = if (studyHasBeenSaved) {
                                savedMandatoryStates
                              }
                              else {
                                c("No mandatory state")
                              })
  })
  
  output$allowedTransitsionChoices  <- renderUI({
    # siia tuleb mingi listilaadne asi teha, laplly ja lisada igale tagile ka oma nimi ühega probs ei saaa :((((()))))
    
    #   if (is.null(v$dtreeIds)) {
    #     v$dtreeIds <<- 1
    #   } else {
    #     v$dtreeIds <<- c(v$dtreeIds, max(v$dtreeIds) + 1)
    #   }
    # output$inputs <- renderUI({
    #   tagList(lapply(1:length(v$dtreeIds), function(i) {
    #     checkboxGroupInput(
    #       paste0("layerInput", v$dtreeIds[i]),
    #       sprintf("Layer #%d", v$dtreeIds[i]),
    #       choices = v$states
    #     ) # choices = v$customisedStates
    #   }))
    # })
    
    states <- as.list(if (is.null(v$customisedStates)) {
                          input$cstateCohorts
                        }
                        else {
                          v$customisedStates
                        })
    tagList(lapply(1:length(states), function(i) {
      checkboxGroupInput(
        paste0("allowedTransitions", states[i]),
        sprintf("Allowed transitsion states for %s", states[i]),
        choices = states,
        selected = states
      ) # choices = v$customisedStates
    }))
  })
   
   
 #   for (state in if (is.null(states)) {
 #     input$cstateCohorts
 #   }
 #   else {
 #     states
 #   }) {
 #     shiny::checkboxGroupInput("allowedTransitions",
 #                               paste("Allowed transitsion states for ", state),
 #                               states,
 #                               selected = c("S1","S2"))
 #   }
 # })
  
  
  
  trajectoriesGeneration <- reactive({
    validate(need(
      !is.null(v$data),
      "Please import relevant cohorts under 'Import' tab!"
    ))
    # Create a Progress object
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    
    progress$set(message = "Cleaning data", value = 0)
    
    
    cohortData <- cleanCohortData(
      cohortData = v$data,
      mandatoryStates = input$mandatoryStates,
      outOfCohortAllowed = as.logical(input$outOfCohortAllowed)
    )
    progress$set(message = "Generating trajectories", value = 1 /
                   3)
    result <- NULL
    ############################################################################
    #
    # Reassemble allowed transitions to a list of vectors
    #
    ############################################################################
    
    stateVector = if (is.null(v$customisedStates)) {
      input$cstateCohorts
    }
    else {
      v$customisedStates
    }
    
    v$allowedTransitions = list()
    
    allowedTransitions_ids <- lapply(1:length(stateVector), function(i) {
      paste("allowedTransitions", stateVector[i], sep = "")
    })
    names(allowedTransitions_ids) = stateVector
    for (state in stateVector) {
    #  print(allowedTransitions_ids[[state]])
      targets = sprintf(input[[allowedTransitions_ids[[state]]]])
      v$allowedTransitions[[state]] <- sprintf(input[[allowedTransitions_ids[[state]]]])
      if((length(targets) == 0)){
        # If there are no allowed targets, then the state is absorbing.
        input$absorbingStates = union(input$absorbingStates, state)
      }
      
    }
    # print(v$allowedTransitions)
    names(v$allowedTransitions) = stateVector
    # print(v$allowedTransitions)
    ############################################################################
    #
    # Generation
    #
    ############################################################################
    if (input$trajectoryType == 0) {
      # print(v$allowedTransitions)
      result <- getTrajectoriesDiscrete(
        connection = conn,
        cohortData = cohortData,
        stateDuration = v$stateLength,
        stateSelection = input$stateSelectionType,
        statePriorityVector = input$rankListPriority,
        absorbingStates = input$absorbingStates,
        studyName = studyName,
        pathToResults = pathToResults,
        allowedStatesList = v$allowedTransitions
      )
    }
    else if (input$trajectoryType == 1) {
      result <- getTrajectoriesContinuous(
        connection = conn,
        stateSelection = input$stateSelectionType,
        patientData =  cohortData,
        statePriorityVector = input$rankListPriority,
        absorbingStates = input$absorbingStates,
        studyName = studyName,
        pathToResults = pathToResults,
        allowedStatesList = v$allowedTransitions
      )
    }
    ############################################################################
    #
    # Saving study settings as new row
    #
    ############################################################################
    progress$set(message = "Saving settings", value = 2 / 3)
    
    savedTrajectoryType <- if (input$trajectoryType == 0) {
      "Discrete"
    }
    else {
      "Continuous"
    }
    savedTrajectoryStates <- input$rankListPriority
    savedPriorityOrder <- input$rankListPriority
    savedStateSelectionType <- input$stateSelectionType
    savedAbsorbingStates <- input$absorbingStates
    savedMandatoryStates <- input$mandatoryStates
    savedLengthOfStay <- v$stateLength
    savedOutOfCohortAllowed <-  as.logical(input$outOfCohortAllowed)
    # defining a row
    newSettings <- data.frame(
      studyName,
      savedTrajectoryType,
      paste(savedTrajectoryStates, collapse = ","),
      paste(savedPriorityOrder, collapse = ","),
      as.integer(savedStateSelectionType),
      paste(savedAbsorbingStates, collapse = ","),
      paste(savedMandatoryStates, collapse = ","),
      savedLengthOfStay,
      savedOutOfCohortAllowed
    )
    if (studyName %in% settings$studyName) {
      settings[studyIndex,] <- newSettings
    }
    else {
      colnames(newSettings) <- colnames(settings)
      settings <- rbind(settings, newSettings)
    }
    
    write.csv(
      settings,
      paste(
        pathToResults,
        "/inst/Settings/trajectorySettings.csv",
        sep = ""
      ),
      row.names = FALSE,
      col.names = TRUE
    )
    print(paste(
      "Saved settings to: ",
      paste(
        pathToResults,
        "/inst/Settings/trajectorySettings.csv",
        sep = ""
      ),
      sep = ""
    ))
    
    v$patientData <<- result
    
    
    
    progress$set(message = "Creating statistics tables", value = 4 / 5)
    
    tmpDataState <-
      dplyr::filter(result,!STATE %in% c("START", "EXIT"))
    
    table <- prop.table(table(tmpDataState$STATE))
    table <- as.data.frame(table)
    colnames(table) <- c("STATE", "PERCENTAGE")
    table$PERCENTAGE <- round(table$PERCENTAGE * 100, 3)
    v$trajectoryTable1 <- table
    
    tmpDataState <-
      dplyr::arrange(tmpDataState, SUBJECT_ID, STATE_START_DATE)
    tmpDataState <-
      dplyr::slice(dplyr::group_by(tmpDataState, SUBJECT_ID), 1)
    
    table <- prop.table(table(tmpDataState$STATE))
    table <- as.data.frame(table)
    colnames(table) <- c("STATE", "PERCENTAGE")
    table$PERCENTAGE <- round(table$PERCENTAGE * 100, 3)
    v$trajectoryTable2 <- table
    
    progress$set(message = "Done", value = 1)
    
  })
  
  ################################################################################
  #
  # Profile tab content
  #
  ################################################################################
  
  output$profiles_personId <-
    renderText({
      input$profiles_personIdInput
    })
  
  observeEvent(input$profileSearchButton, {
    v$dataPerson <-
      getProfileData(
        conn,
        dbms = connectionDetails$dbms,
        cdmSchema = cdmSchema,
        personId = input$profiles_personIdInput
      )
  })
  
  observeEvent(input$profileSearchButton, {
    validate(need(
      !is.null(v$patientData),
      "Please create trajectories under the 'Trajectories' tab!"
    ))
    # Check if selected patient's id exists, if it does generate the plot, if it does not, do not generate the plot
    if (idExists(v$patientData, input$profiles_personIdInput)) {
      v$profileStochasticPlot <-
        visualisePatient(v$patientData,
                         as.numeric(input$profiles_personIdInput),
        )
    }
  })
  
  output$patientPlot <- shiny::renderPlot({
    # Check if the plot has been generated, if it has been --> show
    if (is.null(v$profileStochasticPlot)) {
      return()
    }
    v$profileStochasticPlot
  })
  
  output$patientSex <- shinydashboard::renderInfoBox({
    if (input$profileSearchButton == 0) {
      return(shinydashboard::infoBox(
        "Sex",
        "Unknown",
        icon = shiny::icon("venus-mars"),
        color = "red"
      ))
    }
    isolate(
      shinydashboard::infoBox(
        "Sex",
        v$dataPerson$genderString[1],
        icon = shiny::icon("venus-mars"),
        color = "red"
      )
    )
  })
  
  output$patientAge <- shinydashboard::renderInfoBox({
    if (input$profileSearchButton == 0) {
      return(
        shinydashboard::infoBox(
          "Birthdate",
          "??-??-????",
          icon = shiny::icon("calendar-alt"),
          color = "yellow"
        )
      )
    }
    isolate(
      shinydashboard::infoBox(
        "Birthdate",
        v$dataPerson$birthtimeString[1],
        icon = shiny::icon("calendar-alt"),
        color = "yellow"
      )
    )
  })
  
  
  output$patientExists <- shinydashboard::renderInfoBox({
    validate(need(
      !is.null(v$patientData),
      "Please create trajectories under 'Trajectories' tab!"
    ))
    if (input$profileSearchButton == 0) {
      return(shinydashboard::infoBox(
        "In cohort",
        "?",
        icon = shiny::icon("registered"),
        color = "green"
      ))
    }
    isolate(
      shinydashboard::infoBox(
        "In cohort",
        ifelse(idExists(
          v$patientData,  as.numeric(input$profiles_personIdInput)
        ), "Yes", "No"),
        icon = shiny::icon("registered"),
        color = "green"
      )
    )
  })
  
  observeEvent(input$refreshPlotButton, {
    validate(need(
      !is.null(v$patientData),
      "Please create trajectories under the 'Trajectories' tab!"
    ))
    # Check if selected patient's id exists, if it does generate the plot, if it does not, do not generate the plot
    if (idExists(v$patientData,  as.numeric(input$profiles_personIdInput))) {
      v$profileStochasticPlot <-
        visualisePatient(
          v$patientData,
          as.numeric(input$profiles_personIdInput),
          trajectoryStopDays = as.numeric(input$markovStateDurationPlot),
          theme = input$markovStatePlotType,
          # cdmTmpSchema = cdmTmpSchema,
          # dbms = dbms
          # feature = input$markovStatePlotFeatures
        )
    }
  })
  
  
}
