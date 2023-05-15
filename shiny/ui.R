################################################################################
#
# Header content
#
################################################################################
header <-
  shinydashboard::dashboardHeader(title = "Cohort2Trajectory dashboard",
                                  tags$li(
                                    div(
                                      img(
                                        src = 'images/logo.png',
                                        title = "OHDSI PLP",
                                        height = "40px",
                                        width = "40px"
                                      ),
                                      style = "padding-top:0px; padding-bottom:0px;"
                                    ),
                                    class = "dropdown"
                                  ))


################################################################################
#
# Sidebar content
#
################################################################################
sidebar <- shinydashboard::dashboardSidebar(
  shinydashboard::sidebarMenu(
    shinydashboard::menuItem("Description", tabName = "description", icon = icon("home")),
    shinydashboard::menuItem(
      "Import via Atlas",
      tabName = "import",
      icon = icon("file-import")
    ),
    shinydashboard::menuItem(
      "Import via JSON",
      tabName = "import-json",
      icon = icon("file-import")
    ),
    shinydashboard::menuItem(
      "Statistics",
      tabName = "statisticsTab",
      icon = icon("chart-area")
    ),
    shinydashboard::menuItem(
      "Prioritization",
      tabName = "prioritization",
      icon = icon("mask")
    ),
    shinydashboard::menuItem(
      "Trajectories",
      tabName = "trajectories",
      icon = icon("upload")
    ),
    shinydashboard::menuItem("Profiles", tabName = "profiles", icon = icon("user")),
    shinydashboard::menuItem("Help", tabName = "help", icon = icon("info"))
  )
)

################################################################################
#
# Body content
#
################################################################################

body <- shinydashboard::dashboardBody(
  shinydashboard::tabItems(
    ################################################################################
    #
    # Description content
    #
    ################################################################################
    shinydashboard::tabItem(
      tabName = "description",
      shiny::includeMarkdown(path = "./www/shinyDescription.md")
      
    ),
    ################################################################################
    #
    # Import content from Atlas
    #
    ################################################################################
    shinydashboard::tabItem(
      tabName = "import",
      # # Choosing the target cohort for patient inclusion
      shinydashboard::box(
        title = "Choose the patient cohort:",
        status = "primary",
        solidHeader = TRUE,
        uiOutput("targetCohort")
      ),
      
      shinydashboard::box(
        title = "Choose the state cohorts:",
        status = "primary",
        solidHeader = TRUE,
        uiOutput("stateCohorts")
      ),
      
      shiny::actionButton("importDataButton", "Import data"),
      
      shinydashboard::tabBox(
        title = "Cohorts' overview",
        id = "tabsetOver",
        width = "100%",
        shiny::tabPanel(title = "Selected cohorts",
                        shiny::fluidRow(
                          shiny::column(
                            width = 12,
                            shinydashboard::box(
                              width = 12,
                              title = "Information",
                              status = "primary",
                              solidHeader = T,
                              shinycssloaders::withSpinner(textOutput("cohortsChosen")),
                              shinydashboard::infoBoxOutput("totalPatientsTarget"),
                              shinydashboard::infoBoxOutput("totalPatients"),
                              shinydashboard::infoBoxOutput("targetFilled")
                            ),
                          )
                          
                        )),
        shiny::tabPanel(title = "Customise",
                        shiny::fluidRow(
                          shiny::column(
                            width = 12,
                            shinydashboard::box(
                              width = 12,
                              title = "",
                              status = "primary",
                              solidHeader = T,
                              shiny::textInput(
                                inputId = "customisedTarget",
                                label = tags$div(
                                  HTML(
                                    '<i class="	fas fa-check-circle" style = "color:#00000;"></i><span style = "color:#00000"> Name the selected patient cohort </span>'
                                  )
                                ),
                                placeholder = "Target cohort"
                              ),
                              shiny::textInput(
                                inputId = "customisedStates",
                                label = tags$div(
                                  HTML(
                                    '<i class="	fas fa-chart-pie" style = "color:#00000;"></i><span style = "color:#00000"> Name the selected state cohorts </span>'
                                  )
                                ),
                                placeholder = "State 1, State 2, ..."
                              ),
                              actionButton("customiseButton", "Apply")
                            )
                          )
                        ))
      )
    ),
    
    ################################################################################
    #
    # Import content from JSON
    #
    ################################################################################
    shinydashboard::tabItem(
      tabName = "import-json",
      # # Choosing the target cohort for patient inclusion
      shinydashboard::box(
        solidHeader = TRUE,
        width = 12,
        title = "Patient cohort JSON:",
        status = "primary",
        shiny::textAreaInput(
          inputId = 'targetJSON',
          label = "Target cohort JSON",
          value = "",
          placeholder = "Target cohort JSON...",
          height = '300px'
        ),
        shiny::actionButton("importInstButtonJSON", "Show saved JSONs"),
        shiny::actionButton("addButtonJSON", "Add state"),
        shiny::actionButton("rmButtonJSON", "Remove state"),
        shiny::actionButton("importDataButtonJSON", "Import data")
      ),
      shiny::fluidRow(shiny::column(
        width = 12,
        shinydashboard::box(
          width = 12,
          title = "Information",
          status = "primary",
          solidHeader = T,
          shinydashboard::infoBoxOutput("totalPatientsTargetJSON"),
          shinydashboard::infoBoxOutput("totalPatientsJSON"),
          shinydashboard::infoBoxOutput("targetFilledJSON")
        ),
      )),
      shiny::uiOutput("statesJSON")
      
      
    ),
    ############################################################################
    #
    # Statistics content
    #
    ############################################################################
    shinydashboard::tabItem(
      tabName = "statisticsTab",
      shiny::tabPanel(title = "Selected cohorts",
                      shiny::fluidRow(
                        shiny::column(
                          width = 12,
                          shinydashboard::box(
                            width = 12,
                            title = "Inclusion",
                            status = "primary",
                            solidHeader = T,
                            shinycssloaders::withSpinner(shiny::tableOutput('cohortbyinclusion'))
                          ),
                          shinydashboard::box(
                            width = 12,
                            title = "Chronological transitions",
                            status = "primary",
                            solidHeader = T,
                            shinycssloaders::withSpinner(DT::dataTableOutput('chronologicalTransitions'))
                          ),
                          shiny::column(
                            width = 6,
                            shinydashboard::box(
                              title = "Incidence of trajectories' states",
                              width = NULL,
                              status = "primary",
                              solidHeader = TRUE,
                              shinycssloaders::withSpinner(shiny::tableOutput('generatedTrajectoriesStatistics1'))
                            )
                          ),
                          shiny::column(
                            width = 6,
                            shinydashboard::box(
                              title = "Incidence of states as trajectory head",
                              width = NULL,
                              status = "primary",
                              solidHeader = TRUE,
                              shinycssloaders::withSpinner(shiny::tableOutput('generatedTrajectoriesStatistics2'))
                            )
                          ),
                          
                        )
                      )),
      shiny::tabPanel(title = "State overlap heatmap",
                      shiny::fluidRow(
                        shiny::column(
                          width = 12,
                          shinydashboard::box(
                            width = 12,
                            title = "State overlap heatmap",
                            status = "primary",
                            solidHeader = T,
                            shinycssloaders::withSpinner(shiny::plotOutput("stateOverlapHeatmap")),
                            shiny::actionButton("heatmapButton", "Show heatmap!"),
                          )
                        )
                        
                      )),
      #   )
      #
    ),
    ############################################################################
    #
    # Prioritization content
    #
    ############################################################################
    
    shinydashboard::tabItem(tabName = "prioritization",
                            shiny::uiOutput("inputStatePrioritization")),
    
    ################################################################################
    #
    # Trajectories generation content
    #
    ################################################################################
    shinydashboard::tabItem(
      tabName = "trajectories",
      shiny::fluidPage(
        shinydashboard::box(
          width = 12,
          title = "Trajectory generation settings",
          status = "primary",
          solidHeader = T,
          shiny::fluidRow(
            shiny::column(
              width = 4,
              shiny::radioButtons(
                inputId = "trajectoryType",
                label = "Trajectory type:",
                choices = c("Discrete time" = 0,
                            "Continuous time" = 1),
                selected = if (studyHasBeenSaved) {
                  if (savedTrajectoryType == "Discrete") {
                    0
                  }
                  else {
                    1
                  }
                }
                else {
                  0
                }
              )
            ),
            shiny::column(
              width = 4,
              shiny::numericInput(
                inputId = "stateDuration",
                label = tags$div(
                  HTML(
                    '<i class="	fas fa-user-cog" style = "color:#00000;"></i><span style = "color:#00000"> Length of a state stay in days (only discrete case) </span>'
                  )
                ),
                value = if (studyHasBeenSaved) {
                  savedLengthOfStay
                }
                else {
                  30
                },
                min = 0
              )
            ),
            shiny::column(
              width = 4,
              shiny::radioButtons(
                inputId = "stateSelectionType",
                label = "State selection type",
                choices = c(
                  "First occurring" = 1,
                  "Largest overlap" = 2,
                  "Priority first" = 3
                ),
                selected = if (studyHasBeenSaved) {
                  savedStateSelectionType
                }
                else {
                  1
                }
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(width = 4,
                          uiOutput("absorbingStateChoices")),
            shiny::column(width = 4,
                          uiOutput("mandatoryStateChoices")),
            shiny::column(
              width = 4,
              shiny::radioButtons(
                inputId = "outOfCohortAllowed",
                label = "Allow states out of observation period",
                choices = c("No" = FALSE,
                            "Yes" = TRUE),
                selected = if (studyHasBeenSaved) {
                  as.logical(savedOutOfCohortAllowed)
                }
                else {
                  FALSE
                }
              ),
              shiny::actionButton("trajectoriesButton", "Generate")
            )
          ),
          shiny::fluidRow(
            shiny::column(width = 4,
                          uiOutput("fixOutOfCohort")),
            shiny::column(
              width = 4,
              shiny::radioButtons(
                inputId = "allowMerging",
                label = "Allow merging states",
                choices = c("No" = FALSE,
                            "Yes" = TRUE)
              )
            ),
            shiny::column(
              width = 4,
              shiny::numericInput(
                inputId = "mergingThreshold",
                label = tags$div(
                  HTML(
                    '<i class="	fas fa-user-cog" style = "color:#00000;"></i><span style = "color:#00000"> Threshold (0-1) </span>'
                  )
                ),
                value = 0.5,
                min = 0,
                max = 1
              )
            )
          )
        ),
        shinydashboard::box(
          width = 12,
          title = "Allowed transitions",
          status = "primary",
          solidHeader = T,
          uiOutput("allowedTransitsionChoices")
        )
      )
    ),
    
    
    ################################################################################
    #
    # Profiles content
    #
    ################################################################################
    shinydashboard::tabItem(
      tabName = "profiles",
      shiny::fluidRow(shiny::column(
        width = 11,
        shiny::textInput(
          inputId = "profiles_personIdInput",
          label = tags$div(
            HTML(
              '<i class="	fas fa-user-alt" style = "color:#00000;"></i><span style = "color:#00000"> Profiles </span>'
            )
          ),
          placeholder = "Person Id"
        )
      )),
      actionButton("profileSearchButton", "Search"),
      shinydashboard::tabBox(
        title = "Profile's overview",
        id = "tabsetProfile",
        width = "100%",
        shiny::tabPanel(title = "Information",
                        shiny::fluidRow(
                          shiny::column(
                            width = 12,
                            shinydashboard::box(
                              width = 12,
                              title = "Profile",
                              status = "primary",
                              solidHeader = T,
                              shinycssloaders::withSpinner(shinydashboard::infoBoxOutput("patientSex")),
                              shinycssloaders::withSpinner(shinydashboard::infoBoxOutput("patientAge")),
                              shinycssloaders::withSpinner(shinydashboard::infoBoxOutput("patientExists"))
                            ),
                          )
                          
                        )),
        shiny::tabPanel(
          title = "Trajectory",
          shiny::fluidRow(shinycssloaders::withSpinner(shiny::plotOutput("patientPlot"))),
          shiny::column(
            width = 8,
            shiny::radioButtons(
              "markovStatePlotType",
              label = h3("Plot type"),
              choices =
                c("Normal" = 1,
                  "Distinguishable" = 2),
              selected = 1
            ),
            shiny::numericInput(
              inputId = "markovStateDurationPlot",
              label = tags$div(
                HTML(
                  '<i class="	fas fa-user-cog" style = "color:#00000;"></i><span style = "color:#00000"> Length of allowed inactivity (days) </span>'
                )
              ),
              value = 30,
              min = 0
            )
          ),
          actionButton("refreshPlotButton", "Refresh")
        )
      )
      
    ),
    ################################################################################
    #
    # Help content
    #
    ################################################################################
    shinydashboard::tabItem(
      tabName = "help",
      shinydashboard::tabBox(
        title = "Help",
        id = "helpTabset",
        width = "100%",
        shiny::tabPanel(
          title = "Description",
          shiny::includeMarkdown(path = "./www/shinyHelpDescription.md")
        ),
        shiny::tabPanel(
          title = "Import",
          shiny::includeMarkdown(path = "./www/shinyHelpImport.md")
        ),
        shiny::tabPanel(
          title = "Trajectories",
          shiny::includeMarkdown(path = "./www/shinyHelpTrajectories.md")
        ),
        shiny::tabPanel(
          title = "Profiles",
          shiny::includeMarkdown(path = "./www/shinyHelpProfiles.md")
        )
      ),
    )
  )
)
################################################################################
#
# Shiny customization content
#
################################################################################

shinydashboard::dashboardPage(skin = "black", header, sidebar, body)
