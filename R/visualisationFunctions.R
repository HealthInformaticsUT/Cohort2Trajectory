################################################################################
#
# Some functions for visualizing the outputs of the study
#
################################################################################



#' Visualize patient's trajectory pathway
#'
#' This function outputs a patient oriented plot
#'
#' @param patientData Object of class data.frame with columns SUBJECT_ID, STATE, STATE_START_DATE, STATE_END_DATE
#' @param patientId Patient's id
#' @param trajectoryStopDays Number of days which will determine the possible split between trajectories
#' @param theme Plot's theme indicator
#' @keywords internal
visualisePatient = function(patientData,
                            patientId,
                            trajectoryStopDays = 183,
                            theme = 1
                            # connection,
                            # dbms,
                            # cdmTmpSchema = "ohdsi_temp"
                            ) {
  newPatientData = dplyr::select(patientData,
                                 SUBJECT_ID,
                                 STATE,
                                 STATE_START_DATE,
                                 STATE_END_DATE)
  newPatientData =  dplyr::filter(newPatientData, SUBJECT_ID == patientId)
  newPatientData =  dplyr::arrange(newPatientData, STATE_START_DATE, STATE_END_DATE)
  momentStart = as.numeric(
    as.Date(newPatientData$STATE_START_DATE) - as.Date(newPatientData$STATE_START_DATE[1])
  )
  momentEnd = as.numeric(
    as.Date(newPatientData$STATE_END_DATE) - as.Date(newPatientData$STATE_START_DATE[1])
  )
  momentDuration = as.numeric(
    as.Date(newPatientData$STATE_END_DATE) - as.Date(newPatientData$STATE_START_DATE)
  )
  # Calculate the trajectories between for the given patient
  n = length(momentStart)
  momentInclusion = rep(TRUE, n)
  if (theme == 1) {
    # Will create an error if only one data row present
    if (n == 1) {
      momentDifBoolean = c(TRUE)
    }
    else {
      momentDif = momentEnd[-length(momentEnd)] - momentStart[-1]
      # Will return TRUE if momentDif element is smaller than trajectoryStopDays
      momentDifBoolean = -momentDif > trajectoryStopDays
    }
    groups = rep(1, n)
    
    for (index in 1:length(momentDifBoolean)) {
      if (!momentDifBoolean[index])
        groups[index + 1] =  groups[index]
      else {
        groups[index + 1] =  groups[index] + 1
        # Let's crop the trajectories' durations so that every trajectory start's from 0 not some x value
        momentStart[c(index + 1:n)] = momentStart[c(index + 1:n)] - momentStart[index + 1]
        momentEnd[c(index + 1:n)] = momentStart[c(index + 1:n)] + momentDuration[c(index +
                                                                                     1:n)]
        
        
        
      }
      
    }
    
    
  }
  else if (theme == 2) {
    groups = rep(1, n)
    # variable for iterating through groups of trajectories
    group = 1
    # Special case: only one data row
    if (n == 1) {
      momentInclusion = c(TRUE)
    }
    else {
      countOther = 0
      for (index in  1:n) {
        if (as.character(newPatientData$STATE[index]) == "0" |
            as.character(newPatientData$STATE[index]) == "OUT OF COHORTS")
        {
          countOther = countOther + momentDuration[index]
          if (countOther > trajectoryStopDays) {
            momentInclusion[index] = FALSE
            if (!(as.character(newPatientData$STATE[index + 1]) %in% c("0", "OUT OF COHORTS"))) {
              group = group + 1
              
              # Let's crop the trajectories' durations so that every trajectory start's from 0 not some x value
              momentStart[c(index + 1:n)] = momentStart[c(index + 1:n)] - momentStart[index + 1]
              momentEnd[c(index + 1:n)] = momentStart[c(index + 1:n)] + momentDuration[c(index +
                                                                                           1:n)]
              
            }
          }
        }
        else{
          countOther = 0
        }
        groups[index] = group
      }
    }
  }
  
  
  # Some Na's may occur
  momentStart = as.vector(na.exclude(momentStart))
  momentEnd = as.vector(na.exclude(momentEnd))
  
  # Color for visualisation
  n_states = length(unique(newPatientData$STATE))
  colors = NULL
  # Due to RColorBrewer limitations we repeat if > 12 classes
  if (n_states > 12) {
    n = 12
    colors_vec = colors = RColorBrewer::brewer.pal(n = n, name = 'Paired')
    while (n < n_states) {
      colors_vec = c(colors_vec, colors_vec[n %% 12])
      n = n + 1
    }
    colors = colors_vec
  }
  else {
    colors = RColorBrewer::brewer.pal(n = n_states, name = 'Paired')
  }
  colorTable = cbind(unique(as.vector(newPatientData$STATE)), colors)
  colnames(colorTable) = c("STATE", "COLOR")
  
  ##############################################################################
  #
  # Let's add information from cost table under the given date interval
  #
  ##############################################################################
  
  newPatientData = cbind(newPatientData,
                         momentStart,
                         momentEnd,
                         groups,
                         momentInclusion)
  colnames(newPatientData) = c(
    "SUBJECT_ID",
    "STATE",
    "STATE_START_DATE",
    "STATE_END_DATE",
    "MOMENT_START",
    "MOMENT_END",
    "GROUP",
    "INCLUDE"
    # "COST"
  )
  newPatientData = dplyr::filter(newPatientData, INCLUDE == TRUE)
  newPatientData = merge(x = newPatientData,
                         y = colorTable,
                         by = "STATE",
                         all.x = TRUE)
  newPatientData =  dplyr::arrange(newPatientData, STATE_START_DATE, STATE_END_DATE)
  newPatientData =  dplyr::mutate(newPatientData,
                                  MOMENT_START = ifelse(MOMENT_START == 0 &
                                                          STATE != 'START', 1,
                                                        MOMENT_START))
  blank_data = data.frame(GROUP = sort(unique(groups)),
                          y = 1,
                          x = as.vector(
                            tapply(newPatientData$MOMENT_END, newPatientData$GROUP, max)
                          ))
  
  
  p = ggplot2::ggplot(newPatientData) +
    ggplot2::geom_rect(
      xmin = newPatientData$MOMENT_START,
      xmax = newPatientData$MOMENT_END,
      ymin = rep(-Inf, nrow(newPatientData)),
      ymax = rep(Inf, nrow(newPatientData)),
      fill = newPatientData$COLOR,
      alpha = (0.3)
    ) +  ggplot2::geom_blank(data = blank_data, ggplot2::aes(x = x, y = y)) + ggplot2::facet_wrap( ~ GROUP, scales = "free_x")
  # "h" a variable for text height
  h = rep(1, nrow(newPatientData))
  
  p = p + ggplot2::geom_text(ggplot2::aes(
    x = MOMENT_START + (MOMENT_END - MOMENT_START) / 2,
    y = h,
    label = STATE,
    angle  = 90
  ),
  size = 4) +  # ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::xlab("Days") + ggplot2::ylab("Measurements") + ggplot2::expand_limits(x = 0) +  ggplot2::theme_bw() + ggplot2::xlim(c(0, max(newPatientData$MOMENT_END)))
  return(p)
}


#' Visualise overlaping cohorts to detect possible state's which would be advisible to join
#'
#' This function outputs a pheatmap
#'
#' @param patientData Object of class data.frame with columns SUBJECT_ID, STATE, STATE_START_DATE, STATE_END_DATE
#' @param stateLabels Labels for state notation
#' @keywords internal
visualiseStateOverlap = function(patientData, stateLabels) {
  patientDataCopy = data.frame(patientData[, 1:4])
  colnames(patientDataCopy) = c("SUBJECT_ID", "STATE", "STATE_START_DATE", "STATE_END_DATE")
  patientDataCopy = dplyr::filter(patientDataCopy, STATE != '0')
  
  matrix = matrix(0,
                  ncol = length(stateLabels),
                  nrow = length(stateLabels))
  colnames(matrix) = stateLabels
  rownames(matrix) = stateLabels
  patientDataCopy = dplyr::arrange(patientDataCopy,
                                   SUBJECT_ID,
                                   STATE_START_DATE,
                                   STATE_END_DATE)
  for (patientId in unique(patientDataCopy$SUBJECT_ID)) {
    patientObservedData = patientDataCopy[patientDataCopy$SUBJECT_ID == patientId,]
    patientObservedData = dplyr::arrange(patientObservedData, STATE_START_DATE)
    size_patientDf = nrow(patientObservedData)
    for (row in 1:(size_patientDf - 1)) {
      if (size_patientDf == 1)
        next
      intervalOverlaps = mapply(
        daysOverlap,
        dateStart = as.Date(patientObservedData[row,]$STATE_START_DATE),
        dateEnd =  as.Date(patientObservedData[row,]$STATE_END_DATE),
        controlStart = patientObservedData$STATE_START_DATE[(row + 1):size_patientDf],
        controlEnd = patientObservedData$STATE_END_DATE[(row + 1):size_patientDf]
      )
      if (sum(intervalOverlaps) > 0) {
        matrix[as.character(patientObservedData$STATE[row]), as.character(patientObservedData$STATE[(row +
                                                                                                       1):size_patientDf])] =  matrix[as.character(patientObservedData$STATE[row]), as.character(patientObservedData$STATE[(row +
                                                                                                                                                                                                                              1):size_patientDf])] + as.numeric(intervalOverlaps > 0)
      }
    }
  }
  
  matrix = prop.table(matrix, 1)
  
  
  ##############################################################################
  #
  # Let's make ta matrix symmetrical, we are searching for overlaps not insight
  # on which state dominates the other
  #
  ##############################################################################
  
  for (i in 1:nrow(matrix)) {
    for (j in 1:ncol(matrix)) {
      if (i == j) {
        next
      }
      saved_ij = matrix[i, j]
      matrix[i, j] = (matrix[i, j] + matrix[j, i]) / 2
      matrix[j, i] = (saved_ij + matrix[j, i]) / 2
    }
  }
  
  plot = pheatmap::pheatmap(matrix, cluster_rows = F, cluster_cols = F)
  return(plot)
}
