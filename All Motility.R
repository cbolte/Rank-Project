#Use a for loop so that you can run all species for a given set of time

detections <- list(smb, lmb)
names <- c('smallmouth_bass', 'largemouth_bass')

length(detections)
length(names)


all_motility <- function(l_detections, v_names, a_str_startdate='2000-01-01 00:00:00', a_str_enddate='2100-01-01 00:00:00') {
  total <- c(1:length(names))
  df_all_motility <- NULL
  if (length(l_detections) != length(v_names)) {
    print('The length of the two vectors or lists are not equivalent')
    break
  }
  for (t in total) {
    df <- data.frame(l_detections[t])
    species_name <- v_names[t]
    df_moves <- get_motility(df, species = species_name, str_startdate = a_str_startdate, str_enddate = a_str_enddate)
    df_all_motility <- rbind(df_all_motility, df_moves)
  }
  return(df_all_motility)
}

test_output <- all_motility(detections, names)

print(data.frame(detections[1]))


