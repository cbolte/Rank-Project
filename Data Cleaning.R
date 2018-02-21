#Make Basic Filters for eliminating bad detections

#1. If a single movement is more than 100 rkm, then remove the detections of the receiver that has currently arrived.

clip_and_replace_middle <- function(df_all_detections, df_tag_list, taglist_colname, upmove_rkm = 50, low_rkm = 0, high_rkm = 540) {
  all_clean_detections <- NULL
  det <- subset(df_all_detections, Detect_rkm > low_rkm & Detect_rkm < high_rkm) #use det from here on out
  det <- det[is.na(det$DetectDate) == FALSE, ]
  det <- det[is.na(det$Detect_rkm) == FALSE, ]
  det$Detect_rkm <- as.numeric(det$Detect_rkm)
  all_tags <- unique(df_tag_list[[taglist_colname]])
  for (tag in all_tags) {
    count <- NULL
    print(tag)
    ind_tag <- subset(det, TagID == tag)
    ind_tag <- ind_tag[order(ind_tag$DetectDate), ]
    all_rows <- c(1:nrow(ind_tag))
    if (nrow(ind_tag) > 0) {
      for (row in all_rows) { #This will identify where the detections in that receiver start
        if (row == nrow(ind_tag)) {
          print("There are no errant detections in the middle of this detection history above the specified threshold.")
          all_clean_detections <- rbind(all_clean_detections, ind_tag)
          break
        }
        first_rkm <- ind_tag$Detect_rkm[row]
        second_rkm <- ind_tag$Detect_rkm[row + 1]
        diff_rkm <- abs(second_rkm - first_rkm)
        if (first_rkm != second_rkm & abs(diff_rkm) > upmove_rkm) {
          #If there is a difference greater than the specified threshold for a single movement, the following needs to occur
          #1. Identify the detection before the change.
          #2. Determine how many (which rows) detections are on the errant receiver before it changes.
          #3. Remove these detections
          #4. Continue at location where original row. is it possible to reset where the row is?
          start_bad_det <- row + 1
          new_rows <- c(start_bad_det:nrow(ind_tag))
          for (rows in new_rows) {
            bad_rkm <- ind_tag[rows]
            next_rkm <- ind_tag[rows + 1]
            if (bad_rkm != next_rkm) { #we have reached the end of the bad receiver/detection
              #remove detections from start_bad_det to bad_rkm
              ind_tag <- ind_tag[-(start_bad_det:bad_rkm), ] #ind_tag is now cleaned.
              all_clean_detections <- rbind(all_clean_detections, ind_tag)
              break
            }
          }
        }
      }
    }
  }
  return(all_clean_detections)
}


new_smb <- clip_and_replace_middle(smb, smb, 'TagID', 100)

#For next time, identify fish that will set off each criteria, then make sure that they can be added to eachother using the script.

