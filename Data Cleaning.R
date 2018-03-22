#Make Basic Filters for eliminating bad detections

#1. If a single movement is more than a specified rkm, then remove the detections of the receiver that has currently arrived.

clean_bad_receivers <- function(df_all_detections, df_tag_list, taglist_colname, upmove_rkm = 50, low_rkm = 0, high_rkm = 540) {
  all_clean_detections <- NULL
  det <- df_all_detections
  det <- det[is.na(det$DetectDate) == FALSE, ]
  det <- det[is.na(det$Detect_rkm) == FALSE, ]
  det$Detect_rkm <- as.numeric(det$Detect_rkm)
  det <- subset(det, Detect_rkm > low_rkm & Detect_rkm < high_rkm) #use det from here on out
  det$Detect_rkm <- as.numeric(det$Detect_rkm)
  all_tags <- unique(df_tag_list[[taglist_colname]])
  for (tag in all_tags) {
    count <- NULL
    print(tag)
    ind_tag <- subset(det, TagID == tag)
    ind_tag <- ind_tag[order(ind_tag$DetectDate), ]
    all_rows <- c(1:nrow(ind_tag))
    restriction <- 0
    #print(length(all_rows))
    if (nrow(ind_tag) > 0) {
      for (row in all_rows) { #This will identify where the detections in that receiver start
        if (row == nrow(ind_tag)) { #end the current iteration of the for-loop because nothing fits the other criteria.
          print("There are no errant detections in the middle of this detection history above the specified threshold.")
          all_clean_detections <- rbind(all_clean_detections, ind_tag)
          break
        }
        if (restriction == 1) {
          print('Multiple Large Movements in this Tag, Run Script Again')
          break
        }
        first_rkm <- ind_tag$Detect_rkm[row]
        second_rkm <- ind_tag$Detect_rkm[row + 1]
        diff_rkm <- abs(second_rkm - first_rkm)
        if (first_rkm != second_rkm & diff_rkm > upmove_rkm) {
          restriction <- 1
          print("Triggering Condition")
          print(row)
          #If there is a difference greater than the specified threshold for a single movement, the following needs to occur
          #1. Identify the detection before the change.
          #2. Determine how many (which rows) detections are on the errant receiver before it changes.
          #3. Remove these detections
          #4. Continue at location where original row. is it possible to reset where the row is?
          start_bad_det <- row + 1 #this is the beginning of the bad detections from an errant receiver.
          print(start_bad_det)
          #print(ind_tag[start_bad_det, ])
          new_rows <- c(start_bad_det:nrow(ind_tag)) #Gets detections from the start of the bad receiver to the end of that TagIDs detections
          #print(head(ind_tag))
          for (rows in new_rows) {
            #print(rows)
            bad_rkm <- ind_tag$Detect_rkm[rows]
            next_rkm <- ind_tag$Detect_rkm[rows + 1]
            if (rows == nrow(ind_tag)) {
              #print('End Second Iteration')
              break
            }
            if (bad_rkm != next_rkm) { #we have reached the end of the bad receiver/detection
              #remove detections from start_bad_det to bad_rkm
              #print('End Bad Receiver')
              #print(start_bad_det)
              #print(bad_rkm)
              new_ind_tag <- ind_tag[-c(start_bad_det:rows), ] #ind_tag is now cleaned.
              all_clean_detections <- rbind(all_clean_detections, new_ind_tag)
              break
            } #end of second row iteration.
          }#Iterate through rows after meeting below condition
        } #If statement if diff_rkm and > threshold
      } #Iterate by row in an individual tag
    } #If nrow > 0 statement
  } #Initial For Loop (iterate by TagID)
  return(all_clean_detections)
}

ind_striped <- subset(striped, TagID == 820)

new_striped <- clean_bad_receivers(df_all_detections = ind_striped, 
                                   df_tag_list = ind_striped,
                                   taglist_colname = 'TagID', upmove_rkm = 100)

#SCRIPT WORKS FOR SINGLE TAG
ind_striped$Detect_rkm <- as.numeric(ind_striped$Detect_rkm)
test_det <- subset(ind_striped, Detect_rkm > 0 & Detect_rkm < 540)

