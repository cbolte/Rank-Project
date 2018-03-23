#After cleaning up detections run the following code.

#REMOVES THE LAST DETECTIONS ASSOCIATED WITH THE RIVER KILOMETER OF THE LAST DETECTION

clip_and_replace <- function(df_all_detections, v_tags) { 
  target_tags <- v_tags
  df_cleaned_tags <- NULL
  print("Trimming Last Detection Off of Tags with Errant Final Detections")
  for (tagid in target_tags) { # Step 1
    print(tagid)
    ind_tag <- subset(df_all_detections, TagID == tagid)
    ind_tag <- ind_tag[order(ind_tag$DetectDate, decreasing=TRUE), ] #reverses order of detection history.
    all_rows <- c(1:nrow(ind_tag))
    count <- 1
    for (row in all_rows) {
      first_receiver <- ind_tag$Detect_rkm[row]
      second_receiver <- ind_tag$Detect_rkm[row + 1]
      if (first_receiver == second_receiver) {
        count <- count + 1
      }
      else { #first receiver != second receiver
        break
      }
    }
    ind_tag <- ind_tag[order(ind_tag$DetectDate), ] #returns order back to normal
    ind_tag <- head(ind_tag, -(count))
    
    df_cleaned_tags = rbind(df_cleaned_tags, ind_tag) #Step 2
  }
  #return(df_cleaned_tags)
  
  print("Removing Target Tags from Original All_Detection Data.Frame")
  #Remove tags from original data.frame
  df_det_clean <- df_all_detections
  for (tagid in target_tags) {
    df_det_clean <- subset(df_det_clean, TagID != tagid)
    print(tagid)
  }
  #return(df_det_clean)
  
  print("Adding Trimmed Detections Back to All_Detection Data.Frame")
  df_det_clean = rbind(df_det_clean, df_cleaned_tags)
  return(df_det_clean)
}





