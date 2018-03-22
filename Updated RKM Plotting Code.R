library(ggplot2)
library(dplyr)

plot_rkm_time_vemco <- function(df, pdf_name, int_rkm = -100, str_start_datetime = "2000-01-01 00:00:00", str_end_datetime = "2050-01-01 00:00:00") {
  pdf(paste(pdf_name, ".pdf", sep = "" )) 
  df_edit <- subset(df, Detect_rkm > int_rkm)
  df_edit <- subset(df_edit, DetectDate > str_start_datetime & DetectDate < str_end_datetime)
  all_tags <- unique(df$TagID)
  for (tag in all_tags) {
    df_id <-subset(df_edit, df_edit$TagID == tag)
    if (nrow(df_id) > 0) {
      df_id <- df_id[order(df_id$DetectDate), ]
      first_det <- min(df_id$DetectDate)
      last_det <- max(df_id$DetectDate)
      fish_move <- ggplot(df_id, aes(x=DetectDate, y=Detect_rkm, group = TagID)) +
        geom_point() + geom_line() +
        ggtitle(paste(tag, "from", first_det, "to", last_det)) +
        xlab("Time After Release") +
        ylab("River Kilometer")
      plot(fish_move)
    }
  }
  dev.off()
}

plot_rkm_time_vemco(big_striped, 'All Bass Movements over 100 rkm')



