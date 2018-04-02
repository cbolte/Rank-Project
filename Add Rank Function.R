year_vector <- c(2008)
df_year_vector <- data.frame(year_vector)

add_rank_individuals <- function(df, year_vector) {
  print('Adding Ranks')
  df_ranks <- NULL
  if (is.null(df) == FALSE) {
    det <- df
    det$study_year_month <- as.character(det$study_year_month)
    all_tags <- c(unique(det$tag))
    for (tagid in all_tags) {
      print(tagid)
      #print(class(tagid))
      ind_tag <- subset(det, tag == tagid)
      #print(ind_tag)
      all_years <- c(unique(df_year_vector$year_vector))
      print(ind_tag)
      print(class(ind_tag$study_year_month[1]))
      for (year in all_years) {
        print(year)
        grep_year <- as.character(year)
        print(grep_year)
        ind_tag <- ind_tag[grepl(grep_year, ind_tag$study_year_month), ]
        print(ind_tag)
        if (nrow(ind_tag) > 0) {
          ind_tag <- ind_tag[order(ind_tag$total_rkm, decreasing=TRUE), ]
          ind_tag$index <- c(1:nrow(ind_tag))
          df_ranks <- rbind(df_ranks, ind_tag)
        }
      }
      #print(df_ranks)
  }
  return(df_ranks)
  }
}

ttest_eight_year_motility <- add_rank_individuals(test_eight_year_motility, year_vector)

test_ind_rank <- add_rank_individuals(test_ind, test_year_vector)

test_ind <- subset(test_eight_year_motility, tag == 4324)
test_ind_year <- test_eight_year_motility[grepl(2008, test_eight_year_motility$study_year_month), ]

