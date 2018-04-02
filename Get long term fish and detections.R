
get_longterm_sturgeon <- function(df, taglist, n_years) {
  all_fish <- c(unique(taglist$TagID))
  df_longterm_fish <- NULL
  for (fish in all_fish) {
    print(fish)
    ind_fish <- subset(df, TagID == fish)
    first_det <- min(ind_fish$DetectDate)
    last_det <- max(ind_fish$DetectDate)
    total_time <- as.numeric(difftime(last_det, first_det, units = 'days'))
    total_time <- signif(total_time/365, digits=3) #convert total_time to years
    print(total_time)
    if (total_time >= n_years) {
      print('Condition Triggered')
      df_longterm_fish <- rbind(df_longterm_fish, data.frame(fish, total_time, first_det, last_det))
    }
  }
  df_longterm_fish <- df_longterm_fish[order(df_longterm_fish$total_time), ]
  return(df_longterm_fish)
}

longterm_fish <- get_longterm_sturgeon(green, green)

get_specific_tags <- function(df, taglist) {
  det <- df
  all_tags <- unique(taglist$fish)
  df_new <- NULL
  for (tag in all_tags) {
    print(tag)
    ind_tag <- subset(det, TagID == tag)
    df_new <- rbind(df_new, ind_tag)
  }
  return(df_new)
}

df_longterm <- get_specific_tags(green, longterm_fish)








get_all_motilities_months <- function(df, species_name, year_list) {
  #Year list should have 3 entries, 'year', 'start', 'end'. This should be made before the loop
  df_all_motilities <- NULL
  all_years <- 1:nrow(year_list)
  for (y in all_years) { #y should be a number index
    start_datetime <- as.character(year_list$start_date[y])
    end_datetime <- as.character(year_list$end_date[y])
    study_year <- as.character(year_list$year[y])
    print(start_datetime)
    print(end_datetime)
    print(study_year)
    motility <-  df %>% 
      get_motility(species_name = species_name, str_startdate = start_datetime, str_enddate = end_datetime) %>%
      add_rank
    df_all_motilities <- rbind(df_all_motilities, motility)
  }
  return(df_all_motilities)
}

all_motilities <- get_all_motilities(df_longterm, 'Green_Sturgeon', df_years)
