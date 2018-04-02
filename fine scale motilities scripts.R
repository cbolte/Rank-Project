
get_all_motilities_months <- function(df, species_name, year_list) {
  #Year list should have 3 entries, 'year', 'start', 'end'. This should be made before the loop
  df_all_motilities <- NULL
  all_years <- 1:nrow(year_list)
  for (y in all_years) { #y should be a number index
    start_datetime <- as.character(year_list$start_date[y])
    end_datetime <- as.character(year_list$end_date[y])
    year_month <- as.character(year_list$year_month[y])
    print(start_datetime)
    print(end_datetime)
    print(year_month)
    motility <-  df %>% 
      get_finescale_motility(species_name = species_name, str_startdate = start_datetime, str_enddate = end_datetime, study_year_month=year_month)
    df_all_motilities <- rbind(df_all_motilities, motility)
  }
  return(df_all_motilities)
}



get_finescale_motility <- function(df, low_rkm=0, high_rkm=550, species_name, str_startdate='2000-01-01 00:00:00', str_enddate='2100-01-01 00:00:00', study_year_month) {
  det <- df[df$Detect_rkm > low_rkm & df$Detect_rkm < high_rkm, ] #Takes detections only within specified rkms
  det <- df[df$DetectDate > str_startdate & df$DetectDate < str_enddate, ] #takes detections only within certain dates
  df_allmoves <- NULL

  if (nrow(det) > 0) {
  det$Detect_rkm <- as.numeric(det$Detect_rkm)
  det$Release_rkm <- as.numeric(det$Release_rkm)
  det <- det[is.na(det$Detect_rkm) == FALSE, ]
  det <- det[is.na(det$Release_rkm) == FALSE, ]
  all_tags <- c(unique(det$TagID))

  count <- 1
  total_count <- length(all_tags)
  print(species_name)
  for (tag in all_tags) {
    #Get fish that have been detected for over a specified number of years.
    print(paste(count, '/', total_count))
    ind_tag <- det[det$TagID == tag, ]
    #print(ind_tag)
    #print(nrow(ind_tag))
    if (nrow(ind_tag) > 0) {
    ind_tag <- ind_tag[order(ind_tag$DetectDate), ]
    first_det <- min(ind_tag$DetectDate)
    last_det <- max(ind_tag$DetectDate)
    length <- ind_tag$Length[1]
    all_rows <- c(1:nrow(ind_tag))
    total_rkm <- abs(ind_tag$Release_rkm[1] - ind_tag$Detect_rkm[1])
    for (row in all_rows) {
      first_rkm <- ind_tag$Detect_rkm[row]
      second_rkm <- ind_tag$Detect_rkm[row + 1]
      if (row == nrow(ind_tag)) { #may need to add some subtleties to make sure that if the last detection is different from the second to last that this gets accounted for.
        species <- species_name
        df_allmoves <- rbind(df_allmoves, data.frame(tag, total_rkm, species, study_year_month, length, first_det, last_det))
        break
      }
      if (first_rkm != second_rkm) {
        diff_rkm <- abs(first_rkm - second_rkm)
        total_rkm <- total_rkm + diff_rkm
      }
    }
    count <- count + 1
    }
  }
}
  return(df_allmoves)
}

head_all_years <- head(all_years_and_months, 36)

eight_year_motility <- get_all_motilities_months(df=det_eight_year_sturg, species_name='Green_Sturgeon', year_list=all_years_and_months)

test_eight_year_motility <- get_all_motilities_months(df=det_eight_year_sturg, species_name='Green_Sturgeon', year_list=head_all_years)


df_test_motility <- ranking_all_years(df=df_longterm, species_name='Green_Sturgeon', year_list=all_years_and_months)



eight_year_sturg <- get_longterm_sturgeon(green, green, 8)
det_eight_year_sturg <- get_specific_tags(green, eight_year_sturg)



####################### Generate the Graphs ############################



test_id <- subset(eight_year_motility, eight_year_motility$tag == 4343)

get_individual_plots <- function(df_motility, year_list, pdf_name) {
  pdf(paste(pdf_name, ".pdf", sep = "" ),width = 11, height = 8)
  graph_list <- list() #makes an empty list to store all of the plots
  first_year <- year_list$year[1]
  last_year <- year_list$year[nrow(year_list)]
  all_years <- 1:nrow(year_list)
  species_name <- as.character(df_motility$species[1])
  for (y in all_years) {
    start_datetime <- as.character(year_list$start_date[y])
    end_datetime <- as.character(year_list$end_date[y])
    study_year <- as.character(year_list$year[y])
    motility <- subset(df_motility, first_det > start_datetime & last_det < end_datetime)
    
    #calculate Rsquared, P value, slope, and intercept and plot them on each graph.
    #linear_mod <- lm(log10(total_rkm) ~ index, data=motility) #does this need to be log transformed?
    #model_summary <- summary(linear_mod)
    #model_coef <- model_summary$coefficients
    #r2 <- signif(model_summary$r.squared, digits = 3)
    #beta.estimate <- signif(model_coef['index', 'Estimate'], digits=3) # slope?
    #intercept <- signif(model_coef['(Intercept)', 'Estimate'], digits=3) #intercept
    #std.error <- signif(model_coef['index', 'Std. Error'], digits=3)
    #t_value <- beta.estimate/std.error
    #p_value <- signif(2*pt(-abs(t_value), df=nrow(motility)-ncol(motility)), digits = 3)
    
    #v_all_values <- c(r2, p_value)
    #df_all_values <- data.frame(v_all_values)
    
    #want p_value, r2, slope, and intercept
    
    
    p <- ggplot(data = motility, aes(x= index, y= log10(total_rkm), color = (tag))) + 
      geom_point() + 
      geom_line() +
      theme(legend.position = 'none') +
      xlab('Rank') +
      ylab('RKM') +
      ggtitle(study_year) +
      #ggtitle(paste(study_year, 'Rank vs Motility of', species_name, 'in Sacramento/San Joaquin Watershed')) +
      #theme(plot.title = element_text(size = 10, face = "bold")) +
      theme(axis.title = element_text(size=8)) +
      scale_x_continuous(limits = c(0, 65), breaks = seq(0, 65, by = 10)) +
      scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 1)) +
      geom_smooth(method='lm', col='red', size=.25, se = FALSE) + 
      annotate('text', x=.35, y=1, label = paste('P-Value:', p_value, '\n',
                                                 'R2 Value:',  r2, '\n',
                                                 'Slope:', beta.estimate, '\n',
                                                 'Intercept:', intercept), size=2.5)
    
    graph_list <- c(graph_list, list(p))
  }
  do.call('grid.arrange', c(graph_list, top = paste(species_name, 'Motility vs Rank', first_year, 'to', last_year)))
  dev.off()
}







