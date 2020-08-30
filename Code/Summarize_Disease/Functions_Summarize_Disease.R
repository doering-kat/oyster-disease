#calculate dermo or MSX prevalance from the disease data (DZDataSampleInforJoin.csv

# Do this at the standard disease bars.
# df: should be the file "./Derived_Data/DZDataSampleInfoJoin.csv
# disease: can be "MSX" or "Dermo"
# level: is implemented only for "bar" (may generalize to NOAA code as well later)
# returns: a dataframe with the data.
CalcPrevalence <- function(dis_df, disease = "MSX"){
# Filter  the data to calculate.
    if(disease == "Dermo"){ #have to use the intensity column to calculate presence
        dis_df <-  dis_df %>% 
            # mutate so PMAR_Intensity is made of 1s and 0s fore presence/absence.
            mutate(Presence = ifelse(PMAR_Intensity %in% 1:7, 1, PMAR_Intensity)) %>% 
            select(YearSamp, ID, SiteType.x, Presence)
    }
    if(disease == "MSX"){ # use a presence/absence column already
        dis_df <- dis_df %>% 
                    rename(Presence = Hnel_Presence) %>% 
                    select(YearSamp, ID, SiteType.x, Presence)
    }
    # filter because only want the standard disease bars
    dis_df <- filter(dis_df, SiteType.x == "Standard Disease Site (43 Key Bars)") %>% 
                select(-SiteType.x) %>% #no longer need SiteType.x.
                na.omit() # get rid of any NA's for YearSamp, ID, or Presence.
    
    # count oysters by year and ID that did not have the disease.
    no_dis <- dis_df %>%
                filter(Presence == 0) %>%  # not present
                group_by(YearSamp, ID) %>%
                # each line represents an animal, so count total by yr and bar.
                count()%>%
                rename(no_n = n) %>%   #rename te count column.
                ungroup() #remove grouping variables.
    #count oysters by year and ID that did have the disease.
    yes_dis <- dis_df %>% 
                filter(Presence == 1) %>%  # present
                group_by(YearSamp, ID) %>% 
                # each line represents an animal, so count total by yr and bar.
                count()%>%
                rename(yes_n = n) %>%  #rename the count column.
                ungroup() #remove grouping variables.
    # calculate the total sampled for disease.
    total_dis <- dis_df %>% 
                   filter(Presence == 1 | Presence == 0) %>% 
                   group_by(YearSamp, ID) %>% 
                   count() %>% 
                   rename(dis_n = n) %>% 
                   ungroup()
    #calculate prevalence.
    
    #join no and yes dis to total dis
    prev_df <- left_join(total_dis, yes_dis, by = c("YearSamp", "ID")) %>% #join the positive dis
        left_join(no_dis, by = c("YearSamp", "ID")) %>% #join the negative diss also
        replace_na(list(yes_n = 0, no_n = 0)) #replace NA's with 0s
    
    #calculate prevalance: should just be yes_n/tot_n *100
    prev_df <- mutate(prev_df, dis_Prev = yes_n/dis_n)
    prev_df$disease <- disease # add a column with the disease.
    return(prev_df) # return the prevalence data.
}

CalcDermoIntensty <- function(dis_df, disease = "Dermo"){
    # Filter  the data to calculate.
    if(disease == "Dermo"){ #have to use the intensity column to calculate presence
        dis_df <-  dis_df %>% 
            # mutate so PMAR_Intensity is made of 1s and 0s fore presence/absence.
            mutate(Presence = ifelse(PMAR_Intensity %in% 1:7, 1, PMAR_Intensity)) %>% 
            select(YearSamp, ID, SiteType.x, Presence, PMAR_Intensity)
    }
    if(disease == "MSX"){ # use a presence/absence column already
        stop("This function not written for use with MSX.")
    }
    
    # filter because only want the standard disease bars
    dis_df <- filter(dis_df, SiteType.x == "Standard Disease Site (43 Key Bars)") %>% 
        select(-SiteType.x) %>% #no longer need SiteType.x.
        na.omit() # get rid of any NA's
    
    #calculate mean infection intensity. On a scale from 0 -7.
    mean_intensity <- dis_df %>% 
                        group_by(YearSamp, ID) %>% 
                        summarize(Mean_Intensity = mean(PMAR_Intensity)) %>% 
                        ungroup()
    
    #Calculate percent with lethal infections (>= 5)
    yes_lethal <- dis_df %>% 
                    select(YearSamp, ID, PMAR_Intensity) %>% 
                    filter(PMAR_Intensity >=5) %>%  # lethal infections
                    group_by(YearSamp, ID) %>% 
                    # each line represents an animal, so count total by yr and bar.
                    count()%>%
                    rename(lethal_n = n) %>%  #rename the count column.
                    ungroup() #remove grouping variables.
        
    all_sampled <- dis_df %>% 
                    select(YearSamp, ID, PMAR_Intensity) %>% 
                    filter(PMAR_Intensity <= 7) %>% 
                    group_by(YearSamp, ID) %>% 
                    count() %>% 
                    rename(total_n = n) %>%
                    ungroup()
    #get percent lethal.
    #    #join no and yes dis to total dis
    lethal_df <- left_join(all_sampled, yes_lethal, by = c("YearSamp", "ID")) %>% #join the positive dis
        replace_na(list(total_n = 0, lethal_n = 0)) %>%  #replace NA's with 0s
        mutate(PercentLethal = 100*(lethal_n/total_n))
    
    df <- full_join(lethal_df, mean_intensity, by = c("YearSamp", "ID"))
    df$dis <- disease
    
    return(df) # return the prevalence data.
}
