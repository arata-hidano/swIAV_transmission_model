#=========================================================================#
#----------Mathematical model for within-farm swIAV transmission v1-------
#=========================================================================#
#****CHUNK 1
library(tidyverse)
library(data.table)
 set.seed(12345)
  # Define variables
    # DISEASE STATE: M S E I R (tentative: need to think if include E, check how long it is)
    s_mda = 0
    s_S = 1
    s_E = 2 # TO DO: needs to add event from E to I
    s_I = 3 # TO DO: needs to add event from I to R
    s_R = 4
    
    # INDIVIDUAL LEVEL OF IAV IN GILT
    prev_IAV_gilt = 0.8
    # Decay of maternal derived antibody
    day_mda_loss = 70 # it's noy 1 or 0, gradual decline, so need to think how to function that
    # Decay of immunity after infection
    day_immunity_loss = 180
    # Latent period
    day_latent = 2
    # Infectious period
    day_infectious = 6
    # Transmission coefficient (direct)
    beta_direct = 0.5 # need to refine, density-dependent? Assuming R0 = 3, D = 6 then beta = beta_direct/N (now fequency-dependent)
    # Transmission coefficeint (indirect)
    beta_indirect = 0.001 # guessed
    # INITIAL FARM LEVEL PREVALENCE
    prev_farm = 0.1 # Need to stratify depending on farm types
    
    
    # DEMOGRAPHIC STATE: PIGLET (0), WEANED (1), FATTENING (2), GILTS (3) SOWS (4), BOARS (5)
    d_piglet = 0
    d_weaned = 1
    d_fattening = 2
    d_gilt = 3
    d_sow = 4
    d_boar = 5
    
    female = 0
    male = 1
    
  # Simulation specific variable
    d_update_interval = 1 # unit is day
    s_update_interval = 1 # unit is day
    current_day = 0 # days in simulation
    count_month = 0 # months in simulation
    length_simulation_day = 2*12*30 # unit is day

  # Set up demographic
    # smallholder with sow = 3: try to replicate Tiemann et al 2018
    # sow1 (id 1) age 2. Farrow on Day 10 (MAY)
    # sow2 (id 2) age 3. Farrow on Day 40 (JUNE)
    # sow3 (id 3) age 4. Farrow on Day 70 (JULY)
    # each farrow, 7 piglets and 3 dies within a week. Weaning age 3 mo (90 days)
    # Then mating doesn't happen by September
    # Probability of mating on September, October, November 33% each
    # sell as matured pig when reached age. Sows culled at age 5 or after they weaned.
    
    # REMOVAL
        # Mortality of sows
        cull_mo_min = 58
        cull_mo_max = 65 # if sows reach this minimum age of culling and sows not having weaned piglets, then cull
        
        # Mortality of piglet before weaning
        num_die_piglet_min = 1
        num_die_piglet_max = 4
        
        # r_reason
        r_death = 0
        r_sold = 1
        r_culled = 2
    
    
    # Weaning age/period in day
    wean_day_min = 81
    wean_day_max = 108
    
    # Time from weaning to become grower (needs this info)
    wean_to_grow = 90
    
    # Time from entering growing until finishing
    grow_to_finish = 180
    
    # Time from entering growing until first farrowing (15mo)
    grow_to_first_farrow = 250
    
    # Time from introducing into a farm until first farrowing
    introduction_to_first_farrow = 150
    # Time from first farrow till culling 
    first_farrow_cull_min = 40*30
    first_farrow_cull_max = 55*30
    
    # litter size: uniform(6,9)
    litter_size_min = 6
    litter_size_max = 9
    

    
    # Reproduction parameter
    return_to_heat = 80
    gestation_period = 120
  # INITIALIZE FARM AND ANIMAL INFO
    n_backyard = 100
    EMPTY_data_table = data.table(id = integer(), 
                                  farm_id = integer(), 
                                  demographic = integer(), 
                                  age = numeric(), 
                                  sex = integer(),
                                  status = integer(), 
                                  next_d_date = integer(), 
                                  next_s_date = integer(),
                                  farrow_date = integer(), 
                                  removal_date = integer(),
                                  #immunity_date = integer(),  # REMOVED AND REPLACED BY PUTTING THIS INFO IN next_s_date
                                  infection_history = integer(),
                                  farrow_times = integer()
                                  
    ) 
    
    # demographic and status: see above. sex: female 0 and male 1. age in months. 
    # immunity_date: day until immunity dissapears. next_d_date: day until weaning. 
    # infection_history: 0 not infected before. 1 infected before. Needs to consider multiple subtype maybe. 
   
  # SETTING UP FARM_data_frame - now onlyconsidering farrow-to-finish but this needs to be updated
    # SAME FARM BUT DIFFERENT BUILDING - allocate same identity but differentiate by ID
    farm_id = seq_along(1:n_backyard)
    prop_introduce_gilt = 0.2
    introduce_replace_gilt = rbinom(n_backyard,1,prop_introduce_gilt)
    # introduce_replace_gilt = 0
    # FOR FARM, NEED TO THINK HOW TO SAVE BUILDING MANAGEMENT DATA AND HOW PIGS HAVE DIRECT CONTACTS
    # MAYBE THOSE HAVE DIRECT CONTACTS IN THE SAME ROW, THEN INDIRECT ONE IN DIFFERENT ROWS WITH ID SHOWING THEY ARE GROUPED
    FARM_data_frame = data.table(farm_id = farm_id,
                                 introduce_replace_gilt = introduce_replace_gilt,
                                 N_PIGLET = integer(),
                                 N_WEANED = integer(),
                                 N_FATTENING = integer(),
                                 N_GILT = integer(),
                                 N_SOW = integer(),
                                 N_BOAR = integer(),
                                 N_TOTAL = integer(),
                                 N_SUSCEPTIBLE = integer(),
                                 N_EXPOSED = integer(), # TOTAL NUMBER OF EXPOSED ANIMALS IN THIS GROUP
                                 N_INFECTED = integer(), # TOTAL NUMBER OF INFECTED ANIMALS IN THIS GROUP
                                 N_IMMUNE = integer(),
                                 N_MDA = integer(),
                                 day_p_start = integer()
                                 )
    
     
  # SOW_data_frame
      ## sows
      # SOW_data_frame = EMPTY_data_table
      current_pig_id = 0
      start_date = as.Date('2020-05-01')
      
    ANIMAL_data_frame = EMPTY_data_table
      
      # FOR EACH FARM INITIALISE, ADD SOWS FIRST
      for(i in 1:n_backyard)
      {
        temp_init = sample(1:6,1)
        
        # UPDATE FARM_data_frame
        FARM_data_frame[i,]$N_PIGLET = 0
        FARM_data_frame[i,]$N_WEANED = 0
        FARM_data_frame[i,]$N_FATTENING = 0                                            
        FARM_data_frame[i,]$N_GILT = 0 
        FARM_data_frame[i,]$N_SOW = temp_init 
        FARM_data_frame[i,]$N_BOAR = 0 
        
        if(rbinom(1,1,prev_farm)==1)
        {
          FARM_data_frame[i,]$N_EXPOSED = 1
          FARM_data_frame[i,]$N_SUSCEPTIBLE = temp_init - 1
          temp_exposed_id = sample(1:temp_init,1)
          temp_status = rep(s_S,temp_init)
          temp_status[temp_exposed_id] = s_E
          temp_s_date = rep(0,temp_init)
          temp_s_date[temp_exposed_id] = day_latent
        } else
        {
          FARM_data_frame[i,]$N_EXPOSED = 0
          temp_status = rep(s_S,temp_init)
          temp_s_date = rep(0,temp_init)
          FARM_data_frame[i,]$N_SUSCEPTIBLE = temp_init
        }
        FARM_data_frame[i,]$N_INFECTED = 0
        FARM_data_frame[i,]$N_IMMUNE = 0
        FARM_data_frame[i,]$N_MDA = 0
        FARM_data_frame[i,]$N_TOTAL = temp_init
        # THIS CAN BE DIFFERENT IF THE INITIAL CONDITION IS DIFFERENT
        
        init_sow_id= seq_along(1:temp_init) + current_pig_id
        current_pig_id = current_pig_id + temp_init
        ANIMAL_data_frame = ANIMAL_data_frame %>% add_row(id = init_sow_id, 
                                                    farm_id = rep(i,length(init_sow_id)), 
                                                    demographic = rep(d_sow,length(init_sow_id)),
                                                    sex = rep(female,length(init_sow_id)), 
                                                    status = temp_status,
                                                    farrow_times = rep(0,length(init_sow_id)),
                                                    next_s_date = temp_s_date
        )                                        
       
      }
      
     
     
      # INITIAL CONDITION
          # SOW
          
          first_farrow = sample(as.numeric(as.Date('2020-08-01') - start_date),NROW(ANIMAL_data_frame),replace=T)
          ANIMAL_data_frame = ANIMAL_data_frame %>% mutate(farrow_date=coalesce(farrow_date,first_farrow)) 
           # if farrow_date >0 meaning that it's pregnant. 0 means it's empty
          init_sow_age_min = 13
          init_sow_age_max = 52 # can apply normal distribution or anything else
          init_sow_age = sample((init_sow_age_max-init_sow_age_min),NROW(ANIMAL_data_frame),replace=T) + init_sow_age_min
          ANIMAL_data_frame = ANIMAL_data_frame %>% mutate(age=coalesce(age,init_sow_age)) 
          ANIMAL_data_frame$next_d_date = 0 # when next_d_date >0 means it's milking (sows) or being milked (piglet)
          #SOW_data_frame$immunity_date = 0
          sow_mortality_date = (sample(cull_mo_min:cull_mo_max,NROW(ANIMAL_data_frame),replace=T) - init_sow_age)*30
          ANIMAL_data_frame$removal_date = sow_mortality_date
  # # DATA FRAME
  #     PIGLET_data_frame = WEANER_data_frame = FATTEN_data_frame = GILT_data_frame = EMPTY_data_table
      
      
  # REMOVE_data_frame
      REMOVE_data_frame = EMPTY_data_table
      REMOVE_data_frame = mutate(REMOVE_data_frame, r_reason = integer())
      
  # EVENT table
      FARROW_EVENT = data.table(months=integer(),year=integer())
      
  # LENGTH OF PERSISTENCE
      # maybe just add into vector and record farm id in another vector?
      # first day of having S till no S exists
      persistence_vector = c()
      persistence_farm_id = c()
      persistence_farm_x = c()
      immune_farm_x = c()
      gilt_x = c()
      farrow_x = c()
      x = 46
#****CHUNK 1 DONE

      
      
      
      
#=================================================================================
      
     
#=========START SIMULATION==========================================================
  while(current_day<length_simulation_day)
  {
#=========GOING THROUGH EACH DATE=====================================================
    # UPDATE DAY AND COUNT OF EACH EVENT
    current_day = current_day + 1
    current_month = (current_day %/% 30 + 5) %% 12 + 1  # it's 5 because the simulation starts in May
    current_year = current_day %/% 365
    persistence_farm_x = c(persistence_farm_x,FARM_data_frame[x,]$N_INFECTED)
    immune_farm_x = c(immune_farm_x,FARM_data_frame[x,]$N_IMMUNE)
      # Mating happens less between May and September
      if(current_month>=5 & current_month <= 8) 
      {
        prob_mating = 0.001
      }
      else if(current_month>=9 & current_month <= 10)
      {
        prob_mating = 0.001
      }
      else
      {
        prob_mating = 0.15
      }
      # SUBTRACT 1 FROM each variable that describes dates
    ANIMAL_data_frame$next_s_date = ANIMAL_data_frame$next_s_date - 1 # DAY FOR STATUS CHANGE
    ANIMAL_data_frame$next_d_date = ANIMAL_data_frame$next_d_date - 1 # DAY FOR DEMOGRAPHIC CHANGE
    ANIMAL_data_frame$farrow_date = ANIMAL_data_frame$farrow_date - 1 # DAY FOR FARROW - DO I NEED THIS IN ADDITION TO next_d_date
    ANIMAL_data_frame$removal_date = ANIMAL_data_frame$removal_date - 1 # DAY FOR REMOVAL
    
    
    
#============GOING THROUGH EACH FARM FOR EVENTS===================================#
    # Calculate the number of new infections beta*S*I, IF S > 0
    for(nfarm in 1:NROW(FARM_data_frame))
    {
            if(FARM_data_frame[nfarm,]$N_INFECTED>0)
            {
              eval_trans = 1 #IF 1 THEN EVALUATE TRANSMISSION EVENT
              temp_I = FARM_data_frame[nfarm,]$N_INFECTED
              temp_lamda = temp_I * beta_direct/ FARM_data_frame[nfarm,]$N_TOTAL
              temp_S = FARM_data_frame[nfarm,]$N_SUSCEPTIBLE
            }else
            {
              eval_trans = 0
            }
            
      #=====DISEASE TRANSMISSION COMPONENT=========================
                if(eval_trans==1)
                {
                  
                  temp_data = ANIMAL_data_frame %>% filter(farm_id == nfarm) 
                  old_s_vec = temp_data %>% pull(status)
                  old_s_date = temp_data %>% pull(next_s_date)
                  # old_s_vec = ANIMAL_data_frame[ANIMAL_data_frame$farm_id==nfarm & ANIMAL_data_frame$demographic==d_piglet,]$status
                  # old_s_date = ANIMAL_data_frame[ANIMAL_data_frame$farm_id==nfarm & ANIMAL_data_frame$demographic==d_piglet,]$next_s_date
                  
                  # DETERMINE THE NUMBER OF NEW INFECTED
                  new_inf_count =  rbinom(1,temp_S,temp_lamda)
                    if(new_inf_count>0)
                    {
                      # EXTRACT SUSCPETIBLE FROM THIS FARM
                      
                      temp_SUS_id = temp_data %>% filter(status==s_S) %>% pull(id)
                        
                      if(length(temp_SUS_id)==1) # if only one animal choose this animal
                      {
                        new_EXPOSED_id = temp_SUS_id
                      }
                      else
                      {
                        new_EXPOSED_id = sample(temp_SUS_id,new_inf_count,replace=F)
                      }
                      
                      ANIMAL_data_frame[(ANIMAL_data_frame$farm_id == nfarm) & (ANIMAL_data_frame$id %in% new_EXPOSED_id),]$status = s_E
                      ANIMAL_data_frame[(ANIMAL_data_frame$farm_id == nfarm) & (ANIMAL_data_frame$id %in% new_EXPOSED_id),]$next_s_date = day_latent
                      
                      # THEN UPDATE FARM_data_frame
                      FARM_data_frame[nfarm,]$N_EXPOSED = FARM_data_frame[nfarm,]$N_EXPOSED + new_inf_count
                      FARM_data_frame[nfarm,]$N_SUSCEPTIBLE = FARM_data_frame[nfarm,]$N_SUSCEPTIBLE - new_inf_count
                    }
                 
                }
      
      #=====DISEASE TRANSMISSION COMPONENT DONE===================
      
      
      
      
      
      
      #=====MORATLITY AND REMOVAL=================================
      
      # count how many animals removed from each age group 
      ###### TO DO
      ###### NEED TO CHECK REASON FOR REMOVAL? HOW?? When put removal_date, add info already?
      #####  CONTINUE OTHER EVENTS
                temp_data = ANIMAL_data_frame %>% filter(farm_id == nfarm)
                remove_data = temp_data %>% filter(removal_date==0)
                if(NROW(remove_data)>0)
                {
                # CHANGE DEMOGRAPHIC
                 temp_demographic = remove_data %>% count(demographic)
                 FARM_data_frame[nfarm,]$N_TOTAL = FARM_data_frame[nfarm,]$N_TOTAL - NROW(remove_data)
                 for(n_temp_demographic in 1:NROW(temp_demographic))
                 {
                   temp_demo = temp_demographic[n_temp_demographic,] %>% pull(demographic)
                   temp_num = temp_demographic[n_temp_demographic,] %>% pull(n)
                   if(temp_demo==d_piglet)
                   {
                     FARM_data_frame[nfarm,]$N_PIGLET = FARM_data_frame[nfarm,]$N_PIGLET - temp_num
                   }
                   else if(temp_demo==d_weaned)
                   {
                     FARM_data_frame[nfarm,]$N_WEANED = FARM_data_frame[nfarm,]$N_WEANED - temp_num
                   }
                   else if(temp_demo==d_fattening)
                   {
                     FARM_data_frame[nfarm,]$N_FATTENING = FARM_data_frame[nfarm,]$N_FATTENING - temp_num
                   }
                   else if(temp_demo==d_gilt)
                   {
                     FARM_data_frame[nfarm,]$N_GILT = FARM_data_frame[nfarm,]$N_GILT - temp_num
                   }
                   else if(temp_demo==d_sow) # @@@ REPLACE BY NEW GILTS
                   {
                     FARM_data_frame[nfarm,]$N_SOW = FARM_data_frame[nfarm,]$N_SOW - temp_num
                     # EXTRACT FARM POLICY
                      temp_policy = FARM_data_frame[nfarm,]$introduce_replace_gilt
                      
                                              if(temp_policy == 0) # IF PRIORITISING OWN ANIMALS
                                              {
                                                # OPTION 1: REPLACE BY OWN PIGLET/GILT
                                                temp_WEANER = ANIMAL_data_frame[ANIMAL_data_frame$sex == female & ANIMAL_data_frame$farm_id == nfarm & ANIMAL_data_frame$demographic==d_weaned,]
                                                
                                                # STEP 1: COMPARE temp_num and NROW(temp_WEANER), if NROW > temp_num, AVAILABLE GILT IS ENOUGH FOR REPLACEMENT
                                                if(NROW(temp_WEANER)>=temp_num)
                                                {
                                                  temp_WEANER = temp_WEANER[order(temp_WEANER$next_d_date),]
                                                  temp_WEANER_id = temp_WEANER[1:temp_num,]$id 
                                                  ANIMAL_data_frame[ANIMAL_data_frame$id %in% temp_WEANER_id,]$demographic = d_gilt
                                                  # NEED To CHANGE next_d_date
                                                  ANIMAL_data_frame[ANIMAL_data_frame$id %in% temp_WEANER_id,]$next_d_date =  ANIMAL_data_frame[ANIMAL_data_frame$id %in% temp_WEANER_id,]$next_d_date + grow_to_first_farrow
                                                  FARM_data_frame[nfarm,]$N_WEANED =  FARM_data_frame[nfarm,]$N_WEANED - temp_num
                                                  FARM_data_frame[nfarm,]$N_GILT = FARM_data_frame[nfarm,]$N_GILT + temp_num
                                                }
                                                # STEP 2: IF temp_num > NROW, TAKE ALL WEANER INTO GILTS. TAKE REMAINING FROM PURCHASE
                                                else if(NROW(temp_WEANER)<temp_num & NROW(temp_WEANER)>=0)
                                                {
                                                  temp_WEANER_id = temp_WEANER$id 
                                                  if(NROW(temp_WEANER)>0)
                                                  {
                                                    ANIMAL_data_frame[ANIMAL_data_frame$id %in% temp_WEANER_id,]$demographic = d_gilt
                                                    ANIMAL_data_frame[ANIMAL_data_frame$id %in% temp_WEANER_id,]$next_d_date =  ANIMAL_data_frame[ANIMAL_data_frame$id %in% temp_WEANER_id,]$next_d_date + grow_to_first_farrow
                                                    FARM_data_frame[nfarm,]$N_WEANED =  FARM_data_frame[nfarm,]$N_WEANED - NROW(temp_WEANER)
                                                  }
                                                  
                                                  FARM_data_frame[nfarm,]$N_GILT = FARM_data_frame[nfarm,]$N_GILT + temp_num
                                                  # REMAINING FROM PURCHASE - introduce temp_num animals
                                                  temp_num = temp_num - NROW(temp_WEANER)
                                                  # ADD GILT - WHEN TO ADD? INFECTION STATUS OF GILT (BASED On GLOBAL PREVALENCE FOR NOW)
                                                  temp_gilt_status = rbinom(temp_num,1,prev_IAV_gilt)
                                                  temp_s_date = ifelse(temp_gilt_status==1,day_latent,0)
                                                  temp_n_infected = length(temp_gilt_status[temp_gilt_status==1])
                                                
                                                  temp_gilt_status = ifelse(temp_gilt_status==1,s_E,s_S) # Because infected is s_I
                                                  
                                                  ANIMAL_data_frame =  ANIMAL_data_frame %>% add_row(id = seq_along(1:temp_num)+current_pig_id,
                                                                                     next_d_date = rep(introduction_to_first_farrow,temp_num),
                                                                                     farm_id = rep(nfarm,temp_num),
                                                                                     demographic = rep(d_gilt,temp_num),
                                                                                     sex = rep(female,temp_num), 
                                                                                     status = temp_gilt_status,
                                                                                     next_s_date = temp_s_date,
                                                                                     farrow_times = rep(0,temp_num),
                                                                                     removal_date = rep(-1,temp_num) # ASSUMING INTRODUCED GILT DOESN't DIE UNTIL FARROWING
                                                  )
                                                  if(nfarm==x)
                                                  {
                                                    gilt_x = c(gilt_x,current_day)
                                                  }
                                                  current_pig_id = current_pig_id + temp_num
                                                  # UPDATE disease counter
                                                  FARM_data_frame[nfarm,]$N_EXPOSED = FARM_data_frame[nfarm,]$N_EXPOSED + temp_n_infected
                                                  FARM_data_frame[nfarm,]$N_SUSCEPTIBLE = FARM_data_frame[nfarm,]$N_SUSCEPTIBLE + (temp_num -temp_n_infected)
                                                  FARM_data_frame[nfarm,]$N_TOTAL = FARM_data_frame[nfarm,]$N_TOTAL + temp_num
                                                }
                                              } # OPTION 1 END
                                                        else
                                                        {
                                                          if(nfarm==x)
                                                          {
                                                            gilt_x = c(gilt_x,current_day)
                                                          }
                                                          # OPTION 2: REPLACE BY INTRODUCTION 
                                                          # ADD GILT - WHEN TO ADD? INFECTION STATUS OF GILT (BASED On GLOBAL PREVALENCE FOR NOW)
                                                          temp_gilt_status = rbinom(temp_num,1,prev_IAV_gilt)
                                                          temp_n_infected = length(temp_gilt_status[temp_gilt_status==1])
                                                        
                                                          temp_s_date = ifelse(temp_gilt_status==1,day_latent,0)
                                                          temp_gilt_status = ifelse(temp_gilt_status==1,s_E,s_S) # Because infected is s_I
                                                          # If between MAY and September, longer time to first farrow
                                                          
                                                         
                                                          ANIMAL_data_frame =  ANIMAL_data_frame %>% add_row(id = seq_along(1:temp_num)+current_pig_id,
                                                                                             next_d_date = rep(introduction_to_first_farrow,temp_num),
                                                                                             farm_id = rep(nfarm,temp_num),
                                                                                             demographic = rep(d_gilt,temp_num),
                                                                                             sex = rep(female,temp_num), 
                                                                                             status = temp_gilt_status,
                                                                                             next_s_date = temp_s_date,
                                                                                             farrow_times = rep(0,temp_num),
                                                                                             removal_date = rep(-1,temp_num) # ASSUMING INTRODUCED GILT DOESN't DIE
                                                                                              )
                                                         
                                                          current_pig_id = current_pig_id + temp_num
                                                          # UPDATE disease counter
                                                          FARM_data_frame[nfarm,]$N_EXPOSED = FARM_data_frame[nfarm,]$N_EXPOSED + temp_n_infected
                                                          FARM_data_frame[nfarm,]$N_SUSCEPTIBLE = FARM_data_frame[nfarm,]$N_SUSCEPTIBLE + (temp_num -temp_n_infected)
                                                          FARM_data_frame[nfarm,]$N_TOTAL = FARM_data_frame[nfarm,]$N_TOTAL + temp_num
                                                          FARM_data_frame[nfarm,]$N_GILT = FARM_data_frame[nfarm,]$N_GILT + temp_num
                                                        }
                   }
                   else if(temp_demo==d_boar)
                   {
                     FARM_data_frame[nfarm,]$N_BOAR = FARM_data_frame[nfarm,]$N_BOAR - temp_num
                   }
                 }
                # CHANGE DISEASE STATUS
                 temp_status = remove_data %>% count(status)
                
                 for(n_status in 1:NROW(temp_status))
                 {
                   temp_demo = temp_status[n_status,] %>% pull(status)
                   temp_num = temp_status[n_status,] %>% pull(n)
                   if(temp_demo==s_S)
                   {
                     FARM_data_frame[nfarm,]$N_SUSCEPTIBLE = FARM_data_frame[nfarm,]$N_SUSCEPTIBLE - temp_num
                   }
                   else if(temp_demo==s_E)
                   {
                     FARM_data_frame[nfarm,]$N_EXPOSED = FARM_data_frame[nfarm,]$N_EXPOSED - temp_num
                   }
                   else if(temp_demo==s_I)
                   {
                     FARM_data_frame[nfarm,]$N_INFECTED = FARM_data_frame[nfarm,]$N_INFECTED - temp_num
                      # RECORD DURATION OF PERSISTENCE
                       if(FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_INFECTED==0)
                       {
                         
                         diff = current_day - FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$day_p_start
                         persistence_vector = c(persistence_vector,diff)
                         persistence_farm_id = c(persistence_farm_id,nfarm)
                       }
                   }
                   else if(temp_demo==s_R)
                   {
                     FARM_data_frame[nfarm,]$N_IMMUNE = FARM_data_frame[nfarm,]$N_IMMUNE - temp_num
                   }
                   else if(temp_demo==s_mda)
                   {
                     FARM_data_frame[nfarm,]$N_IMMUNE = FARM_data_frame[nfarm,]$N_IMMUNE - temp_num
                   }
              
                 }
                 
                 # REMOVE ANIMALS FROM DATA
                 ANIMAL_data_frame = ANIMAL_data_frame[(ANIMAL_data_frame$farm_id != nfarm) | (ANIMAL_data_frame$farm_id == nfarm & ANIMAL_data_frame$removal_date!=0),]
                 REMOVE_data_frame = REMOVE_data_frame %>% add_row(remove_data)
                }
               
            
              
            # @@@ how to differentiate culling and death? also now not considering mortality except pigs, what to do?
         
      #=====MORTALITY AND REMOVAL DONE============================
      
                
                
                
                
      #====FARROWING EVENT========================================
                temp_data = ANIMAL_data_frame %>% filter(farm_id == nfarm)
                temp_farrowing_data = temp_data %>% filter(farrow_date==0)
                if(NROW(temp_farrowing_data)>0)
                {
                  if(nfarm==x)
                  {
                    farrow_x = c(farrow_x,current_day)
                  }
                  for(animal in 1:NROW(temp_farrowing_data))
                  {
                    this_animal = temp_farrowing_data[animal,]
                    temp_id = this_animal$id
                    temp_status = this_animal$status
                      # UPDATE SOW DATA
                      ANIMAL_data_frame[ANIMAL_data_frame$id==temp_id,]$farrow_times = ANIMAL_data_frame[ANIMAL_data_frame$id==temp_id,]$farrow_times + 1
                      
                        # DETERMINE HOW MANY PIGLETS
                        n_piglet = sample(litter_size_min:litter_size_max,1)
                        
                        # DETERMINE WHICH PIGLET DIES WHEN IN THE FIRST WEEK
                        n_piglet_mortality = sample(num_die_piglet_min:num_die_piglet_max,1)
                        day_piglet_mortality = c(sample(1:wean_day_min,n_piglet_mortality,replace=T),rep(0,n_piglet-n_piglet_mortality))
                                # Because not all piglets are dying, indicate removeal_date = 0 if they don't die while piglets
                        
                        # DETERMINE next_d_date
                        piglet_next_d_date = sample(wean_day_min:wean_day_max,1,replace=F)
                        ANIMAL_data_frame[ANIMAL_data_frame$id==temp_id,]$next_d_date = piglet_next_d_date # WEANING DATE
                        
                        # DETERMINE IF MDA IS PASSED DEPENDING ON THE STATUS OF SOW
                        if(temp_status==s_R)
                        {
                          piglet_status = s_mda
                          # piglet_immunity_date = sample(day_mda_loss,n_piglet,replace=T)
                          piglet_immunity_date = round(-1*log(sample(1:100,n_piglet,replace=T)/100)/(1/day_mda_loss))
                          # UPDATE FARM TABLE
                          FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_PIGLET = FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_PIGLET + n_piglet
                          FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_IMMUNE = FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_IMMUNE + n_piglet
                          FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_TOTAL = FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_TOTAL + n_piglet
                        }
                        else # dam not immunity
                        {
                          piglet_status = s_S
                          piglet_immunity_date = rep(0,n_piglet)
                          # UPDATE FARM TABLE
                          FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_PIGLET = FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_PIGLET + n_piglet
                          FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_SUSCEPTIBLE = FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_SUSCEPTIBLE + n_piglet
                          FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_TOTAL = FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_TOTAL + n_piglet
                        }
                        # ADD NEW BORN TO PIGLET DATAFRAME
                        ANIMAL_data_frame = ANIMAL_data_frame %>% add_row(id = seq_along(1:n_piglet)+current_pig_id,
                                                                          farm_id = rep(nfarm,n_piglet), 
                                                                          demographic = rep(d_piglet,n_piglet), 
                                                                          status = rep(piglet_status,n_piglet),
                                                                          age = rep(0,n_piglet), 
                                                                          sex = sample(0:1,n_piglet,replace=T),
                                                                          next_d_date = rep(piglet_next_d_date,n_piglet),
                                                                          next_s_date = piglet_immunity_date,
                                                                          removal_date = day_piglet_mortality
                                                                          )
                        # UPDATE PIG ID
                        current_pig_id = current_pig_id + n_piglet
                       
                        
                        # record farrowing event
                        FARROW_EVENT = FARROW_EVENT %>% add_row(months = current_month, year = current_year)  
                  }
                  
                }
      #====FARROWING EVENT DONE===================================
              
     
                
                
      #===SOW PREGNANT EVENT=====================================
                temp_pregnant_data = temp_data %>% filter((farrow_date==-1*return_to_heat)|((-1*farrow_date)-return_to_heat)%%30==0)
                if(NROW(temp_pregnant_data)>0)
                    { 
                        for(animal in 1:NROW(temp_pregnant_data))
                        {
                          temp_id = temp_pregnant_data[animal,]$id
                          if(prob_mating!=1)
                          {
                            temp_mating = rbinom(1,1,prob_mating)
                            if(temp_mating==1)
                            {
                              ANIMAL_data_frame[ANIMAL_data_frame$id==temp_id,]$farrow_date = gestation_period
                            }
                          }
                          else
                          {
                              ANIMAL_data_frame[ANIMAL_data_frame$id==temp_id,]$farrow_date = gestation_period
                          }
                        }
                        
                    }
      
      #===SOW PREGNANT EVENT DONE================================
           
              
                
                
                  
      #====DEMOGRAPHIC AND DISEASE STATUS CHANGE=======================================
                temp_data = ANIMAL_data_frame %>% filter(farm_id == nfarm)
                temp_demographic_data = temp_data %>% filter(next_d_date == 0|next_s_date==0) #extract those changing demographic status
                if(NROW(temp_demographic_data)>0)
                    {
                      for(animal in 1:NROW(temp_demographic_data))
                      {
                        this_animal = temp_demographic_data[animal,]
                        temp_id = this_animal$id
                        temp_demographic = this_animal$demographic
                        temp_status = this_animal$status
                        temp_d_date = this_animal$next_d_date
                        temp_s_date = this_animal$next_s_date
                          if(temp_d_date==0)
                          {
                            # change demographic and assign new next_d_date
                            current_demographic = temp_demographic
                            if(current_demographic==d_piglet)
                            {
                              next_demographic = d_weaned
                              next_d_date = wean_to_grow
                              # UPDATE FARM TABLE
                              FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_PIGLET = FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_PIGLET -1
                              FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_WEANED = FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_WEANED + 1
                              # UPDATE ANIMAL TABLE
                              ANIMAL_data_frame[ANIMAL_data_frame$id==temp_id,]$demographic = next_demographic
                              ANIMAL_data_frame[ANIMAL_data_frame$id==temp_id,]$next_d_date = next_d_date
                              
                            }
                            else if(current_demographic==d_weaned)
                            {
                              next_demographic = d_fattening
                              next_d_date = 0
                              # UPDATE FARM TABLE
                              FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_WEANED = FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_WEANED -1
                              FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_FATTENING = FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_FATTENING + 1
                              # UPDATE ANIMAL TABLE
                              ANIMAL_data_frame[ANIMAL_data_frame$id==temp_id,]$demographic = next_demographic
                              ANIMAL_data_frame[ANIMAL_data_frame$id==temp_id,]$next_d_date = next_d_date
                              ANIMAL_data_frame[ANIMAL_data_frame$id==temp_id,]$removal_date = grow_to_finish
                            }
                            else if(current_demographic==d_gilt) # GILT BECOMING SOW
                            {
                              next_demographic = d_sow
                              next_d_date = 0 # @@@ check if needed to put next_d_date for gilts and need to include any events for sows
                              # UPDATE FARM TABLE
                              FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_GILT = FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_GILT -1
                              FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_FATTENING = FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_FATTENING + 1
                              # UPDATE ANIMAL TABLE
                              ANIMAL_data_frame[ANIMAL_data_frame$id==temp_id,]$demographic = next_demographic
                              ANIMAL_data_frame[ANIMAL_data_frame$id==temp_id,]$next_d_date = next_d_date
                              ANIMAL_data_frame[ANIMAL_data_frame$id==temp_id,]$removal_date = sample(first_farrow_cull_min:first_farrow_cull_max,1,replace=F)
                              ANIMAL_data_frame[ANIMAL_data_frame$id==temp_id,]$farrow_date = 1 # On the next day it'll farrow, then do farrowing events
                              ANIMAL_data_frame[ANIMAL_data_frame$id==temp_id,]$farrow_times = 0
                            }
                          } # changing demographic done
                        if(temp_s_date==0)
                        {
                          # changing disease status
                          current_status = temp_status
                          if(current_status==s_E)
                          {
                            next_status = s_I
                            next_s_date = day_infectious
                              # IF THIS FARM HAS NO INFECTED ANIMALS BEFORE, RECORD
                              if(FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_INFECTED==0)
                              {
                                FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$day_p_start = current_day
                              }
                            
                            # UPDATE FARM TABLE
                            FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_EXPOSED = FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_EXPOSED -1
                            FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_INFECTED = FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_INFECTED + 1
                            # UPDATE ANIMAL TABLE
                            ANIMAL_data_frame[ANIMAL_data_frame$id==temp_id,]$status = next_status
                            ANIMAL_data_frame[ANIMAL_data_frame$id==temp_id,]$next_s_date = next_s_date
                          }
                          else if(current_status==s_I)
                          {
                            next_status = s_R
                            next_s_date = round(-1*log(sample(1:100,1)/100)/(1/day_immunity_loss)) # get a random time to next event
                            # UPDATE FARM TABLE
                            FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_INFECTED = FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_INFECTED -1
                            FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_IMMUNE = FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_IMMUNE + 1
                            # UPDATE ANIMAL TABLE
                            ANIMAL_data_frame[ANIMAL_data_frame$id==temp_id,]$status = next_status
                            ANIMAL_data_frame[ANIMAL_data_frame$id==temp_id,]$next_s_date = next_s_date
                              # IF ALL INFECTED ANIMALS ARE GONE, RECORD
                                if(FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_INFECTED==0)
                                {
                                  
                                  diff = current_day - FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$day_p_start
                                  persistence_vector = c(persistence_vector,diff)
                                  persistence_farm_id = c(persistence_farm_id,nfarm)
                                }
                          }
                          else if(current_status==s_R)
                          {
                            next_status = s_S
                            next_s_date = 0
                            # UPDATE FARM TABLE
                            FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_IMMUNE = FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_IMMUNE -1
                            FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_SUSCEPTIBLE = FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_SUSCEPTIBLE + 1
                            # UPDATE ANIMAL TABLE
                            ANIMAL_data_frame[ANIMAL_data_frame$id==temp_id,]$status = next_status
                            ANIMAL_data_frame[ANIMAL_data_frame$id==temp_id,]$next_s_date = next_s_date
                          }
                        }
                      }
                      
                      
                    }
                    
                
                
      #====DEMOGRAPHIC AND DISEASE STATUS CHANGE DONE==================================
                
      

                
    
        
  
      
    }
    
#========GOING THROUGH EACH FARM FOR EVENTS DONE========================================#
   




  }
#========GOING THROUGH EACH DATE DONE====================================================#
#-------------------------------------------------------------------------------------------
    

# ERROR CHECK
head(FARM_data_frame,20)
nrow(FARM_data_frame[FARM_data_frame$N_PIGLET<0,])
nrow(FARM_data_frame[FARM_data_frame$N_WEANED<0,])
nrow(FARM_data_frame[FARM_data_frame$N_FATTENING<0,])
nrow(FARM_data_frame[FARM_data_frame$N_GILT<0,])
nrow(FARM_data_frame[FARM_data_frame$N_SOW<0,])
nrow(FARM_data_frame[FARM_data_frame$N_TOTAL<0,])
nrow(FARM_data_frame[FARM_data_frame$N_SUSCEPTIBLE<0,])
nrow(FARM_data_frame[FARM_data_frame$N_EXPOSED<0,])
nrow(FARM_data_frame[FARM_data_frame$N_INFECTED<0,])
nrow(FARM_data_frame[FARM_data_frame$N_IMMUNE<0,])
nrow(FARM_data_frame[FARM_data_frame$N_MDA<0,])
FARM_data_frame$day_p_start[!is.na(FARM_data_frame$day_p_start)]
summary(persistence_vector)
persistence_farm_id[persistence_vector==50]
FARM_data_frame[x,]
persistence_farm_x
immune_farm_x
immune_farm_x + persistence_farm_x
gilt_x
farrow_x
# this is quite long because of transmission parameter maybe too low