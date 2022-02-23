#=========================================================================#
#----------Mathematical model for within-farm swIAV transmission v1-------
#=========================================================================#
#****CHUNK 1
#*
library(tidyverse)
library(data.table)
library(profvis)
library(reshape)

set.seed(12345)
setwd("D://OneDrive - LSHTM//OneDrive - London School of Hygiene and Tropical Medicine//RF//swIAV_Modelling//Within_farm//swIAV_transmission_model")
source("functions_update_transmission.R")
source("Cambodia_pig_parameters.R")

# IMPORT USER DEFINE PARAMETRE
source("User_define_parameters_swIAV.R")

# profvis({

list_persistence_vector = vector("list",nrow(parameters))
list_persistence_farm = vector("list",nrow(parameters))

for(k in 1:nrow(parameters)){
  
  backyard_or_commercial = parameters[k,1]
  temp_typo = parameters[k,2]
  herd_size = parameters[k,3]
  mate_success = parameters[k,4]
  cull_cycle = parameters[k,5]
 
  r0 = parameters[k,6]
  beta_direct = r0/day_infectious # need to refine, density-dependent? Assuming R0 = 3, D = 6 then beta = beta_direct/N (now fequency-dependent)
  beta_indirect = beta_direct/24 # Cador 2017
 
# demographic and status: see above. sex: female 0 and male 1. age in months. 
# immunity_date: day until immunity dissapears. next_d_date: day until weaning. 
# infection_history: 0 not infected before. 1 infected before. Needs to consider multiple subtype maybe. 

# SETTING UP FARM_data_frame - now onlyconsidering farrow-to-finish but this needs to be updated
  prob_mating = mate_success
#=============SETTING UP BACKYARD FARM================================================================#
if(backyard_or_commercial==0)
{
  # Choosing varying parameters
 
  
  # PRE-DEFINE THE NUMBER OF ROWS 
  N_INITIAL_BACKYARD = ceiling(length_simulation_day/360*litter_size_max*2*n_backyard*herd_size*2) #// assuming 2 farrow/sow/year
    
  EMPTY_data_table = data.table(id = seq(1,N_INITIAL_BACKYARD), 
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
                                # infection_history = integer(),
                                farrow_times = integer(),
                                alive = integer()
                                
  ) 
  
  
  
  
  
  farm_id = seq_along(1:n_backyard)
  
  # DEFINING WHETHER SMALLHOLDERS INTRODUCE GILT
  # prop_introduce_gilt = 0.2
  # introduce_replace_gilt = rbinom(n_backyard,1,prop_introduce_gilt)
  # None introduced gilt
  introduce_replace_gilt = rep(0,n_backyard)
  
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
                               farm_type = integer(),
                               day_p_start = integer()
                               
  )
  
  
  # SOW_data_frame
  ## sows
  # SOW_data_frame = EMPTY_data_table
  
  
  ANIMAL_data_frame = copy(EMPTY_data_table)
  
  # FOR EACH FARM INITIALISE, ADD SOWS FIRST
  for(i in 1:n_backyard)
  {
    # temp_init = sample(1:max_h_size,1)
    temp_init = herd_size
    # UPDATE FARM_data_frame
    FARM_data_frame = FARM_data_frame[i,
                                      ':='(N_PIGLET=0,
                                           N_WEANED =0,
                                           N_FATTENING = 0,
                                           N_GILT = 0,
                                           N_SOW = temp_init,
                                           N_BOAR = 0,
                                           N_INFECTED = 0,
                                           N_IMMUNE = 0,
                                           N_MDA = 0,
                                           N_TOTAL = temp_init,
                                           farm_type = backyard_farm_type
                                           
                                      )]
    # FARM_data_frame[i,]$N_PIGLET = 0
    # FARM_data_frame[i,]$N_WEANED = 0
    # FARM_data_frame[i,]$N_FATTENING = 0                                            
    # FARM_data_frame[i,]$N_GILT = 0 
    # FARM_data_frame[i,]$N_SOW = temp_init 
    # FARM_data_frame[i,]$N_BOAR = 0 
    
    if(rbinom(1,1,prev_farm)==1) #why do we randomly choose initial status?
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
    # FARM_data_frame[i,]$N_INFECTED = 0
    # FARM_data_frame[i,]$N_IMMUNE = 0
    # FARM_data_frame[i,]$N_MDA = 0
    # FARM_data_frame[i,]$N_TOTAL = temp_init
    # THIS CAN BE DIFFERENT IF THE INITIAL CONDITION IS DIFFERENT
    
    init_sow_id= seq_along(1:temp_init) + current_pig_id
    current_pig_id = current_pig_id + temp_init
    ANIMAL_data_frame[init_sow_id, ":="(
                                                      farm_id = rep(i,length(init_sow_id)), 
                                                      demographic = rep(d_sow,length(init_sow_id)),
                                                      sex = rep(female,length(init_sow_id)), 
                                                      status = temp_status,
                                                      # TO DO: assign random number
                                                      farrow_times = rep(0,length(init_sow_id)),
                                                      next_s_date = temp_s_date,
                                                      alive = 1
    )]                                        
    
  }
  setDT(FARM_data_frame)
  # setkeyv(FARM_data_frame,c("farm_id"))
  
  # INITIAL CONDITION
  # SOW
  
  first_farrow = sample(as.numeric(as.Date('2020-08-01') - start_date),current_pig_id,replace=T)
  # if farrow_date >0 meaning that it's pregnant. 0 means it's empty

  # Better way is to pick up a random number of farrowing so far then assigne age.
  # But do we need to keep track of age? Maybe not. Once farrow, just need to capture farrow times
  
  temp_farrow_times = sample(0:(cull_cycle-1),current_pig_id,replace=T)
  # init_sow_age = farrow_times*150 
  # 
  # init_sow_age = init_sow_age + init_sow_age_min
  # Assign the number of farrows
  
  
  
  # sow_mortality_date governs the time at which sows are removed. 
  # after a set cycle of farrowing, sows are removed
  # This can consider a max number of conception failure to be allowed e.g. failing 3 times leads to cull
  # Culling date should be determined after giving 8th farrow
  #SOW_data_frame$immunity_date = 0
  # Here random death should be considered. Mortality of 4.3%
  # To get when sows die, cull_cycle - farrow_times then times 150 (this is the remaining life expectancy)
  
  sow_mortality_date = rbinom(current_pig_id,1,sow_mortality)
  position = seq(1:current_pig_id)[sow_mortality_date>0]
  remaining_time = (cull_cycle - temp_farrow_times[position])*150
  random_time = sapply(remaining_time,function(x){sample(1:x,1)})
  sow_mortality_date[position] = random_time
  # sample 
  # if it's 1, then replace it by a value representing remaining age

  # sow_mortality_date = (sample(cull_mo_min:cull_mo_max,current_pig_id,replace=T) - init_sow_age)*30
  
  ANIMAL_data_frame[1:current_pig_id, ":=" (
    # age = init_sow_age,
                                                farrow_date = first_farrow,
                                                farrow_times = temp_farrow_times,
                                                removal_date = sow_mortality_date,
                                                next_d_date = 0  # when next_d_date >0 means it's milking (sows) or being milked (piglet)
                                                )] 
  
  # APPEND EMPTY ROWS TO ANIMAL_data_frame
  
  setDT(ANIMAL_data_frame)
  # setkeyv(ANIMAL_data_frame,c("id","farm_id"))
  # # DATA FRAME
  #     PIGLET_data_frame = WEANER_data_frame = FATTEN_data_frame = GILT_data_frame = EMPTY_data_table
  
  
  # # REMOVE_data_frame FOR BACKYARD
  # REMOVE_data_frame = EMPTY_data_table
  # REMOVE_data_frame[, ":=" (r_reason = NA,
  #                           current_day= NA)]
}


#========= ADD COMMERCIAL FARMS IF NECESSARY===========================================================
# WONDERING IF BETTER TO TREAT ONE BATCH AS ONE GROUP
# WHAT'S THE EFFECT OF BATCH SIZE - 
# ALSO DO WE NEED EACH INDIVIDUAL ANIMAL DATA? PROBABLY NOT. JUST RECORD ROW FOR BATCH, COLUMN FOR STATUS
# MULTIPLE BATCHES CAN EXIST IN THE SAME ROOM - IF IN SAME UNIT AND SAME DAY UNTIL NEXT D, THEN SAME ROOM
# REALLY EASY TO USE IBM FRAMEWORK
if(backyard_or_commercial==1)
{
  
  # DEFINE DATA STRUCTURES FOR COMMERCIAL FARMS
  COM_FARM_data_frame = data.table(farm_id = integer(),
                                   unit_id = integer(), # ID TO GO THROUGH (UNIT IS BUILDING)
                                   room_id = integer(), # ROOMS IN THE UNIT
                                   unique_room_id = integer(), # THIS IS DISTINCT ID
                                   farm_type = integer(),# FF, PIGLET OR FATTENING
                                   stage = integer(), # INDICATES PRODUCTION STAGE AS BELOW
                                   N_INFECTED_ROOM = integer(), # NUMBER OF INFECTED ANIMALS IN THE ROOM
                                   N_INFECTED_UNIT = integer(), # NUMBER OF INFECTED ANIMALS IN THE UNIT
                                   # N_INFECTED_ROOM_OTHER = integer(), # NUMBER OF INFECTED ANIMALS IN OTHER ROOMS IN THIS UNIT
                                   N_TOTAL_ROOM = integer(),
                                   N_TOTAL_UNIT = integer(),
                                   N_SUSCEPTIBLE_ROOM = integer(),
                                   N_EXPOSED_ROOM = integer(),
                                   N_IMMUNE_ROOM = integer(),
                                   day_p_start = integer(),# need to think on which level to record persistence farm, unit, room
                                   # batch_size = integer(), # do I need this?
                                   next_d_date = integer(), # DEMOGRAPHIC CHANGE OCCURRS AT ROOM LEVEL
                                   lambda = numeric() # FORCE OF INFECTION
                                   
  )
  
  EMPTY_COM_ANIMAL_data_frame = data.table(farm_id = integer(),
                                           unit_id = integer(),
                                           # batch_id = integer(),
                                           room_id = integer(),
                                           unique_room_id = integer(),
                                           animal_id = integer(),
                                           status = integer(),
                                           next_s_date = integer(),
                                           # next_d_date = integer(), # DEMOGRAPHIC EVENT OCCURS AT RROM LEVEL, NEED TO ADD removal_date for individual's death
                                           farrow_times = integer()
  )

  # FIRST ADD FF FARMS
  if(n_commercial_FF>0)
  {
    intro_interval_week = rep(1,1) # What's the interval between gilt introduction? Set vector
    for(i in 1:n_commercial_FF)
    {
      b_vec = c(2,2) # BATCH SIZE TO CHOOSE FROM
      temp_batch_size = sample(b_vec,1) # change batch size accordingly
      temp_num_unit = 5
      temp_num_room = (day_gilt_development+day_breeding_room+day_farrow_room+day_weaned_sow+day_growing_room)/7
      # UPDATE COM_FARM_data_frame
      COM_FARM_data_frame = COM_FARM_data_frame %>% add_row(
        farm_id = rep(current_farm_id,temp_num_room), # 5 unit for FF farm
        unit_id =  c(rep(1+current_unit_id,day_gilt_development/7),
                     rep(2+current_unit_id,day_breeding_room/7),
                     rep(3+current_unit_id,(day_farrow_room)/7),
                     rep(4+current_unit_id,day_weaned_sow/7),
                     rep(5+current_unit_id,day_growing_room/7)
        ),
        room_id = c(seq(1,day_gilt_development/7),
                    seq(1,day_breeding_room/7),
                    seq(1,(day_farrow_room)/7),
                    seq(1,day_weaned_sow/7),
                    seq(1,day_growing_room/7)
        ),
        unique_room_id = seq_along(1:temp_num_room) + current_unique_room_id,
        farm_type = rep(farm_type_FF,temp_num_room),
        stage = c(rep(1,day_gilt_development/7),
                  rep(2,day_breeding_room/7),
                  rep(3,(day_farrow_room)/7),
                  rep(4,day_weaned_sow/7),
                  rep(5,day_growing_room/7)
        ),
        N_INFECTED_ROOM = rep(0,temp_num_room),
        N_INFECTED_UNIT = rep(0,temp_num_room),
        N_TOTAL_UNIT =  c(rep(day_gilt_development/7,day_gilt_development/7),
                          rep(day_breeding_room/7,day_breeding_room/7),
                          rep(day_farrow_room/7,day_farrow_room/7),
                          rep(day_weaned_sow/7,day_weaned_sow/7),
                          rep(day_growing_room/7,day_growing_room/7)
        ),
         # CURRENTLY IGNORING PIGLET - IF STARTS WITH INFECTED CONDITION THEN MAY NEED TO ADD THEM IN N_TOTAL_ROOM etc
        N_TOTAL_ROOM =  rep(1,temp_num_room),
        N_SUSCEPTIBLE_ROOM = rep(1,temp_num_room),
        N_EXPOSED_ROOM = rep(0,temp_num_room),
        N_IMMUNE_ROOM =  rep(0,temp_num_room),
        next_d_date = c(
          seq(day_gilt_development,7,by=-7), # Gilt development
          seq(day_breeding_room,7,by=-7), # Breeding room
          seq(day_farrow_room1+day_farrow_room2,7,by=-7), # Farrowing room, wnen next_d_tate hits 0 and moving to farrow room, decide piglet status depending on dam status
          seq(day_weaned_sow,7,by=-7), # Weaned sow room
          seq(day_growing_room,7,by=-7) # Growing room
        )
       
        
        
      )
      
      # ADD ANIMALS (THIS IS ACTUALLY BATCH RATHER THAN ANIMAL)
      # FIRST COUNT HOW MANY BATCHES TOTAL
      temp_num_animal = (day_gilt_development+day_breeding_room+day_farrow_room+day_weaned_sow+day_growing_room)/7
      
      COM_ANIMAL_data_frame = EMPTY_COM_ANIMAL_data_frame %>% add_row(
        
        farm_id = rep(current_farm_id,temp_num_animal), 
        unit_id = c(rep(current_unit_id+1,day_gilt_development/7),
                    rep(current_unit_id+2,day_breeding_room/7),
                    rep(current_unit_id+3,day_farrow_room/7),
                    rep(current_unit_id+4,day_weaned_sow/7), #unit 4 and 2 are in the same building
                    rep(current_unit_id+5,day_growing_room/7)
        ),
        animal_id = seq_along(1:temp_num_animal) + current_pig_id,
        room_id = c(seq(1,day_gilt_development/7),
                    seq(1,day_breeding_room/7),
                    seq(1,(day_farrow_room)/7),
                    seq(1,day_weaned_sow/7),
                    seq(1,day_growing_room/7)
        ),
        unique_room_id = seq_along(1:temp_num_animal) + current_unique_room_id, # ONLY VALID WHEN 1 ANIMAL IN 1 ROOM
        status = rep(s_S,temp_num_animal),
        next_s_date = rep(0,temp_num_animal),
     
        farrow_times = c(rep(0,(day_gilt_development+day_breeding_room)/7),
                         rep(1,(day_farrow_room1+day_farrow_room2+day_weaned_sow)/7),
                         rep(0,(day_growing_room)/7)
        )
        
      ) # INITILIZE COM_ANIMAL_data_frame DONE
      
      # UPDATE farm_id
      current_farm_id = current_farm_id + 1
      current_unit_id = current_unit_id + 5
      current_pig_id = current_pig_id + temp_num_animal
      current_unique_room_id = current_unique_room_id + temp_num_room
      
    }
  } # IF COMMERCIAL_FF EXISTS. FINISHED
  if(n_commercial_PIGLET>0)
  {
    # ADD COMMERCIAL PIGLET FARM
  }
  if(n_commercial_FATTENING>0)
  {
    # ADD COMMERCIAL FATTENING FARM DONE
  }
  REMOVE_COM_data_frame = EMPTY_COM_ANIMAL_data_frame
}



#==========ADDING COMMERCIAL FARMS DONE========================================================================          





# EVENT table
FARROW_EVENT = data.table(months=integer(),year=integer())

# LENGTH OF PERSISTENCE
# maybe just add into vector and record farm id in another vector?
# first day of having S till no S exists
persistence_vector = c()
persistence_farm_id = c()
# persistence_farm_x = c()
immune_farm_x = c()
gilt_x = c()
farrow_x = c()
# x = 46
#****CHUNK 1 DONE



COM_persistence = c()

#=================================================================================


#=========START SIMULATION==========================================================
while(current_day<length_simulation_day)
{
  #=========GOING THROUGH EACH DATE=====================================================
  # UPDATE DAY AND COUNT OF EACH EVENT
  current_day = current_day + 1
  current_month = (current_day %/% 30 + 5) %% 12 + 1  # it's 5 because the simulation starts in May
  current_year = current_day %/% 365
  # persistence_farm_x = c(persistence_farm_x,FARM_data_frame[x,]$N_INFECTED)
  # immune_farm_x = c(immune_farm_x,FARM_data_frame[x,]$N_IMMUNE)
  
  # Below obsolete as no seasonality of mating is considered
  # Mating happens less between May and September
  # if(current_month>=5 & current_month <= 8) 
  # {
  #   prob_mating = mate_success
  # }
  # else if(current_month>=9 & current_month <= 10)
  # {
  #   prob_mating = mate_success
  # }
  # else
  # {
  #   prob_mating = mate_success
  # }
  # UPDATE DATE FOR BACKYARD

  
#===============BACKYARD=============================================

  
if(backyard_or_commercial==0)
{
  if(NROW(ANIMAL_data_frame)>0)
  {
    # SUBTRACT 1 FROM each variable that describes dates
    ANIMAL_data_frame[1:current_pig_id,":="(
      next_s_date = next_s_date - 1,
      next_d_date = next_d_date -1,
      farrow_date = farrow_date -1,
      removal_date = removal_date -1
    )]
  }
  #============GOING THROUGH EACH BACKYARD FOR EVENTS===================================#
  # Calculate the number of new infections beta*S*I, IF S > 0
  if(NROW(FARM_data_frame)>0)
  {
  #=============UPDATE TRANSMISSIOn EVENT===============================================================
    new_inf_count = mapply(calc_new_infection, FARM_data_frame[,N_INFECTED],FARM_data_frame[,N_TOTAL],FARM_data_frame[,N_SUSCEPTIBLE],MoreArgs = list(beta_direct))
    
    # IF AT LEAST ONE TRANSMISSION OCCURS
    if(sum(new_inf_count)>0)
    {
      # THEN UPDATE FARM_data_frame
      FARM_data_frame[,
                      ":="(N_EXPOSED = N_EXPOSED + new_inf_count,
                           N_SUSCEPTIBLE = N_SUSCEPTIBLE - new_inf_count
                      )]
      # GET THE VECTOR OF FARM ID THAT NEEDS UPDATE
      farm_id_to_update = seq_along(1:FARM_data_frame[,max(farm_id)])[new_inf_count>0]
      new_inf_use = new_inf_count[new_inf_count>0]
      
      animal_id_update = as.vector(unlist(mapply(update_infection_backyard,farm_id_to_update,new_inf_use,MoreArgs  = list(ANIMAL_data_frame))))
      
      ANIMAL_data_frame[id %in% animal_id_update,
                        ":="(status = s_E,
                             next_s_date = day_latent
                        )]
      
    }
    
    
   
    
  
    
   
  #======================================================================================================
    # THEN VECTORIZE THE FUNCTION TO REMOVE/UPDATE STATUS ETC, OR AT LEAST VISIT ONLY FARMS THAT ARE RELEVANT
    
    # TO DO:
    # PUT REMOVE AS THE LAST EVENT AFTER DOING ALL UPDATES
    # VECTORIZE SOME EVENT UPDATE
    
    
#*******************************************************************************************************    
#                                         FARROWING EVENT         
#*********************************************************************************************************
#*
#*        
    # temp_data = ANIMAL_data_frame %>% filter(farm_id == nfarm)
    # temp_farrowing_data = temp_data %>% filter(farrow_date==0)
    # setkey(TEMP_ANIMAL_data_frame,farrow_date)
    
    TEMP_ANIMAL_data_frame1 = na.omit(ANIMAL_data_frame,cols="farm_id")
    # TEMP_FARROW_id = TEMP_ANIMAL_data_frame1[farrow_date==0,.N,by=farm_id][,farm_id]
    
    # USED TO RETRIEVE farm_type from ANIMAL_data_frame
    # But realistically at this moment we don't simulate different types in the same run, so already specify
    # TEMP_FARROW_dat = TEMP_ANIMAL_data_frame1[farrow_date==0,c("id","farm_id","status","farm_type","farrow_times")]
    TEMP_FARROW_dat = TEMP_ANIMAL_data_frame1[farrow_date==0,c("id","farm_id","status","farrow_times")]
    
    # TEMP_ANIMAL_data_frame = TEMP_ANIMAL_data_frame1[farm_id==nfarm]
    temp_nrow = NROW(TEMP_FARROW_dat)
    if(temp_nrow>0)
    {
      # RANDOM SAMPLING OF VALUES FOR ALL FARROWING ANIMALS
      # DETERMINE HOW MANY PIGLETS
      # if this varies over typology, need to change below
      
      n_piglet_vec = sample(litter_size_min:litter_size_max,temp_nrow,replace=T)
      n_total_piglet = sum(n_piglet_vec)
      # DETERMINE HOW MANY PIGLET DIES WHEN IN THE FIRST WEEK
      n_piglet_mortality_vec = sample(num_die_piglet_min:num_die_piglet_max,temp_nrow,replace=T)
      # DETERMINE next_d_date
      # piglet_next_d_date = sample(wean_day_min:wean_day_max,temp_nrow,replace=T)
      
      # ANIMAL_data_frame[ANIMAL_data_frame$id==temp_id,]$next_d_date = piglet_next_d_date # WEANING DATE
      ANIMAL_data_frame[id %in% TEMP_FARROW_dat[,id],":="(
        farrow_times = farrow_times+1,
        next_d_date = wean_day)]
      
      # WHEN farrow_times reaches = cull_cylce then add culling event TO DO
      
      
      # PREPARE EMPTY VECTOR TO CONTAIN PIGLET INFO
      day_piglet_mortality = vector(mode="numeric",length=n_total_piglet)
      piglet_immunity_date = vector(mode="numeric",length=n_total_piglet)
      piglet_status = vector(mode="numeric",length=n_total_piglet)
      piglet_farm_id = vector(mode="numeric",length=n_total_piglet)
      piglet_next_d_date_piglet = vector(mode="numeric",length=n_total_piglet)
      piglet_sex = vector(mode="numeric",length=n_total_piglet)
      piglet_demographic = vector(mode="numeric",length=n_total_piglet)
      current_piglet = 1
      # NOW VISIT EACH FARROWING PIGS IN THE LIST
      for(i in 1:temp_nrow)
      {
  #=============  ASSIGN PIGLET INFORMATION TO EACH FARROW   ================================#
        temp_sow_id = TEMP_FARROW_dat[i,id]
        temp_status = TEMP_FARROW_dat[i,status]
        nfarm =  TEMP_FARROW_dat[i,farm_id]
        n_piglet = n_piglet_vec[i]
        # temp_typo = TEMP_FARROW_dat[i,farm_type]
        temp_farrow = TEMP_FARROW_dat[i,farrow_times]
        # assign sex for new piglets
        temp_sex = sample(0:1,n_piglet,replace=T)
        temp_day_piglet_mortality = vector(mode="numeric",length=n_piglet)
        # if selling weaners, set the removal
        # if this farm is not keeping weaners
        # but also if this is the last farrowing they keep weaners
        temp_piglet_demographic = rep(d_piglet,n_piglet)
        temp_num_death = n_piglet_mortality_vec[i]
        # --------SETTING REMOVAL DATE-----------------------
        # first assign death date for those died during piglet
        # Many of these parts are added on Feb 2022 to accommodate the status of Cambodian smallholder
        temp_death_date = sample(1:wean_day,temp_num_death,replace=T)
        # Prepare piglet sex that won't die before weaning so that weaners can be kept if needed
        temp_sex_nodeath = temp_sex[-(1:length(temp_death_date))]
        if(temp_typo == t_breed) # if HHs are Breeding
        {
          temp_nodeath_date = rep(wean_day,n_piglet-temp_num_death)
          if(temp_farrow==cull_cycle)
          {
           
            # TO DO need to decide how many weaners to keep
            if(sum(temp_sex_nodeath) < length(temp_sex_nodeath))
            {
              # if there are female
              # first female stays on farm as replacement
              female_position1 = seq(1:length(temp_sex_nodeath))[temp_sex==female][1]
              temp_nodeath_date[female_position1] = -1 # indicate this will be kept for weaner
              temp_piglet_demographic[temp_num_death+female_position1] = d_replace_piglet
              
            }
            # SET CULLING DATE FOR THIS SOW EVEN IF REPLACEMENT IS NOT SCECURED
            ANIMAL_data_frame[id == temp_sow_id, removal_date := wean_day]
            
          }
        }else{
          temp_nodeath_date = rep(-1,n_piglet-n_piglet_mortality_vec[i]) # if not Breeding, not removed
          
        }
        # COMBINE DATES FOR PIGLETS DIE AND THOSE NOT DIE
        temp_day_piglet_mortality = c(temp_death_date,temp_nodeath_date)
        day_piglet_mortality[current_piglet:(current_piglet+n_piglet-1)] = temp_day_piglet_mortality
        piglet_sex[current_piglet:(current_piglet+n_piglet-1)] = temp_sex
       
        
        
        
        # Because not all piglets are dying, indicate removeal_date = 0 if they don't die while piglets
    #-----------  DETERMINE IF MDA IS PASSED DEPENDING ON THE STATUS OF SOW  -----------------------------------------------
        # 
        piglet_farm_id[current_piglet:(current_piglet+n_piglet-1)] = nfarm
        # Assigning the next demographic event date: obsolete, not any more random weaning date. Fixed. 
        # piglet_next_d_date_piglet[current_piglet:(current_piglet+n_piglet-1)] = piglet_next_d_date[i]
        piglet_next_d_date_piglet[current_piglet:(current_piglet+n_piglet-1)] = wean_day + 1
          # By adding 1, ease the computation by not changing their demographic status if they are removed on the same day
        
        
        if(temp_status==s_R)
        {
          piglet_status[current_piglet:(current_piglet+n_piglet-1)] = s_mda
          piglet_immunity_date[current_piglet:(current_piglet+n_piglet-1)] = round(rgamma(n_piglet,shape=shape_gamma_mda,scale=scale_gamma)) #sample from gamma
          # UPDATE FARM TABLE
          FARM_data_frame[nfarm,
                          ":="(N_PIGLET = N_PIGLET + n_piglet,
                               N_IMMUNE = N_IMMUNE + n_piglet,
                               N_TOTAL = N_TOTAL + n_piglet
                               
                          )]
        }
        else # dam not immunity
        {
          piglet_status[current_piglet:(current_piglet+n_piglet-1)] = s_S
          piglet_immunity_date[current_piglet:(current_piglet+n_piglet-1)] = rep(-1,n_piglet)
          # UPDATE FARM TABLE
          FARM_data_frame[nfarm,
                          ":="(N_PIGLET = N_PIGLET + n_piglet,
                               N_SUSCEPTIBLE = N_SUSCEPTIBLE + n_piglet,
                               N_TOTAL = N_TOTAL + n_piglet
                               
                          )]
        }
    #------------FINISH PIGLET IMMUNE STATUS---------------------------------------------------------------
        current_piglet = current_piglet+n_piglet
      }
 #=============    FINISH VISITING EACH FARROW  ======================================================
      # FINISHED PUTTING INFOR FOR PIGLET
      # ADD NEW BORN TO PIGLET DATAFRAME
      
      ANIMAL_data_frame[id %in% (seq_along(1:n_total_piglet)+current_pig_id),
                        ":="(
                          farm_id = piglet_farm_id, 
                          demographic = rep(d_piglet,n_total_piglet), 
                          status = piglet_status,
                          # age = rep(0,n_total_piglet), 
                          # TO DO need to determine sex earlier
                          # sex = sample(0:1,n_total_piglet,replace=T),
                          sex = piglet_sex,
                          next_d_date = piglet_next_d_date_piglet,
                          next_s_date = piglet_immunity_date,
                          removal_date = day_piglet_mortality
                        )]
      # UPDATE PIG ID
      current_pig_id = current_pig_id + n_total_piglet
      
      
    }
    # record farrowing event
    FARROW_EVENT = FARROW_EVENT %>% add_row(months = current_month, year = current_year) 
    #====FARROWING EVENT DONE===================================    
        
     
    
#=================  SOW PREGNANT EVENT  =====================================
    temp_pregnant_data = TEMP_ANIMAL_data_frame1[((farrow_date==-1*return_to_heat)|((-1*farrow_date)-return_to_heat)%%30==0),c("id","status","farm_id")] 
    if(nrow(temp_pregnant_data)>0)
    { 
      temp_mating = rbinom(nrow(temp_pregnant_data),1,prob_mating)
      temp_pregnant_id = temp_pregnant_data[,id]
      id_to_update = temp_pregnant_id[temp_mating==1]
      
      temp_inf_status = temp_pregnant_data[,status]
      temp_sow_infection = rbinom(nrow(temp_pregnant_data),1,prev_IAV_boar)
      temp_position = (temp_sow_infection==1 & temp_inf_status == s_S)
      
      temp_id_infected = temp_pregnant_id[temp_position]
      if(length(temp_id_infected)>0)
      {
        # Update farm record of infection
        temp_farm_id = temp_pregnant_data[,farm_id]
        temp_farm_id_infected_all = temp_farm_id[temp_position]
        temp_count_farm_num = temp_farm_id_infected_all %>% tibble::as_tibble() %>% dplyr::count(value)
        temp_farm_id_infected = temp_count_farm_num %>% pull(value)
        temp_exp_n =  temp_count_farm_num %>% pull(n)
        
        # Update infection
        ANIMAL_data_frame[id %in% temp_id_infected,
                          ":="(status = s_E,
                               next_s_date = day_latent
                          )]
        
        # UPDATE farm info
        FARM_data_frame[farm_id %in% temp_farm_id_infected,
                        ":="(N_EXPOSED = N_EXPOSED + temp_exp_n,
                             N_SUSCEPTIBLE = N_SUSCEPTIBLE - temp_exp_n
                        )]
      }
     
     # 
      # SOME SOWS DO NOT GET MATING IF THEY KEEP FAILING
      # AT THIS EVENT, DISEASE TRANSMISSION CAN HAPPEN
      # SOME GET INFECTION RANDOMLY DEPENDING ON THE PREVALENCE
      # CAN TRY DIFFERENT LEVELS OF PREVALENCE IN BOARS AND SEE IF BOAR OR REPLACEMENT GILT IS MORE IMPORTANT
      
      # Update pregnancy
      ANIMAL_data_frame[id %in% id_to_update,farrow_date := gestation_period]
     
      
     
    
    }
    
#===                SOW PREGNANT EVENT DONE               ================================
    
    
    
    
    
#====           DEMOGRAPHIC AND DISEASE STATUS CHANGE         =======================================
    temp_demographic_data = TEMP_ANIMAL_data_frame1[(next_d_date == 0|next_s_date==0),c("id","farm_id","demographic","status","next_d_date","next_s_date")]
    if(NROW(temp_demographic_data)>0)
    {
      temp_id_vec = temp_demographic_data[,id]
      temp_farm_id_vec = temp_demographic_data[,farm_id]
      for(animal in 1:NROW(temp_demographic_data))
      {
        # this_animal = temp_demographic_data[animal,]
        temp_id = temp_id_vec[animal]
        temp_farm_id = temp_farm_id_vec[animal]
       
#=============   DEMOGRAPHIC STATUS CHANGE  =====================================================#
        
        if(temp_demographic_data[animal,next_d_date]==0)
        {
          # change demographic and assign new next_d_date
          current_demographic = temp_demographic_data[animal,demographic]
          
    #===========    PIGLET BECOMES WEANERS ===========================================
          if(current_demographic==d_piglet)
          {
            tem_next_d_date = wean_to_grow
            # UPDATE FARM TABLE
            FARM_data_frame[temp_farm_id,
                            ":="(N_PIGLET = N_PIGLET -1,
                                 N_WEANED = N_WEANED + 1
                            )]
            # FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_PIGLET = FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_PIGLET -1
            # FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_WEANED = FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_WEANED + 1
            # UPDATE ANIMAL TABLE
            ANIMAL_data_frame[id==temp_id,
                              ":="(demographic = d_weaned,
                                   next_d_date = tem_next_d_date
                              )]
            # ANIMAL_data_frame[ANIMAL_data_frame$id==temp_id,]$demographic = next_demographic
            # ANIMAL_data_frame[ANIMAL_data_frame$id==temp_id,]$next_d_date = next_d_date
            
          }
    #============     REPLACEMENT PIGLET BECOMES REPLACEMENT WEANER  ====================
          else if(current_demographic==d_replace_piglet)
          {
            # TO DO fill here becoming
            # Changing status of replacement piglet to replacement weaner
            tem_next_d_date = wean_to_grow
            # UPDATE FARM TABLE
            FARM_data_frame[temp_farm_id,
                            ":="(N_PIGLET = N_PIGLET -1,
                                 N_WEANED = N_WEANED + 1
                            )]
            # FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_PIGLET = FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_PIGLET -1
            # FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_WEANED = FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_WEANED + 1
            # UPDATE ANIMAL TABLE
            ANIMAL_data_frame[id==temp_id,
                              ":="(demographic = d_replace_weaned,
                                   next_d_date = tem_next_d_date
                              )]
          }
    #============   WEANER BECOMES GROWERS  ==============================================
          else if(current_demographic==d_weaned)
          {
            tem_next_d_date = 0
            # UPDATE FARM TABLE
            FARM_data_frame[temp_farm_id,
                            ":="(N_WEANED = N_WEANED -1,
                                 N_FATTENING = N_FATTENING + 1
                                 
                            )]
            # UPDATE ANIMAL TABLE
            ANIMAL_data_frame[id==temp_id,
                              ":="(demographic = d_fattening,
                                   next_d_date = tem_next_d_date,
                                   removal_date = grow_to_finish
                              )]
          }
    #=============   REPLACEMENT WEANER BECOMES GILT ====================================
          else if(current_demographic==d_replace_weaned)
          {
            # TO DO check what's the setting for gilt?
            tem_next_d_date = grow_to_first_farrow
            # UPDATE FARM TABLE
            FARM_data_frame[temp_farm_id,
                            ":="(N_WEANED = N_WEANED -1,
                                 N_GILT = N_GILT + 1
                                 
                            )]
            # UPDATE ANIMAL TABLE
            ANIMAL_data_frame[id==temp_id,
                              ":="(demographic = d_gilt,
                                   next_d_date = tem_next_d_date
                              )]
          }
    #===========   GILT BECOMES SOWS   =================================================
          # This new sow farrows on the next day. Assign removal dates as appropriate
          else if(current_demographic==d_gilt) # GILT BECOMING SOW
          {
            tem_next_d_date = 0 # @@@ check if needed to put next_d_date for gilts and need to include any events for sows
            # UPDATE FARM TABLE
            FARM_data_frame[temp_farm_id,":="(N_GILT = N_GILT - 1,
                                       N_SOW = N_SOW +1)]
            
            # REMOVAL DATE
            temp_sow_mortality_date = rbinom(1,1,sow_mortality)
            if(temp_sow_mortality_date==1){
              # if this animal dies before full cycle
              temp_sow_mortality_date = sample(2:(cull_cycle-1)*150,1)
            }else
            {
              temp_sow_mortality_date = -1
            }
           
            # UPDATE ANIMAL TABLE
            ANIMAL_data_frame[id==temp_id,
                              ":="(demographic = d_sow,
                                   next_d_date = tem_next_d_date,
                                   # removal_date = sample(first_farrow_cull_min:first_farrow_cull_max,1,replace=F),
                                   removal_date = temp_sow_mortality_date,
                                   farrow_date = 1, # when becoming sow, next day it farrows
                                   farrow_times = 0
                              )]
            # ANIMAL_data_frame[ANIMAL_data_frame$id==temp_id,]$demographic = next_demographic
            # ANIMAL_data_frame[ANIMAL_data_frame$id==temp_id,]$next_d_date = next_d_date
            # ANIMAL_data_frame[ANIMAL_data_frame$id==temp_id,]$removal_date = sample(first_farrow_cull_min:first_farrow_cull_max,1,replace=F)
            # ANIMAL_data_frame[ANIMAL_data_frame$id==temp_id,]$farrow_date = 1 # On the next day it'll farrow, then do farrowing events
            # ANIMAL_data_frame[ANIMAL_data_frame$id==temp_id,]$farrow_times = 0
          }
       
        } # changing demographic done
        
        
#============    DISEASE STATUS CHANGE  ==================================================
        
        if( temp_demographic_data[animal,next_s_date]==0)
        {
                              # print("S date event") # Error check
          # changing disease status
          current_status = temp_demographic_data[animal,status]
                              # print(paste0("status is ", current_status)) # Error check
          if(current_status==s_E)
          {
                               # print("S E event") # Error check
            
            # IF THIS FARM HAS NO INFECTED ANIMALS BEFORE, RECORD
            if(FARM_data_frame[temp_farm_id,N_INFECTED]==0)
            {
              FARM_data_frame[temp_farm_id,day_p_start:=current_day] 
              # print(paste0("farm is", temp_farm_id, "Day p is", FARM_data_frame[temp_farm_id,day_p_start]))
            }
            
            # UPDATE FARM TABLE
            FARM_data_frame[temp_farm_id,
                            ":="(N_EXPOSED = N_EXPOSED -1,
                                 N_INFECTED = N_INFECTED + 1
                            )]
            # FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_EXPOSED = FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_EXPOSED -1
            # FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_INFECTED = FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_INFECTED + 1
            # UPDATE ANIMAL TABLE
            ANIMAL_data_frame[id==temp_id,
                              ":="(status = s_I,
                                   next_s_date = day_infectious
                              )]
            # ANIMAL_data_frame[ANIMAL_data_frame$id==temp_id,]$status = next_status
            # ANIMAL_data_frame[ANIMAL_data_frame$id==temp_id,]$next_s_date = next_s_date
          }
          else if(current_status==s_I)
          {
                                                                                # print("S I event") # Error check
            
            next_status = 
            # next_s_date = round(-1*log(sample(1:100,1)/100)/(1/day_immunity_loss)) # get a random time to next event
            temp_next_s_date =  round(rgamma(1,shape=shape_gamma_immunity,scale=scale_gamma)) #sample from gamma
            # UPDATE FARM TABLE
            FARM_data_frame[temp_farm_id,
                            ":="(N_INFECTED = N_INFECTED -1,
                                 N_IMMUNE = N_IMMUNE + 1
                                 
                            )]
            # FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_INFECTED = FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_INFECTED -1
            # FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_IMMUNE = FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_IMMUNE + 1
            # UPDATE ANIMAL TABLE
            ANIMAL_data_frame[id==temp_id,
                              ":="(status = s_R,
                                   next_s_date = temp_next_s_date
                              )]
            # ANIMAL_data_frame[ANIMAL_data_frame$id==temp_id,]$status = next_status
            # ANIMAL_data_frame[ANIMAL_data_frame$id==temp_id,]$next_s_date = next_s_date
            # IF ALL INFECTED ANIMALS ARE GONE, RECORD
            if(FARM_data_frame[temp_farm_id,N_INFECTED]==0)
            {
              
              diff_days = current_day - FARM_data_frame[temp_farm_id,day_p_start]
                                                                                   # print(paste0("farm is ", temp_farm_id,"Day p is ",FARM_data_frame[temp_farm_id,day_p_start], "Diff is ", diff_days))
              
              persistence_vector = c(persistence_vector,diff_days)
              persistence_farm_id = c(persistence_farm_id,temp_farm_id)
            }
          }
          else if(current_status==s_R)
          {
            
            # UPDATE FARM TABLE
            FARM_data_frame[temp_farm_id,
                            ":="(N_IMMUNE = N_IMMUNE - 1,
                                 N_SUSCEPTIBLE = N_SUSCEPTIBLE + 1
                            )]
            # FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_IMMUNE = FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_IMMUNE -1
            # FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_SUSCEPTIBLE = FARM_data_frame[FARM_data_frame$farm_id==nfarm,]$N_SUSCEPTIBLE + 1
            # UPDATE ANIMAL TABLE
            ANIMAL_data_frame[id==temp_id,
                              ":="(status = s_S,
                                   next_s_date = 0
                              )]
            # ANIMAL_data_frame[ANIMAL_data_frame$id==temp_id,]$status = next_status
            # ANIMAL_data_frame[ANIMAL_data_frame$id==temp_id,]$next_s_date = next_s_date
          }
        }
      }
      
      
    }
    
    
    
    #====DEMOGRAPHIC AND DISEASE STATUS CHANGE DONE==================================    
        
 
    # CREATE removal_date = 0 data
    TEMP_ANIMAL_data_frame1 = na.omit(ANIMAL_data_frame,cols="farm_id")
    TEMP_REMOVAL_id = TEMP_ANIMAL_data_frame1[removal_date==0,.N,by=farm_id][,farm_id]
    # setkey(TEMP_REMOVAL,farm_id)
    # CREATE farrow_date = 0 data
   

    # LIST UP ONLY FARMS THAT HAVE REMOVAL EVENT THEN REMOVE
    # NEXT LIST UP FARROW, PREGNANT, DEMOGRAPHIC/STATUS CHANGE EVENT
    # REALLY NO NEED TO GO THROUGH EACH FARM
    
   
      # TEMP_ANIMAL_data_frame = TEMP_ANIMAL_data_frame1[farm_id==nfarm]
     
      
     
      
      
      
#==============       MORATLITY AND REMOVAL          =================================
    if(length(TEMP_REMOVAL_id)>0)
    {
      
    
    # KEY IS TO CHANGE THE NUMEBR OF ANIMALS IN EACH DISEASE STATUS ACCORDINGLY
    
      # count how many animals removed from each age group 
      ###### TO DO
      ###### NEED TO CHECK REASON FOR REMOVAL? HOW?? When put removal_date, add info already?
      #####  CONTINUE OTHER EVENTS
      # temp_data = ANIMAL_data_frame %>% filter(farm_id == nfarm)
      # remove_data = temp_data %>% filter(removal_date==0)
      # n_remove_data=  ANIMAL_data_frame %>% filter(farm_id == nfarm & removal_date==0) %>% NROW(.)
      # setkey(TEMP_ANIMAL_data_frame,removal_date) # slower
      # n_remove_data=  TEMP_ANIMAL_data_frame[removal_date==0L, .N]
      # n_remove_data=  TEMP_REMOVAL[farm_id==nfarm, .N]
      # if(n_remove_data>0)
      for(i in TEMP_REMOVAL_id)
      {
        # CHANGE DEMOGRAPHIC
        # temp_demographic = ANIMAL_data_frame %>% filter(farm_id == nfarm & removal_date==0) %>% count(demographic)
        # temp_demographic = TEMP_ANIMAL_data_frame[removal_date==0,.N,by=demographic]
        temp_demographic = TEMP_ANIMAL_data_frame1[farm_id == i & removal_date==0,.N,by=demographic]
        n_row = temp_demographic[,sum(N)]
        # FARM_data_frame[nfarm,]$N_TOTAL = FARM_data_frame[nfarm,]$N_TOTAL - NROW(remove_data)
        FARM_data_frame[i,N_TOTAL := N_TOTAL - n_row]
        for(n_temp_demographic in 1:NROW(temp_demographic))
        {
          # temp_demo = temp_demographic[n_temp_demographic,] %>% pull(demographic)
          # temp_num = temp_demographic[n_temp_demographic,] %>% pull(n)
          temp_demo = temp_demographic[n_temp_demographic,demographic] 
          temp_num = temp_demographic[n_temp_demographic,N]
          if(temp_demo==d_piglet)
          {
            FARM_data_frame[i,N_PIGLET := N_PIGLET - temp_num]
          }
          else if(temp_demo==d_weaned)
          {
            FARM_data_frame[i,N_WEANED := N_WEANED - temp_num]
          
          }
          else if(temp_demo==d_fattening)
          {
            FARM_data_frame[i,N_FATTENING := N_FATTENING - temp_num]
            
          }
          else if(temp_demo==d_gilt)
          {
            FARM_data_frame[i,N_GILT := N_GILT - temp_num]
            
          }
  #-----------REMOVING SOWS THEN REPLACEMENT--------------------------------------------#
          else if(temp_demo==d_sow) # @@@ REPLACE BY NEW GILTS
          {
            # WHEN REPLACING SOW, THEIR PIGLETS SHOULD BE STILL PIGLETS, NOT WEANER
            FARM_data_frame[i,N_SOW := N_SOW - temp_num]
            
            # EXTRACT FARM POLICY
            temp_policy = FARM_data_frame[i,introduce_replace_gilt]
            
            if(temp_policy == 0) # IF PRIORITISING OWN ANIMALS
            {
              # OPTION 1: REPLACE BY OWN PIGLET/GILT
              # temp_WEANER = ANIMAL_data_frame[ANIMAL_data_frame$farm_id == nfarm & ANIMAL_data_frame$sex == female &  ANIMAL_data_frame$demographic==d_weaned,]
              # temp_WEANER = TEMP_ANIMAL_data_frame1[farm_id == i & sex == female &  demographic==d_weaned][order(next_d_date,decreasing=T)]
              temp_WEANER = TEMP_ANIMAL_data_frame1[farm_id == i & demographic==d_replace_weaned]
              
              # STEP 1: COMPARE temp_num and NROW(temp_WEANER), if NROW > temp_num, AVAILABLE GILT IS ENOUGH FOR REPLACEMENT
              if(NROW(temp_WEANER)>=temp_num)
              {
                # temp_WEANER = temp_WEANER[order(temp_WEANER$next_d_date),]
                temp_WEANER_id = temp_WEANER[1:temp_num,id]
                ANIMAL_data_frame[id %in% temp_WEANER_id, ":="(
                  demographic = d_gilt,
                  next_d_date = next_d_date + grow_to_first_farrow
                )
                                  ]
                # NEED To CHANGE next_d_date
               
                FARM_data_frame[i,":="(
                                          N_WEANED = N_WEANED -temp_num,
                                          N_GILT = N_GILT + temp_num)]
               
              }
              # STEP 2: IF temp_num > NROW, TAKE ALL WEANER INTO GILTS. TAKE REMAINING FROM PURCHASE
              else if(NROW(temp_WEANER)<temp_num & NROW(temp_WEANER)>=0)
              {
                temp_WEANER_id = temp_WEANER$id 
                if(NROW(temp_WEANER)>0)
                {
                  ANIMAL_data_frame[id %in% temp_WEANER_id,":="(
                                                                next_d_date = next_d_date + grow_to_first_farrow,
                                                                demographic = d_gilt)]
                  
                  FARM_data_frame[i,N_WEANED := N_WEANED -NROW(temp_WEANER)]
                }
                
                FARM_data_frame[i,N_GILT := N_GILT + temp_num]
                # REMAINING FROM PURCHASE - introduce temp_num animals
                temp_num = temp_num - NROW(temp_WEANER)
                # ADD GILT - WHEN TO ADD? INFECTION STATUS OF GILT (BASED On GLOBAL PREVALENCE FOR NOW)
                temp_gilt_status = rbinom(temp_num,1,prev_IAV_gilt)
                temp_s_date = ifelse(temp_gilt_status==1,day_latent,0)
                temp_n_infected = length(temp_gilt_status[temp_gilt_status==1])
                
                temp_gilt_status = ifelse(temp_gilt_status==1,s_E,s_S) # Because infected is s_I
                
                # ANIMAL_data_frame =  ANIMAL_data_frame %>% add_row(id = seq_along(1:temp_num)+current_pig_id,
                #                                                    next_d_date = rep(introduction_to_first_farrow,temp_num),
                #                                                    farm_id = rep(nfarm,temp_num),
                #                                                    demographic = rep(d_gilt,temp_num),
                #                                                    sex = rep(female,temp_num), 
                #                                                    status = temp_gilt_status,
                #                                                    next_s_date = temp_s_date,
                #                                                    farrow_times = rep(0,temp_num),
                #                                                    removal_date = rep(-1,temp_num) # ASSUMING INTRODUCED GILT DOESN't DIE UNTIL FARROWING
                # )
                ANIMAL_data_frame[id %in% (seq_along(1:temp_num)+current_pig_id),
                                                       ":="(
                                                                   next_d_date = rep(introduction_to_first_farrow,temp_num),
                                                                   farm_id = rep(i,temp_num),
                                                                   demographic = rep(d_gilt,temp_num),
                                                                   sex = rep(female,temp_num), 
                                                                   status = temp_gilt_status,
                                                                   next_s_date = temp_s_date,
                                                                   farrow_times = rep(0,temp_num),
                                                                   removal_date = rep(-1,temp_num) # ASSUMING INTRODUCED GILT DOESN't DIE UNTIL FARROWING
                )]
                
                # if(i==x)
                # {
                #   gilt_x = c(gilt_x,current_day)
                # }
                current_pig_id = current_pig_id + temp_num
                # UPDATE disease counter
                FARM_data_frame[i,
                                                  ":="(N_EXPOSED = N_EXPOSED + temp_n_infected,
                                                       N_SUSCEPTIBLE = N_SUSCEPTIBLE + (temp_num -temp_n_infected),
                                                       N_TOTAL = N_TOTAL + temp_num
                                                  )]
                # FARM_data_frame[nfarm,]$N_EXPOSED = FARM_data_frame[nfarm,]$N_EXPOSED + temp_n_infected
                # FARM_data_frame[nfarm,]$N_SUSCEPTIBLE = FARM_data_frame[nfarm,]$N_SUSCEPTIBLE + (temp_num -temp_n_infected)
                # FARM_data_frame[nfarm,]$N_TOTAL = FARM_data_frame[nfarm,]$N_TOTAL + temp_num
              }
            } # OPTION 1 END
            else
            {
              # if(i==x)
              # {
              #   gilt_x = c(gilt_x,current_day)
              # }
              # OPTION 2: REPLACE BY INTRODUCTION 
              # ADD GILT - WHEN TO ADD? INFECTION STATUS OF GILT (BASED On GLOBAL PREVALENCE FOR NOW)
              temp_gilt_status = rbinom(temp_num,1,prev_IAV_gilt)
              temp_n_infected = length(temp_gilt_status[temp_gilt_status==1])
              
              temp_s_date = ifelse(temp_gilt_status==1,day_latent,0)
              temp_gilt_status = ifelse(temp_gilt_status==1,s_E,s_S) # Because infected is s_I
              # If between MAY and September, longer time to first farrow
              
              
              # ANIMAL_data_frame =  ANIMAL_data_frame %>% add_row(id = seq_along(1:temp_num)+current_pig_id,
              #                                                    next_d_date = rep(introduction_to_first_farrow,temp_num),
              #                                                    farm_id = rep(nfarm,temp_num),
              #                                                    demographic = rep(d_gilt,temp_num),
              #                                                    sex = rep(female,temp_num), 
              #                                                    status = temp_gilt_status,
              #                                                    next_s_date = temp_s_date,
              #                                                    farrow_times = rep(0,temp_num),
              #                                                    removal_date = rep(-1,temp_num) # ASSUMING INTRODUCED GILT DOESN't DIE
              # )
              ANIMAL_data_frame[id %in% (seq_along(1:temp_num)+current_pig_id),
                                ":="(
                                                                 next_d_date = rep(introduction_to_first_farrow,temp_num),
                                                                 farm_id = rep(i,temp_num),
                                                                 demographic = rep(d_gilt,temp_num),
                                                                 sex = rep(female,temp_num), 
                                                                 status = temp_gilt_status,
                                                                 next_s_date = temp_s_date,
                                                                 farrow_times = rep(0,temp_num),
                                                                 removal_date = rep(-1,temp_num) # ASSUMING INTRODUCED GILT DOESN't DIE
              )]
              
              current_pig_id = current_pig_id + temp_num
              # UPDATE disease counter
              FARM_data_frame[i,
                                                ":="(N_EXPOSED = N_EXPOSED + temp_n_infected,
                                                     N_SUSCEPTIBLE = N_SUSCEPTIBLE + (temp_num -temp_n_infected),
                                                     N_TOTAL = N_TOTAL + temp_num,
                                                     N_GILT = N_GILT + temp_num
                                                  
                                                )]
              # FARM_data_frame[nfarm,]$N_EXPOSED = FARM_data_frame[nfarm,]$N_EXPOSED + temp_n_infected
              # FARM_data_frame[nfarm,]$N_SUSCEPTIBLE = FARM_data_frame[nfarm,]$N_SUSCEPTIBLE + (temp_num -temp_n_infected)
              # FARM_data_frame[nfarm,]$N_TOTAL = FARM_data_frame[nfarm,]$N_TOTAL + temp_num
              # FARM_data_frame[nfarm,]$N_GILT = FARM_data_frame[nfarm,]$N_GILT + temp_num
            }
          }
          else if(temp_demo==d_boar)
          {
            FARM_data_frame[i,]$N_BOAR = FARM_data_frame[i,]$N_BOAR - temp_num
          }
        }
        
  #================   UPDATE DISEASE STATUS TABLE DUE TO REMOVAL  ================================== 
        temp_status = TEMP_ANIMAL_data_frame1[farm_id == i & removal_date==0, .N, by=status] 
          
        
        for(n_status in 1:NROW(temp_status))
        {
          temp_demo = temp_status[n_status,status] 
          temp_num = temp_status[n_status,N] 
          if(temp_demo==s_S)
          {
            FARM_data_frame[i,N_SUSCEPTIBLE := N_SUSCEPTIBLE -temp_num]
          }
          else if(temp_demo==s_E)
          {
            FARM_data_frame[i,N_EXPOSED := N_EXPOSED -temp_num]
            
          }
          else if(temp_demo==s_I)
          {
            FARM_data_frame[i,N_INFECTED := N_INFECTED -temp_num]
            
            # RECORD DURATION OF PERSISTENCE
            # TODO: MAY NEED TO CONSIDER EXPOSED AS WELL
            if(FARM_data_frame[i,N_INFECTED]==0)
            {
              
              diff_days = current_day - FARM_data_frame[i,day_p_start]
              persistence_vector = c(persistence_vector,diff_days)
              persistence_farm_id = c(persistence_farm_id,i)
            }
          }
          else if(temp_demo==s_R|temp_demo==s_mda)
          {
            FARM_data_frame[i,N_IMMUNE := N_IMMUNE -temp_num]
           
          }
          
          
        }
        
        # REMOVE ANIMALS FROM DATA
        # remove_data = ANIMAL_data_frame[farm_id == nfarm & removal_date==0][,current_day := current_day]
        # ANIMAL_data_frame = ANIMAL_data_frame[(farm_id != nfarm) | (farm_id == nfarm & removal_date!=0),]
        
      
      }
    # UPDATE farm_id OF ANIALS THAT WERE REMOVED
    ANIMAL_data_frame[removal_date==0, ":="(
      farm_id = NA,
      alive = 0
    )]
      
      
      # @@@ how to differentiate culling and death? also now not considering mortality except pigs, what to do?
    }  
      #=====MORTALITY AND REMOVAL DONE============================
      
      
      
      
      
  
      
      
      

      
      
      
      
      
      
      
      
    
  } # IF NROW(FARM_data_frame)>0)   
}
  

#========================================================================================================

#                     GOING THROUGH EACH BACKYARD FOR EVENTS DONE                                       #
  
#========================================================================================================
  
# UPDATE DATE FOR COMMERCIAL
if(backyard_or_commercial==1)
  {
    if(NROW(COM_FARM_data_frame)>0)
    {
      COM_FARM_data_frame$next_d_date = COM_FARM_data_frame$next_d_date - 1
      COM_ANIMAL_data_frame$next_s_date = COM_ANIMAL_data_frame$next_s_date - 1
    }
    
  #=======================================================================================
  #-------------RECORD---------------
  temp_count_inf = COM_FARM_data_frame %>% distinct(unit_id,N_INFECTED_UNIT) %>% pull(N_INFECTED_UNIT)
  temp_count_total = COM_FARM_data_frame %>% distinct(unit_id,N_TOTAL_UNIT) %>% pull(N_TOTAL_UNIT)
  
  COM_persistence  = rbind(COM_persistence, temp_count_inf*100/temp_count_total)
  
  
  #========NOW GOING THROUGH COMMERCIAL FARM==============================================#
  if(NROW(COM_FARM_data_frame)>0)
  {
    #======DISEASE TRANSMISSION COMPONENT=====================
    temp_infected_UNIT = COM_FARM_data_frame %>% filter(N_INFECTED_UNIT>0)
    if(NROW(temp_infected_UNIT)>0)
    {
      # update room which has infected, due to direct
      # then update in this room due to indirect from other room
      # finally update other room without infected one, but new infections from other rooms
      for(temp_room in 1:NROW(temp_infected_UNIT))
      {
        
        n_sus_animal_room = temp_infected_UNIT[temp_room,]$N_SUSCEPTIBLE_ROOM 
        # TRANSMISSION HAPPEN ONLY IF THERE ARE SUSCEPTIBLE ANIMALS
        if( n_sus_animal_room > 0)
        {
          temp_unique_room_id = temp_infected_UNIT[temp_room,]$unique_room_id
          
          # CHECK IF THIS ROOM and OTHER ROOM HAS INFECTED ANIMAL
          n_infected_animal_room = temp_infected_UNIT[temp_room,]$N_INFECTED_ROOM 
          n_infected_animal_room_other = temp_infected_UNIT[temp_room,]$N_INFECTED_UNIT - temp_infected_UNIT[temp_room,]$N_INFECTED_ROOM
          
          # CALCULATE THE FORCE OF INFECTION DIRECT AND INDIRECT TRANSMISSION
          temp_lamda_direct = 0
          temp_lamda_indirect = 0
            # DIRECT 
              if(n_infected_animal_room>0)
                {
                # IF THIS ROOM HAS INFECTED ANIMALS, DIRECT TRANSMISSION
                temp_lamda_direct = n_infected_animal_room * beta_direct/ temp_infected_UNIT[temp_room,]$N_TOTAL_ROOM
                } # END OF DIRECT TRANSMISSION
              if(n_infected_animal_room_other>0)
                {
                temp_lamda_indirect = n_infected_animal_room_other * beta_indirect/ (temp_infected_UNIT[temp_room,]$N_TOTAL_UNIT - temp_infected_UNIT[temp_room,]$N_TOTAL_ROOM)
              }
          temp_lamda = temp_lamda_direct + temp_lamda_indirect
         
          # UPDATE INDIVIDUAL DISEASE STATUS
          temp_animals = COM_ANIMAL_data_frame %>% filter(unique_room_id == temp_unique_room_id)
          diff_sus = 0
          diff_exposed = 0 # counting how many more or less animals for each disease status
           if(NROW(temp_animals>0))
          {
            for(this_animal in 1:NROW(temp_animals))
            {
              if(temp_animals[this_animal,]$status==s_S)
              {
                if(temp_lamda>=1)
                {
                  temp_lamda = 1
                }
                new_status = rbinom(1,1,temp_lamda)
                if(new_status==1)
                {
                  # IF TRANMISSION UPDATE STATUS
                  temp_animal_id = temp_animals[this_animal,]$animal_id
                  COM_ANIMAL_data_frame[COM_ANIMAL_data_frame$animal_id == temp_animal_id,]$status = s_E
                  COM_ANIMAL_data_frame[COM_ANIMAL_data_frame$animal_id == temp_animal_id,]$next_s_date = day_latent
                  diff_exposed = diff_exposed + 1
                  diff_sus = diff_sus - 1
                }
              }
            }
           }
          # UPDATE N_EXPOSED SUSCEPTIBLE
          COM_FARM_data_frame = COM_FARM_data_frame[unique_room_id== temp_unique_room_id,
                                                    ':='(N_EXPOSED_ROOM = N_EXPOSED_ROOM + diff_exposed,
                                                         N_SUSCEPTIBLE_ROOM = N_SUSCEPTIBLE_ROOM + diff_sus
                                                      
                                                    )]
          
        } # END CHECKING THE PRESENCE OF SUSCEPTIBLE ANIMALS IN tHIS ROOM
          
      }
    }
     #======DISEASE TRANSMISSION DONE============================================# 
       
    #========MORTALITY & FARROW & DEMOGRAPHIC CHANGE HAPPENING ONCE A WEEK
      # next_d_date = 0 means moving to the next production stage
      # need to set up removal_date for those dying 
    
      # BASIC PROCESS OF MOVING DEMOGRAPHIC GROUPS
      # CAPTURE THE LEAVING ANIMAL'S DISEASE STATUS (I OR S) AND TOTAL NUMBER OF ANIMALS IN THIS ROOM
      # CAPTURE THE TOTAL NUMBER OF I AND N IN THE WHOLE UNIT 
      # CAPTURE THE JOINING ANIMAL'S DISEASE STATUS (I OR S) AND TOTAL NUMBER OF ANIMALS THAT JOIN
      # CAPTURE THE TOTAL NUMBER I AND N IN THE UNIT THAT JOINING ANIMALS USED TO BE PRESENT 
      # repeat...
    
      # GO THROUGH ON ROOM LEVEL
      temp_room_demographic = COM_FARM_data_frame %>% filter(next_d_date==0)
   
      if(NROW(temp_room_demographic)>0)
      {
        # FOLLOWING ARE THOSE MOVING TO THE NEXT GROUP OR GOING TO BE REMOVED
        temp_room_finish = temp_room_demographic %>% filter(stage==stage_grow)
        temp_room_farrow = temp_room_demographic %>% filter(stage==stage_farrow)
        temp_room_wean_sow = temp_room_demographic %>% filter(stage==stage_weaned_sow)
        temp_room_gilt = temp_room_demographic %>% filter(stage==stage_gilt_develop)
        temp_room_breeding = temp_room_demographic %>% filter(stage==stage_breeding)
        if(NROW(temp_room_finish)>0)
        {
          for(temp_room in 1:NROW(temp_room_finish)) # PROCESS FINISHER, MOVE FROM FARROW TO GROW AND FROM FARROW TO WEANED SOW
          {
            temp_farm_id = temp_room_finish[temp_room,]$farm_id
          #=========STEP1: REMOVE FINISHERS AND MOVE PIGLETS INTO GROW ROOM=============#
            # GET THE ROOM ID FOR THOSE BEING FINISHED
              FINISH_room_id = temp_room_finish[temp_room,]$room_id
              FINISH_unique_room_id =  temp_room_finish[temp_room,]$unique_room_id
              FINISH_unit_id = temp_room_finish[temp_room,]$unit_id
              
            # GET THE ROOM ID FOR THOSE LEAVING FARROW
              FARROW_room = temp_room_farrow %>% filter(farm_id == temp_farm_id)
              FARROW_room_id = FARROW_room$room_id
              FARROW_unique_room_id = FARROW_room$unique_room_id
              FARROW_unit_id = FARROW_room$unit_id
              
            # GET THE NUM OF INF AND TOTAL IN FARROWING UNIT
              num_inf_unit_FARROW = FARROW_room$N_INFECTED_UNIT
              num_total_unit_FARROW = FARROW_room$N_TOTAL_UNIT
              num_inf_room_FARROW = FARROW_room$N_INFECTED_ROOM
              num_total_room_FARROW = FARROW_room$N_TOTAL_ROOM
            
            # GET THE NUMBER OF INF AND TOTAL IN FINISHERS LEAVING
              num_inf_room_FINISH =  temp_room_finish[temp_room,]$N_INFECTED_ROOM
              num_inf_unit_FINISH =  temp_room_finish[temp_room,]$N_INFECTED_UNIT
              num_total_room_FINISH = temp_room_finish[temp_room,]$N_TOTAL_ROOM
              num_total_unit_FINISH = temp_room_finish[temp_room,]$N_TOTAL_UNIT
              
              # REMOVE ANIMAL THAT ARE SLAUGHTERED
                       remove_data = COM_ANIMAL_data_frame[unique_room_id==FINISH_unique_room_id,]
                       if(NROW(remove_data)>0)
                       {
                         REMOVE_COM_data_frame = REMOVE_COM_data_frame %>% add_row(remove_data)
                       }
                      
                      COM_ANIMAL_data_frame = COM_ANIMAL_data_frame[COM_ANIMAL_data_frame$unique_room_id!=FINISH_unique_room_id,]
              
              # NOW WEANED ANIMALS COME IN FROM FARROW. NEED TO SEPARATE SOW AND PIGLETS
                  new_batch = COM_ANIMAL_data_frame %>% filter(unique_room_id == FARROW_unique_room_id) 
                  new_batch_PIGLET = new_batch %>% filter(farrow_times==0)
                  new_batch_PIGLET_id = new_batch_PIGLET$animal_id
          
                  new_batch_SOW = new_batch %>% filter(farrow_times < max_farrow_L & farrow_times > 0)
                  new_batch_SOW_id = new_batch_SOW$animal_id
                  
                  # REMOVE SOWS THAT REACHED THE MAX FARROWING
                  SOW_to_remove = new_batch %>% filter(farrow_times == max_farrow_L)
                  if(NROW(SOW_to_remove)>0)
                  {
                    REMOVE_COM_data_frame = REMOVE_COM_data_frame %>% add_row(SOW_to_remove)
                    remove_id = SOW_to_remove$animal_id
                    COM_ANIMAL_data_frame = COM_ANIMAL_data_frame[animal_id != remove_id]
                    # remove_sow_status = SOW_to_remove %>% count(status)
                    # remove_sow_I = remove_sow_status %>% filter(status==s_I) %>% pull(n)
                    
                  }
                  
              # COUNT STATUS OF FARROW AND WEANED SOW THAT LEAVE FARROW ROOM
              new_piglet_status = new_batch_PIGLET %>% count(status)
              new_piglet_S = new_piglet_status %>% filter(status==s_S) %>% pull(n)
              new_piglet_I = new_piglet_status %>% filter(status==s_I) %>% pull(n)
              new_piglet_N = NROW(new_batch_PIGLET)
              
              new_weaned_sow_status = new_batch_SOW %>% count(status)
              new_weaned_sow_S = new_weaned_sow_status %>% filter(status==s_S) %>% pull(n)
              new_weaned_sow_I = new_weaned_sow_status %>% filter(status==s_I) %>% pull(n)
              new_weaned_sow_N = NROW(new_batch_SOW)
              
            
                
            # UPDATE GROW ROOM: ADD NUM OF NEW INFECTED AND TOTAL
              if(length(new_piglet_I)==0)
              {
                new_piglet_I = 0
              }
              COM_FARM_data_frame[COM_FARM_data_frame$unique_room_id==FINISH_unique_room_id,]$N_INFECTED_ROOM = new_piglet_I
                
               
              if(length(new_piglet_S)==0)
              {
                new_piglet_S = 0
              }
              COM_FARM_data_frame = COM_FARM_data_frame[unique_room_id==FINISH_unique_room_id,
                                                        ':='(N_SUSCEPTIBLE_ROOM = new_piglet_S,
                                                             N_TOTAL_ROOM = new_piglet_N,
                                                             next_d_date = day_growing_room
                                                          
                                                        )]
                  # ERROR CHECK
              if(length(new_weaned_sow_I)==0)
              {
                new_weaned_sow_I = 0
              }
                  # if(new_weaned_sow_I+new_piglet_I!=num_inf_room_FARROW)
                  # {
                  #   stop("Sum of num of infected sows and piglets are not equal to the number of infected in this room!")
                  # }
            # UPDATE GROW UNIT WHOLE
                new_total_unit = num_total_unit_FINISH - num_total_room_FINISH + new_piglet_N
                new_infected_unit = num_inf_unit_FINISH - num_inf_room_FINISH + new_piglet_I
                COM_FARM_data_frame = COM_FARM_data_frame[farm_id==temp_farm_id & stage==stage_grow,
                                                          ':='(N_INFECTED_UNIT = new_infected_unit,
                                                               N_TOTAL_UNIT = new_total_unit
                                                          )]

                
            # UPDATE PIGLET ANIMAL INFO e.g. ROOM ID THAT COMES IN
                COM_ANIMAL_data_frame = COM_ANIMAL_data_frame[animal_id %in% new_batch_PIGLET_id,
                                                              ':='(room_id = FINISH_room_id,
                                                                   unique_room_id = FINISH_unique_room_id,
                                                                   unit_id = FINISH_unit_id
                                                              )]
               
            #===========STEP 1: DONE============================================#    
                
            #===========STEP 2: FARROW SOWS INTO WEANED SOW ROOM===================#
                # GET THE SOW IN WEANED SOW AREA NOW
                WEANED_SOW_room = temp_room_wean_sow %>% filter(farm_id == temp_farm_id)
                WEANED_SOW_room_id = WEANED_SOW_room$room_id
                WEANED_SOW_room_unique_room_id = WEANED_SOW_room$unique_room_id 
                WEANED_SOW_room_unit_id = WEANED_SOW_room$unit_id
                  # THESE WILL BE ASSIGNED TO THE NEW FARROWED SOWS
                
                # GET INFO FOR ANIMALS THAT ARE CURRENTLY IN WEANED SOW ROOM AND MOVING INTO BREEDING
                current_weaned_sow_animal_id = COM_ANIMAL_data_frame[COM_ANIMAL_data_frame$unique_room_id == WEANED_SOW_room_unique_room_id,]$animal_id
                num_inf_room_WEAN = WEANED_SOW_room$N_INFECTED_ROOM # this is equal to the unit sum because only 1 room 
                # but probably need to consider weaned_sow and breeding in the same room in future
                num_total_room_WEAN = WEANED_SOW_room$N_TOTAL_ROOM
                num_sus_room_WEAN = WEANED_SOW_room$N_SUSCEPTIBLE_ROOM
                
                # MOVE NEW WEANED SOW INTO THIS AREA
                  # ROOM/UNIT UPDATE FOR WEANED SOW
                if(length(new_weaned_sow_S)==0)
                {
                  new_weaned_sow_S = 0
                }
               
                
                if(length(new_weaned_sow_I)==0)
                {
                  new_weaned_sow_I = 0
                }
                COM_FARM_data_frame = COM_FARM_data_frame[unique_room_id==WEANED_SOW_room_unique_room_id,
                                                          ':='(N_SUSCEPTIBLE_ROOM = new_weaned_sow_S,
                                                               next_d_date = day_weaned_sow,
                                                               N_INFECTED_ROOM = new_weaned_sow_I,
                                                               N_INFECTED_UNIT = new_weaned_sow_I,
                                                               N_TOTAL_ROOM = new_weaned_sow_N,
                                                               N_TOTAL_UNIT = new_weaned_sow_N
                                                          )]
               
                
                # UPDATE FARROWED SOWS THAT COME INTO WEANED SOW AREA
                COM_ANIMAL_data_frame = COM_ANIMAL_data_frame[animal_id %in% new_batch_SOW_id,
                                                              ':='(unit_id = WEANED_SOW_room_unit_id,
                                                                   room_id = WEANED_SOW_room_id,
                                                                   unique_room_id = WEANED_SOW_room_unique_room_id
                                                              )]
              
            
            #===========STEP 2: DONE=============================================#
            
            #===========STEP 3: BREEDING SOWS INTO FARROWING ROOM================#
                # VACATE ROOMS OF BREEDING AND MOVE THEM INTO FARROWING ROOM
                BREEDING_room = temp_room_breeding[temp_room_breeding$farm_id==temp_farm_id,]
                BREEDING_room_unit_id = BREEDING_room$unit_id
                BREEDING_room_room_id = BREEDING_room$room_id
                BREEDING_room_unique_room_id = BREEDING_room$unique_room_id
                
                # GET ANIMALS CURRENTLY IN BREEDING
                current_breeding_animal_id = COM_ANIMAL_data_frame[COM_ANIMAL_data_frame$unique_room_id==BREEDING_room_unique_room_id,]$animal_id
                
                # GET CURRENT NUMBER OF INFECTED ETC IN BREEDING ROOM AND UNIT
                num_inf_room_BREED = BREEDING_room$N_INFECTED_ROOM
                num_inf_unit_BREED = BREEDING_room$N_INFECTED_UNIT
                num_total_room_BREED = BREEDING_room$N_TOTAL_ROOM
                num_total_unit_BREED = BREEDING_room$N_TOTAL_UNIT
                num_sus_room_BREED = BREEDING_room$N_SUSCEPTIBLE_ROOM
                
             
              
                
                # FARROW EVENTS: ADD NUM TO FARM TABLE AND ADD ANIMALS TO ANIMALS TABLE
                  # get litter size, allocate disease status depending on the dam's status
                
                new_piglet_total = 0
                new_piglet_S = 0
                
                  if(length(current_breeding_animal_id)>0)
                  {
                    for(this_animal_id in current_breeding_animal_id)
                    {
                      # UPDATE UNIT/ROOM ID OF THIS ANIMAL
                      COM_ANIMAL_data_frame =
                        COM_ANIMAL_data_frame[animal_id==this_animal_id,
                                              ':='(unit_id = FARROW_unit_id,
                                                   room_id = FARROW_room_id,
                                                   unique_room_id = FARROW_unique_room_id,
                                                   farrow_times = farrow_times + 1
                                                
                                              )]
                      
                      
                      temp_animal = COM_ANIMAL_data_frame[COM_ANIMAL_data_frame$animal_id==this_animal_id,]
                      temp_status = temp_animal$status
                      if(temp_status==s_R) # if immune then MDA
                      {
                        # temp_next_s_date = round(-1*log(sample(1:100,litter_size_L)/100)/(1/day_mda_loss))
                        temp_next_s_date = round(rgamma(litter_size_L,shape=shape_gamma_mda,scale=scale_gamma)) #sample from gamma
                        COM_ANIMAL_data_frame = COM_ANIMAL_data_frame %>% add_row(farm_id = rep(temp_farm_id,litter_size_L),
                                                                                  unit_id = rep(FARROW_unit_id,litter_size_L),
                                                                                  room_id = rep(FARROW_room_id,litter_size_L),
                                                                                  unique_room_id = rep(FARROW_unique_room_id,litter_size_L),
                                                                                  animal_id = seq_along(1:litter_size_L) + current_pig_id,
                                                                                  status = rep(s_mda,litter_size_L),
                                                                                  next_s_date =temp_next_s_date,
                                                                                  farrow_times = rep(0,litter_size_L)
                                                                                    
                                                                                  
                        )
                        current_pig_id = current_pig_id + litter_size_L
                        new_piglet_total =new_piglet_total + litter_size_L
                        
                      }else
                      {
                        COM_ANIMAL_data_frame = COM_ANIMAL_data_frame %>% add_row(farm_id = rep(temp_farm_id,litter_size_L),
                                                                                  unit_id = rep(FARROW_unit_id,litter_size_L),
                                                                                  room_id = rep(FARROW_room_id,litter_size_L),
                                                                                  unique_room_id = rep(FARROW_unique_room_id,litter_size_L),
                                                                                  animal_id = seq_along(1:litter_size_L) + current_pig_id,
                                                                                  status = rep(s_S,litter_size_L),
                                                                                  next_s_date = 0,
                                                                                  farrow_times = rep(0,litter_size_L)
                        )
                        current_pig_id = current_pig_id + litter_size_L
                        new_piglet_total =new_piglet_total + litter_size_L
                        new_piglet_S =  new_piglet_S + litter_size_L
                      }
                    }
                  }
                
                # UPDATE FARROW ROOM THAT MOVE INTO (WHICH WAS EMPTY AFTER WEANED SOWS AND PIGLETS MOVED)
                COM_FARM_data_frame = COM_FARM_data_frame[unique_room_id==FARROW_unique_room_id,
                                                          ':='(N_INFECTED_ROOM = num_inf_room_BREED,
                                                               N_TOTAL_ROOM = num_total_room_BREED + new_piglet_total,
                                                               N_SUSCEPTIBLE_ROOM = num_sus_room_BREED + new_piglet_S,
                                                               next_d_date = day_farrow_room
                                                          )
                                                          
                                                          ]
              
                
                # UPDATE FARROW UNIT: SUBTRACT THAT LEFT AND ADD BREEDING ANIMALS THAT COME INTO
                new_inf_unit_FARROW = num_inf_unit_FARROW - num_inf_room_FARROW + num_inf_room_BREED
                new_total_unit_FARROW = num_total_unit_FARROW - num_total_room_FARROW + num_total_room_BREED + new_piglet_total
                
                COM_FARM_data_frame = COM_FARM_data_frame[unit_id==FARROW_unit_id,
                                                          ':='(N_INFECTED_UNIT = new_inf_unit_FARROW,
                                                               N_TOTAL_UNIT = new_total_unit_FARROW
                                                              )
                                                          ]
               
                
            #===========STEP 3: DONE=============================================#    
                
            #===========STEP 4: WEANED SOWS AND GILTS MOVE INTO BREEDING ROOM==========================#
                # GET ROOM/UNIT ID FOR GILT THAT COME INTO BREEDING
                GILT_room = temp_room_gilt[temp_room_gilt$farm_id==temp_farm_id,]
                GILT_room_id = GILT_room$room_id
                GILT_room_unit_id = GILT_room$unit_id
                GILT_room_unique_room_id = GILT_room$unique_room_id
                
                # GET NUM OF S, I, N OF GILTS IN THIS ROOM
                num_sus_room_GILT = GILT_room$N_SUSCEPTIBLE_ROOM
                num_inf_room_GILT = GILT_room$N_INFECTED_ROOM
                num_total_room_GILT = GILT_room$N_TOTAL_ROOM
                
                # GET NUM OF I AND N OF GILTS UNIT
                num_inf_unit_GILT = GILT_room$N_INFECTED_UNIT
                num_total_unit_GILT = GILT_room$N_TOTAL_UNIT
                
                # GET ANIMAL ID OF GILTS THAT MOVE INTO BREEDING
                current_gilt_animal_id = COM_ANIMAL_data_frame %>% filter(unique_room_id == GILT_room_unique_room_id) %>% pull(animal_id)
                new_breeding_animal_id = c(current_weaned_sow_animal_id,current_gilt_animal_id)
                
                
                # NOW UPDATE BREEDING ROOM/UNIT CONDITION
                # UPDATE S,I,N OF THE BREEDING ROOM
                COM_FARM_data_frame =
                  COM_FARM_data_frame[unique_room_id==BREEDING_room_unique_room_id,
                                      ':='(N_INFECTED_ROOM = num_inf_room_GILT + num_inf_room_WEAN,
                                           N_SUSCEPTIBLE_ROOM = num_sus_room_GILT + num_sus_room_WEAN,
                                           N_TOTAL_ROOM = num_total_room_GILT + num_total_room_WEAN,
                                           next_d_date = day_breeding_room
                                      )]
                
            
                
                # UPDATE I AND N OF THE BREEDING UNIT
                COM_FARM_data_frame =
                  COM_FARM_data_frame[unit_id==BREEDING_room_unit_id,
                                      ':='(N_INFECTED_UNIT = N_INFECTED_UNIT + num_inf_room_GILT + num_inf_room_WEAN - num_inf_room_BREED,
                                           N_TOTAL_UNIT = N_TOTAL_UNIT + num_total_room_GILT + num_total_room_WEAN - num_total_room_BREED
                                        
                                      )
                                      ]
           
                # UPDATE ROOM/UNIT ID FOR ANIMALS THAT MOVE INTO BREEDING ROOM
                COM_ANIMAL_data_frame =
                  COM_ANIMAL_data_frame[animal_id %in% new_breeding_animal_id,
                                        ':='(unit_id = BREEDING_room_unit_id,
                                             room_id = BREEDING_room_room_id,
                                             unique_room_id = BREEDING_room_unique_room_id)]
                                          
            
            #===========STEP 4: DONE=============================================#
                
            #===========STEP 5: INTRODUCE NEW GILT AND UPDATE GILT UNIT===========#
                # DETERMINE NEW ANIMAL STATUS 
                num_new_gilt = 5
                new_gilt_status = rbinom(num_new_gilt,1,prev_IAV_gilt)
                new_gilt_s_date = ifelse(new_gilt_status==1,day_latent,0)
                num_sus_new_gilt = num_new_gilt - length(new_gilt_status[new_gilt_status==1])
                new_gilt_status = ifelse(new_gilt_status==1,s_E,s_R)
                COM_ANIMAL_data_frame = COM_ANIMAL_data_frame %>% add_row(farm_id = rep(temp_farm_id,num_new_gilt),
                                                                          unit_id = rep(GILT_room_unit_id,num_new_gilt),
                                                                          room_id = rep(GILT_room_id,num_new_gilt),
                                                                          unique_room_id = rep(GILT_room_unique_room_id,num_new_gilt),
                                                                          animal_id = current_pig_id + seq_along(1:num_new_gilt),
                                                                          status = new_gilt_status,
                                                                          next_s_date = new_gilt_s_date,
                                                                          farrow_times = rep(0,num_new_gilt)
                  
                )
                current_pig_id = current_pig_id + num_new_gilt
                # UPDATE NUM OF S,I,N OF GILT ROOM
                COM_FARM_data_frame =
                  COM_FARM_data_frame[unique_room_id==GILT_room_unique_room_id,
                                      ':='(N_INFECTED_ROOM = 0,
                                           N_SUSCEPTIBLE_ROOM = num_sus_new_gilt,
                                           N_TOTAL_ROOM = num_new_gilt,
                                           next_d_date = day_gilt_development
                                      )]
                # UPDATE NUM OF I AND N OF GILT UNIT
                COM_FARM_data_frame =
                  COM_FARM_data_frame[unit_id==GILT_room_unit_id,
                                      ':='(N_INFECTED_UNIT = N_INFECTED_UNIT + 0  - num_inf_room_GILT,
                                           N_TOTAL_UNIT = N_TOTAL_UNIT + num_new_gilt - num_total_room_GILT
                                           
                                      )
                                      ]
            #===========STEP 5: DONE===============================================#
          }
        } # IF NROW(temp_finishing)>0 ENDS
         
    
      } # if(NROW(temp_room_demographic)>0) ends
       
    
    #========MORTALITY & FARROW & DEMOGRAPHIC DONE=============================================#
    
    #========DISEASE STATUS CHANGE================================================#
      # GO THROUGH ON ROOM LEVEL FIRST TO UPDATE COM_FARM TABLE
      temp_room_status = COM_ANIMAL_data_frame %>% filter(next_s_date==0 & status !=s_S) %>% count(unique_room_id,status)
      if(NROW(temp_room_status)>0)
      {
        for(this_row in 1:NROW(temp_room_status))
        {
          this_room = temp_room_status[this_row,] 
          this_id = this_room %>% pull(unique_room_id)
         
          this_status = this_room %>% pull(status)
          num_change = this_room %>% pull(n)
            if(this_status==s_mda | this_status == s_R) # IMMUNE TO SUSCEPTIBLE, THIS DOESN'T INFLUENCE UNIT
            {
              COM_FARM_data_frame = COM_FARM_data_frame[unique_room_id==this_id,
                                                        ':='(
                                                          N_SUSCEPTIBLE_ROOM = N_SUSCEPTIBLE_ROOM + num_change
                                                        )]
            }
            else if(this_status==s_E) # EXPOSED TO INFECTIOUS, THIS AFFECTS UNIT
            {
              COM_FARM_data_frame = COM_FARM_data_frame[unique_room_id==this_id,
                                                        ':='(
                                                          N_INFECTED_ROOM = N_INFECTED_ROOM + num_change
                                                        )]
              this_unit = COM_FARM_data_frame[unique_room_id==this_id,unit_id]
              
              COM_FARM_data_frame = COM_FARM_data_frame[unit_id == this_unit,
                                                        ':='(
                                                          N_INFECTED_UNIT = N_INFECTED_UNIT + num_change
                                                        )]
                                                        
            }
          else if(this_status==s_I) # INFECTIOUS TO IMMUNE, AFFECTS UNIT
          {
            COM_FARM_data_frame = COM_FARM_data_frame[unique_room_id==this_id,
                                                      ':='(
                                                        N_INFECTED_ROOM = N_INFECTED_ROOM - num_change
                                                      )]
            
            this_unit = COM_FARM_data_frame[unique_room_id==this_id,unit_id]
            
            COM_FARM_data_frame = COM_FARM_data_frame[unit_id == this_unit,
                                                      ':='(
                                                        N_INFECTED_UNIT = N_INFECTED_UNIT - num_change
                                                      )]
          }
          
        }
      }
      # THEN CHANGE ANIMAL LEVEL INFO
      COM_ANIMAL_data_frame = COM_ANIMAL_data_frame[next_s_date==0 & (status == s_mda | status == s_R),
                                                ':='(status=s_S,
                                                     next_s_date = 0
                                                )]
      COM_ANIMAL_data_frame = COM_ANIMAL_data_frame[next_s_date==0 & status == s_E,
                                                    ':='(status=s_I,
                                                         next_s_date = day_infectious
                                                    )]
      # GET NUMBER OF ANIMALS BECOMING I to R
      nrow_become_immune = NROW(COM_ANIMAL_data_frame[next_s_date==0 & status == s_I])
      COM_ANIMAL_data_frame = COM_ANIMAL_data_frame[next_s_date==0 & status == s_I,
                                                    ':='(status=s_R,
                                                         # next_s_date = round(-1*log(sample(1:100,nrow_become_immune,replace=T)/100)/(1/day_immunity_loss))
                                                         next_s_date = round(rgamma(nrow_become_immune,shape=shape_gamma_immunity,scale=scale_gamma))
                                                    )]
    
      
                         
    #========DISEASE STATUS CHANGE DONE============================================#
  }
  #========GOING THROUGH COMMERCIAL FARM DONE=============================================#
}  
  
}
#========GOING THROUGH EACH DATE DONE====================================================#
#-------------------------------------------------------------------------------------------

# }) # profvis
# ERROR CHECK
# COM_ANIMAL_data_frame[unique_room_id==11] %>% NROW(.)
# COM_FARM_data_frame[unique_room_id==11]
# 
# COM_ANIMAL_data_frame[unique_room_id==27] %>% NROW(.)
# COM_FARM_data_frame[unique_room_id==27]
# 
# COM_ANIMAL_data_frame[unique_room_id==32] %>% NROW(.)
# COM_FARM_data_frame[unique_room_id==32]
# 
# summary(COM_persistence[,1])
# summary(COM_persistence[,2])
# summary(COM_persistence[,3])
# COM_persistence[,1]
# head(FARM_data_frame,20)
# nrow(FARM_data_frame[FARM_data_frame$N_PIGLET<0,])
# nrow(FARM_data_frame[FARM_data_frame$N_WEANED<0,])
# nrow(FARM_data_frame[FARM_data_frame$N_FATTENING<0,])
# nrow(FARM_data_frame[FARM_data_frame$N_GILT<0,])
# nrow(FARM_data_frame[FARM_data_frame$N_SOW<0,])
# nrow(FARM_data_frame[FARM_data_frame$N_TOTAL<0,])
# nrow(FARM_data_frame[FARM_data_frame$N_SUSCEPTIBLE<0,])
# nrow(FARM_data_frame[FARM_data_frame$N_EXPOSED<0,])
# nrow(FARM_data_frame[FARM_data_frame$N_INFECTED<0,])
# nrow(FARM_data_frame[FARM_data_frame$N_IMMUNE<0,])
# nrow(FARM_data_frame[FARM_data_frame$N_MDA<0,])
# FARM_data_frame$day_p_start[!is.na(FARM_data_frame$day_p_start)]
# summary(persistence_vector)
# persistence_farm_id[persistence_vector==50]
# FARM_data_frame[x,]
# persistence_farm_x
# immune_farm_x
# immune_farm_x + persistence_farm_x
# gilt_x
# farrow_x
# this is quite long because of transmission parameter maybe too low



#-----------------------IF COMMERCIAL FARMS, THEN USE BELOW TO PROCESS THE RESULTS---------------
# COM_persistence2 = COM_persistence[200:NROW(COM_persistence),]
# COM_persistence2 = melt(COM_persistence)
# COM_persistence2 = as.data.frame(COM_persistence2)
# colnames(COM_persistence2) = c("Day","Unit","Prevalence")
# COM_persistence2$Unit = factor( COM_persistence2$Unit)
# levels(COM_persistence2$Unit) = c("Gilt","Breeding","Farrowing","Weaned","Growing")
# 
# temp_F =COM_persistence2 %>% filter(Unit=="Farrowing" & Prevalence > 0) 
# temp_G =COM_persistence2 %>% filter(Unit=="Gilt" & Prevalence > 0) 
# temp_B =COM_persistence2 %>% filter(Unit=="Breeding" & Prevalence > 0) 
# temp_G =COM_persistence2 %>% filter(Unit=="Growing" & Prevalence > 0) 
#--------------------------------------------------------------------------------------------------

# 
# x = 0
# y = 0
# for(i in 2:nrow(temp_F)){
#   if(temp_F[i,1] == temp_F[i-1,1] + 1)
#   {
#     x = x + 1
#     if(y < x)
#     {
#       y = x # record the longest
#     }
#     
#   }else{
#     x = 0
#   }
# }
# F_d = y
# 
# x = 0
# y = 0
# for(i in 2:nrow(temp_B)){
#   if(temp_B[i,1] == temp_B[i-1,1] + 1)
#   {
#     x = x + 1
#     if(y < x)
#     {
#       y = x # record the longest
#     }
#     
#   }else{
#     x = 0
#   }
# }
# B_d = y
# 
# result_D = c(result_D,F_d)
# result_B = c(result_B,B_d)

# SAVE PERSISTENCE DATA
list_persistence_vector[[k]] = persistence_vector
list_persistence_farm[[k]] = persistence_farm_id

}
