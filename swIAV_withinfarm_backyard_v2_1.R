#=========================================================================#
#----------Mathematical model for within-farm swIAV transmission v1-------
#=========================================================================#
#****CHUNK 1
library(tidyverse)
library(data.table)
set.seed(12345)

# Do we add a commercial farm?
include_commercial_farm = 0
include_backyard = 1
# Define variables
# DISEASE STATE: M S E I R (tentative: need to think if include E, check how long it is)
s_mda = 0
s_S = 1
s_E = 2 # TO DO: needs to add event from E to I
s_I = 3 # TO DO: needs to add event from I to R
s_R = 4

# INDIVIDUAL LEVEL OF IAV IN GILT
prev_IAV_gilt = 0.1
# Decay of maternal derived antibody
day_mda_loss = 70 # it's noy 1 or 0, gradual decline, so need to think how to function that
# Decay of immunity after infection
day_immunity_loss = 180
  # GAMMA DISTRIBUTION FOR IMMUNITY
  scale_gamma = 3
  shape_gamma_mda = day_mda_loss/scale_gamma
  shape_gamma_immunity = day_immunity_loss/scale_gamma


# Latent period
day_latent = 2
# Infectious period
day_infectious = 6
# Transmission coefficient (direct)
beta_direct = 0.5 # need to refine, density-dependent? Assuming R0 = 3, D = 6 then beta = beta_direct/N (now fequency-dependent)
# Transmission coefficeint (indirect)
beta_indirect = beta_direct/24 # Cador 2017
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
length_simulation_day = 3*12*30 # unit is day

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
# Culling for sows
cull_mo_min = 58
cull_mo_max = 65 # if sows reach this minimum age of culling and sows not having weaned piglets, then cull

# Culling for sows in commercial company 3.8 year (45 months)
cull_mo_L = 48 # 8mo is the first service for large company, 77 days untile first service from intro
# 147 days is 1 cycle. 8 cycles mean 147*8-7 days, 39 months. first service is 7.8 mo, making 47.8 mo. 
max_farrow_L = 8 # once reach this, cull. 

# Mortality of piglet before weaning
num_die_piglet_min = 1
num_die_piglet_max = 4
# FOR Large company, only minumim number
# r_reason
r_death = 0
r_sold = 1
r_culled = 2


# Weaning age/period in day
wean_day_min = 81
wean_day_max = 108

# Weaning age for Large company
wean_day_L = 30

# Time from weaning to become grower (needs this info)
wean_to_grow = 90
# Time from weaning to become grower for Large company
wean_to_grow_L = 30

# Time from entering growing until finishing
grow_to_finish = 180
# Time from entering growing until finishing in Company
grow_to_finish_L = 90

# Time from entering growing until first farrowing (15mo)
grow_to_first_farrow = 250
# Time from entering growing until first farrowing for Company
grow_to_first_farrow_L = 300 # farrowing at 12 mo

# Time from introducing into a farm until first farrowing
introduction_to_first_farrow = 150
# Time from first farrow till culling 
first_farrow_cull_min = 40*30
first_farrow_cull_max = 55*30

first_farrow_cull_L = 36*30 # company
# litter size: uniform(6,9)
litter_size_min = 6
litter_size_max = 9

litter_size_L = 11

# Reproduction parameter
return_to_heat = 80
return_to_heat_L = 30

gestation_period = 120
# INITIALIZE FARM AND ANIMAL INFO
current_pig_id = 0
farm_id = 0
start_date = as.Date('2020-05-01')



# demographic and status: see above. sex: female 0 and male 1. age in months. 
# immunity_date: day until immunity dissapears. next_d_date: day until weaning. 
# infection_history: 0 not infected before. 1 infected before. Needs to consider multiple subtype maybe. 

# SETTING UP FARM_data_frame - now onlyconsidering farrow-to-finish but this needs to be updated
#=============SETTING UP BACKYARD FARM================================================================#
if(include_backyard==1)
{
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
  
  
  
  
  
  n_backyard = 100
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
  
  
  # REMOVE_data_frame FOR BACKYARD
  REMOVE_data_frame = EMPTY_data_table
  REMOVE_data_frame = mutate(REMOVE_data_frame, r_reason = integer(),current_day= integer())
}


#========= ADD COMMERCIAL FARMS IF NECESSARY===========================================================
# WONDERING IF BETTER TO TREAT ONE BATCH AS ONE GROUP
# WHAT'S THE EFFECT OF BATCH SIZE - 
# ALSO DO WE NEED EACH INDIVIDUAL ANIMAL DATA? PROBABLY NOT. JUST RECORD ROW FOR BATCH, COLUMN FOR STATUS
# MULTIPLE BATCHES CAN EXIST IN THE SAME ROOM - IF IN SAME UNIT AND SAME DAY UNTIL NEXT D, THEN SAME ROOM
# REALLY EASY TO USE IBM FRAMEWORK
if(include_commercial_farm==1)
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
                                   batch_size = integer(), # do I need this?
                                   next_d_date = integer() # DEMOGRAPHIC CHANGE OCCURRS AT ROOM LEVEL
                                   
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
  n_commercial_FF = 1 #number of farrow to finish and fattening
  n_structure_FF = 5 # number of structures in FF
  farm_type_FF = 1 # id for Farrow-to-Finish (FF)
  # 1. Gilt development room 70 days (IN LAOS SETTING FIRST SERVICE 8 MONTHS)
  day_gilt_development = 70
  stage_gilt_develop = 1
  # 2. Breeding room 112 days
  day_breeding_room = 112
  stage_breeding = 2
  # 3. Farrowing room 28 days (Farrowing at the first 7 days)
  day_farrow_room1 = 7
  day_farrow_room2 = 21
  day_farrow_room = 28 #SUM
  stage_farrow = 3
  # 4. Weaned sow room 7 day (for sow 3 -> 4 -> 2) or culled
  day_weaned_sow = 7
  stage_weaned_sow = 4
  # 5. Growing pig room 140 days (for piglet 3 -> 5) - this is structure of finishing farm
  day_growing_room = 140
  stage_grow = 5
  # 6. Place for weaned piglets that become gilts?
  
  
  n_commercial_PIGLET = 0 # number of piglet producing commercial farms
  n_structure_PIGLET = 4
  farm_type_PIGLET = 2
  n_commercial_FATTENING = 0 # number of fattening producing farms 
  n_structure_FATTENING = 1 
  farm_type_FATTENING = 3
  # example farm using Etbaigha et al every block 2 animals (block is the 1 week block). Every week introduce 2 gilts
  current_farm_id = max(farm_id) + 1
  current_unit_id = 0
  current_batch_id = 0
  current_unique_room_id = 0
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
persistence_farm_x = c()
immune_farm_x = c()
gilt_x = c()
farrow_x = c()
x = 46
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
  # UPDATE DATE FOR BACKYARD


#===============BACKYARD=============================================

  
if(include_backyard==1)
{
  if(NROW(ANIMAL_data_frame)>0)
  {
    # SUBTRACT 1 FROM each variable that describes dates
    ANIMAL_data_frame$next_s_date = ANIMAL_data_frame$next_s_date - 1 # DAY FOR STATUS CHANGE
    ANIMAL_data_frame$next_d_date = ANIMAL_data_frame$next_d_date - 1 # DAY FOR DEMOGRAPHIC CHANGE
    ANIMAL_data_frame$farrow_date = ANIMAL_data_frame$farrow_date - 1 # DAY FOR FARROW - DO I NEED THIS IN ADDITION TO next_d_date
    ANIMAL_data_frame$removal_date = ANIMAL_data_frame$removal_date - 1 # DAY FOR REMOVAL
  }
  #============GOING THROUGH EACH BACKYARD FOR EVENTS===================================#
  # Calculate the number of new infections beta*S*I, IF S > 0
  if(NROW(FARM_data_frame)>0)
  {
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
        remove_data = remove_data %>% mutate(current_day = current_day)
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
            # piglet_immunity_date = round(-1*log(sample(1:100,n_piglet,replace=T)/100)/(1/day_mda_loss))
            piglet_immunity_date = round(rgamma(n_piglet,shape=shape_gamma_mda,scale=scale_gamma)) #sample from gamma
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
              # next_s_date = round(-1*log(sample(1:100,1)/100)/(1/day_immunity_loss)) # get a random time to next event
              next_s_date =  round(rgamma(1,shape=shape_gamma_immunity,scale=scale_gamma)) #sample from gamma
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
  } # IF NROW(FARM_data_frame)>0)   
}
  #========GOING THROUGH EACH BACKYARD FOR EVENTS DONE========================================#
  
# UPDATE DATE FOR COMMERCIAL
if(include_commercial_farm==1)
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


# ERROR CHECK
COM_ANIMAL_data_frame[unique_room_id==11] %>% NROW(.)
COM_FARM_data_frame[unique_room_id==11]

COM_ANIMAL_data_frame[unique_room_id==27] %>% NROW(.)
COM_FARM_data_frame[unique_room_id==27]

COM_ANIMAL_data_frame[unique_room_id==32] %>% NROW(.)
COM_FARM_data_frame[unique_room_id==32]

summary(COM_persistence[,1])
summary(COM_persistence[,2])
summary(COM_persistence[,3])
COM_persistence[,1]
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