#===========FUNCTIONS==================================================

#======UPDATE FORCE OF INFECTION========
update_lambda_backyard = function(temp_I,temp_N, beta){
  
  # temp_I = Data[,N_INFECTED]
  # temp_N = Data[,N_TOTAL]
  if(temp_I>0){
    temp_lambda = temp_I * beta/ temp_N
   
  }else
  {
    temp_lambda = 0
  }
  return(temp_lambda)
}

#=======UPDATE NUMBER OF NEW INFECTIONS========
calc_new_infection = function(temp_I,temp_N,temp_S,beta){
  
  if(temp_I>0 & temp_S>0){
    temp_lambda = temp_I * beta/ temp_N
    
    new_inf_count = rbinom(1,temp_S,temp_lambda)
    
  }else
  {
    new_inf_count = 0
  }
  return(new_inf_count)
}

#====UPDATE INFECTION STATUS==========
update_infection_backyard = function(nfarm,new_inf_count,ANIMAL_DATA){
  

      # CALCULATE NUM OF NEWLY EXPOSED ANIMALS
     
        # ANIMAL_DATA = as.data.table(ANIMAL_DATA)
        # EXTRACT SUSCPETIBLE FROM THIS FARM
        s_S = 1
        temp_SUS_id = ANIMAL_DATA[farm_id == nfarm & status==s_S,id]
        
        if(length(temp_SUS_id)==1) # if only one animal choose this animal
        {
          new_EXPOSED_id = temp_SUS_id
        }
        else
        {
          new_EXPOSED_id = sample(temp_SUS_id,new_inf_count,replace=F)
        }
       return(new_EXPOSED_id)
       
  
}