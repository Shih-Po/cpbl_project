extrabase_function = function(dummy_list){
    
  rem_type <- dummy_list$rem_type
  base1 <- dummy_list$base1
  base2 <- dummy_list$base2
  base3 <- dummy_list$base3
  player <- dummy_list$player
  
    if (base3 != "NA"){
    base3 = "NA"
          
    }else if(base3 == "NA"){
        
        if (base2 != "NA"){
        base2 = "NA"
        
        }else if(base2 == "NA"){
            
            if (base1 != "NA"){
            base1 = "NA"
                                
            }
        }  
    }
  
  dummy_list$rem_type <- rem_type
  dummy_list$base1 <- base1
  dummy_list$base2 <- base2
  dummy_list$base3 <- base3 
  dummy_list$player <- player
  return (dummy_list);
}    

