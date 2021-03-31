#
#
#  Common nothing new 
#
#
data_folder <-file.path(here::here(), "./data")
dulux_initial_clean <-file.path( data_folder, "Dataset Dulux 2503.xlsx")
dulux_raw_file       <-file.path( data_folder, "Dulux Leads Data.xlsx")


#
#  Cutoff function calculation for glm  model
#
#  Ouput:  a list which includes 
#          1. the optimum cut off value 
#          2. The minum number of errors 
#          3. all the errors acrross the cut off from .1 to .8 
#          4. the preddiction in trem of probability 
optimum_cut_off <-function( model, data, response){
  # All the checks 
  rf_model    <-any(stringr::str_detect(class(model), "randomForest") == TRUE)
  assertthat::assert_that(class(model)[1] == "glm" |  rf_model  == TRUE )
  assertthat::assert_that(any(class(hot_cluster_1) =="data.frame"))
  assertthat::assert_that(any(class(response) =="character"))
  # End of check 
  
  response <-as.numeric(data[, response, drop=TRUE])-1
  if( rf_model == TRUE){
          pre_response  <-predict( model, type="prob")
          pre_response  <-pre_response[,2]
  }else{
          pre_response  <-predict( model, type="response")
  }
  all_predict   <-data.frame(response, 
                             predict= pre_response )
  cut_off <-seq(.1,.8, .1)
  for( cur_cut in cut_off){
    ref <-  as.character( cur_cut * 10)
    all_predict <- all_predict %>%
      dplyr::mutate( "cut_{ref}" := ifelse(pre_response  >= cur_cut, 1,0))
    
  }
  errors   <-sapply( all_predict[,c(3:(length(cut_off)+2))], function(predict)( sum(response- predict)))
  errors   <-abs(errors)
  min_error <-which(errors == min(errors))
  back <-list(cut_off        = cut_off[min_error],
              discripensity  = errors[min_error],
              errors= errors, 
              prediction =  pre_response)
  return(back)
}
