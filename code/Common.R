#
#
#  Common nothing new
#
#
data_folder <-file.path(here::here(), "./data")
dulux_initial_clean <-file.path( data_folder, "Dataset Dulux 2503.xlsx")
dulux_raw_file       <-file.path( data_folder, "Dulux Leads Data.xlsx")
#  To rebuil the data set on filed was missing in Dulux leads data.xlsx
c4c_file <-file.path( data_folder, "C4C Lead data.xlsx")



# preprocess data file
first_data  <-file.path( data_folder, "to_share.csv")
second_data <-file.path( data_folder, "working_data.Rds")






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


#
# Self coded knn
#
# n output: the distance of the k train observation with the input
#
knn <- function(train, test, k, method, response){
  # Test
  assertthat::assert_that(is.data.frame(train))
  assertthat::assert_that(is.data.frame(test))
  assertthat::assert_that(is.character(response))

  # Test if NA
  train_na <-sum(is.na(train))
  test_na  <-sum(is.na(test))
  if( train_na > 0 | test_na > 0){
      message("Some NA have been detected in the train and test, those observations are excluded from the model")
      if( train_na > 0){ train <-na.omit(train)}
      if( test_na > 0) { test  <-na.omit(test) }
  }
  # Remove response
  train_response   <-train[,response, drop=FALSE]
  train_response   <-train_response %>%
    dplyr::mutate( index=dplyr::row_number())

  train   <-train %>%
    dplyr::select(-response) %>%
    dplyr::mutate(index = dplyr::row_number())

  if( response %in% names(test)){
      message("The response variable was in the test data set. It has been removed")
      test <- test %>%
              dplyr::select(-response)
  }
  #   We test the class
  all_class <-sapply(train, class)
  have_character <-any(stringr::str_detect(all_class,"character") == TRUE)
  if( have_character){
    message("Train data set includes characters variables, they have been transformed in factor for the distance")
    character_variables <-which(stringr::str_detect(all_class,"character") == TRUE)
    for( index_factor in character_variables){
         train[,index_factor] <-factor(train[,index_factor])
         test[,index_factor] <-factor(test[,index_factor])
    }
  }
  index_index <-which(names(train) =="index")
  #  End test and pre-processing
  n.test <- nrow(test)
  n.train <- nrow(train)
  if (n.train + n.test <= k) stop("k can not be more than n-1")
  neigh     <- matrix(0, nrow = n.test, ncol = k)
  ref_train <- matrix(0, nrow = n.test, ncol = k)

  for(i in 1:n.test) {
      xmat          <- rbind(test[i,], train[, -index_index ])        #we make a 2 row matrix combining the current test and train rows
      all_distances <- as.matrix(cluster::daisy(as.data.frame(xmat), metric= method))[1,-1]  #then we calculate the distance and append it to the ddist vector.
      all_distances <- data.frame(dist=all_distances)
      all_distances$index <-train$index
      all_distances  <-all_distances %>%
                           dplyr::arrange( dist)
    neigh[i, ]    <- all_distances$dist[1:k]
    ref_train[i,] <- all_distances$index[1:k]
  }
  # now that we have the closest we must build the response
  class_response <-class(train_response[, response])
  if(class_response == "factor"){
    predict <-NULL
    for(test_index in 1:n.test){
      freq<-as.data.frame(table(neigh[test_index, ]))
      predict[test_index] <-freq$freq[which(freq$Freq == max(freq$Freq ))]
    }
  }else{
    predict  = numeric(n.test)
    for(test_index in 1:n.test){
      predict[test_index] <- mean(train_response[,response, drop=TRUE][ref_train[test_index, ]])
    }
  }

 back <-list( predict   =predict,
              distance  =neigh,
              reference =ref_train )
  return(back)
}

