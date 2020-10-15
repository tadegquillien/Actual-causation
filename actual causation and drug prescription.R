######
#' This code simulates the predictions
#' of the model of causal judgment described in
#' "Quillien (2020).When do we think that X caused Y?. Cognition"
#' (https://drive.google.com/file/d/1oZh4fLus69w2Qkv5WHVlvtpG0Vy4qea8/view?usp=sharing)
#' as applied to the vignette from study 2 in
#' "Schwenkler & Sytsma (unpublished ms.) Reversing the norm effect
#' on causal attribution". (https://philpapers.org/rec/SCHRTN-5)
#'  
#' See https://twitter.com/ax_phi/status/1315688538403340298
#' for background

#'The tidyverse package is needed to run the code  


#this function simulates one instance of the treatment

#baseRateBad: prob. that the drug is bad
#x and y are the probability of refusing the drug,
#given that it is bad, for the good and bad doctor,
#respectively
treatment <- function(baseRateBad,x,y){
  
  #at the beginning the patient is sick
  patient_state = 0
  
  ##is the drug good?
  if (runif(1,0,1) < baseRateBad){
    drug <- "bad"
  }else{drug <- "good"}
  
  
  #if the drug is good, both doctors prescribe, and patient recovers
  if(drug == "good"){
    good_doctor_prescribes <- 1
    bad_doctor_prescribes <- 1
    patient_state <- 1
  }
  
  #if the drug is bad, each doctor may refuse prescription
  
  if(drug == "bad"){
    
    #good doctor prescribes with prob. 1-x
    if(runif(1,0,1) < x){
      good_doctor_prescribes <- 0
    }else{good_doctor_prescribes <- 1}
    
    #the bad doctor behaves as a bad doctor with probability 1/2
    if (runif(1,0,1) < .5){
      #if the bad doctor behaves as a bad doctor,
      #he prescribes the drug with probability 1-y
      if(runif(1,0,1) < y){
        bad_doctor_prescribes <- 0
      }else{bad_doctor_prescribes <- 1}
    }
    
    #what happens when the bad doctor behaves like a good doctor
    else{
      if(runif(1,0,1) < x){
        bad_doctor_prescribes <- 0
      }else{bad_doctor_prescribes <- 1}
    }
    
    #if both doctors prescribe the drug, the patient is harmed
    if(bad_doctor_prescribes + good_doctor_prescribes == 2){
      patient_state <- -1
    }
  }
  #return a vector with the following info:
  #whether the good doctor prescribed, whether the bad doctor did, 
  #and the state of the patient
  sim_outcome <- c(good_doctor_prescribes, bad_doctor_prescribes, patient_state)
  return(sim_outcome)
}

#this function runs n simulations of the treatment
run_simulations <- function(n, baseRateBad, x, y){
  vec_g <- c()
  vec_b <- c()
  vec_s <- c()
  #run the simulations
  for (i in 1:n){
    sim <- treatment(baseRateBad,x,y)
    vec_g <- c(vec_g, sim[1])
    vec_b <- c(vec_b, sim[2])
    vec_s <- c(vec_s, sim[3])
    
  }
  #collect the simulation results into a dataframe
  df <- data.frame(vec_g, vec_b, vec_s)
  colnames(df) <- c("good_doctor", "bad_doctor", "outcome")
  
  return(df)
}

#This function generates a dataframe which collects the model's
#causal rating for a wide range of parameters

generate_graph <- function(){
  vec_g <- c()
  vec_b <- c()
  vec_x <- c()
  vec_y <- c()
  vec_br <- c()
  #cycle through different values for x
  for (i in seq(0,1,by=.05)){
    #cycle through different values for delta
    for (j in c(.1,.2,.3,.4)){
      #cycle through different values for baseRateBad
      for (br in c(.3, .5, .7)){
        x <- i
        y <- i - j
        #run 1000 simulations for a given parameter combination
        df <- run_simulations(1000, br, x,y)
        #compute the causal rating for the good and the bad doctor
        #each causal rating is computed by computing a correlation
        #across the 1000 simulations
        cor_g <- cor(df$good_doctor, df$outcome)
        cor_b <- cor(df$bad_doctor, df$outcome)
        #make sure to return NA if the parameters are
        #such that y is below 0
        if (y < 0){
          cor_b <- NA
        }
        #normalize the causal ratings with a softmax transformation
        cor_good <- exp(cor_g)/(exp(cor_g)+exp(cor_b))
        cor_bad <- exp(cor_b)/(exp(cor_g)+exp(cor_b))
        
        #store the info from the simulations in the relevant vectors
        vec_g <- c(vec_g, cor_good)
        vec_b <- c(vec_b, cor_bad)
        vec_x <- c(vec_x, x)
        vec_y <- c(vec_y, j)
        vec_br <- c(vec_br, br)
      }
      
    }
  }
  #collect all causal ratings in a dataframe
  #and return that dataframe
  dat <- data.frame(vec_x, vec_y, vec_g, vec_b, vec_br)
  colnames(dat) <- c("x", "delta", "Good_doctor", "Bad_doctor", "Prob_bad")
  return(dat)
}

#run the simulations
dat <- generate_graph()

#actually draw the graph
ggplot(dat, aes(x=x, y=Good_doctor))+geom_line(color="blue")+
  geom_line(aes(y=Bad_doctor), color="red")+
  facet_grid(Prob_bad~delta, labeller=label_both)+
  ylab("Causation rating")+xlab("Pr(good doctor says No|bad drug)")

#uncomment the next line to save the graph on a hard drive
#ggsave("causal_graph_good_outcomes.png", dpi=500, width=7, height=5)

##'Note that the above code generates causal ratings for
##' "the agent caused the patient to recover"
##' To generate causal ratings for "the agent caused the patient
##' to be harmed", simply run the same simulations while multiplying
##' the variable "patient_state" by -1
