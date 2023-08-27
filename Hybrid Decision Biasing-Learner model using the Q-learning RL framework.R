###########
#Hybrid Decision Biasing-Learner model using the Q-learning RL framework
###########
#Simulator for decision-biasing agent
dbSimulator <- function(gammaVec, nAgents = 4, trials = 25, rounds = 1, alpha = .9, beta = 1, Q0 = 0){
  dbDF <- data.frame() #initialize dataframe
  for (r in 1:rounds){ #loop through rounds
    Qmat <- matrix(Q0,  nrow = nAgents, ncol = k) #Initialize Q-values in a matrix with one row per agent
    socialObs <- rep(NA, nAgents) #social observations
    for (t in 1:trials){ #loop through trials
      for (agent in 1:nAgents){ #loop through agents
        p_ind <- softmax(beta, Qmat[agent,]) #compute individual learning policy; we assume for now that all individual agents have the same parameters as before
        if (t==1){ #first trial has no social
          p_fdc = rep(1/k, k) #uniform probabilities
        }else{
          socialFreq <- table(factor(socialObs[-agent], levels = 1:k)) + .0001 #Frequency of each socially observed action + a very small number to avoid divide by zero
          p_fdc <- socialFreq/sum(socialFreq) #compute probabilities
        }
        p_db <- ((1-gammaVec[agent]) * p_ind) + (gammaVec[agent] * p_fdc) #mixture policy
        action <- sample(1:k,size = 1, prob=p_db) #sample action
        reward <- banditGenerator(action) #generate reward
        Qmat[agent,action] <- Qmat[agent,action] + alpha*(reward - Qmat[agent,action]) #update q-values
        chosen <- rep(0, k) #create an index for the chosen option
        chosen[action]<- 1 #1 = chosen, 0 not
        socialObs[agent] <- action #update social observation vector
        #save data
        trialDF <- data.frame(trial = t, round = r, agent = agent,  reward = reward)
        dbDF <- rbind(dbDF,trialDF)
      }
    }
  }
  
  return(dbDF)
}

gammaVec <-runif(4) #replace with 
dbDF <- dbSimulator(gammaVec) #run simulation


ggplot(dbDF, aes(x = trial, y = reward, color = factor(agent)))+
  geom_line()+
  theme_classic()+
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442"), name = 'Agent')+
  theme(legend.position=c(1,0.1), legend.justification = c(1,0))+
  labs(x = 'Trial', y = 'Reward', title = 'Decision-biasing agents')