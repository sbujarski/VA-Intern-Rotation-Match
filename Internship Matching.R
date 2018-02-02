####################################################
#                                                  #
#    Internship Rotation Matching Algorithm        #
#                                                  #
####################################################


#library(xlsx) #package to import xls files directly
 
#Function to check whether a configuration is valid - works
check <- function(assignment, rotations){
  
  #check each rotation assigned each slot
  #set default to true
  R.valid <- T
  
  for (r in 1:dim(assignment)[2]){
    R <- assignment[,r]
    R.valid <- identical(sort(R), sort(rotations)) && R.valid 
  }
  
  #check no duplicate assignments
  Dup.valid <- T
  for(i in 1:dim(assignment)[1]){
    Dup.valid <- Dup.valid && (anyDuplicated(assignment[i,])==0)
  }
  
  #Return whether all valid
  return(R.valid && Dup.valid)
}

#Function to evaluate fit - works
#returns fitsum (sum of all ranks in assignment) and equitable (differnce between max and min fitsum for interns)
fit <- function(assignment, ranks){
  fits <- matrix(NA, nrow = dim(assignment)[1], ncol = dim(assignment)[2])
  for (i in 1:dim(assignment)[1]){
    for (r in 1:dim(assignment)[2]){
      fits[i,r] <- which(ranks[i,] == assignment[i,r], arr.ind = TRUE)
    }
  }
  #sum fits
  fits <- cbind(fits,rowSums(fits))
  fitsum <- sum(fits[,dim(fits)[2]])
  
  #compute equitability
  equitable <- max(fits[,dim(fits)[2]]) - min(fits[,dim(fits)[2]])
  
  return(c(fitsum,equitable))
}

#fit function with post-doc bonus
#subtracts 0.5 if first rotation is first rank
fit.PD <- function(assignment, ranks,PDbonus){
  fits <- matrix(NA, nrow = dim(assignment)[1], ncol = dim(assignment)[2])
  for (i in 1:dim(assignment)[1]){
    for (r in 1:dim(assignment)[2]){
      fits[i,r] <- which(ranks[i,] == assignment[i,r], arr.ind = TRUE)
    }
  }
  
  #put in post-doc bonus
  fits[,1] <- ifelse(fits[,1]==1, fits[,1]-PDbonus, fits[,1])
  
  #sum fits
  fits <- cbind(fits,rowSums(fits))
  fitsum <- sum(fits[,dim(fits)[2]])
  
  #compute equitability
  equitable <- max(fits[,dim(fits)[2]]) - min(fits[,dim(fits)[2]])
  
  return(c(fitsum,equitable))
}

#match algorith function
#takes ranks from all interns, rotations vector, n runs to sample, and equitability tolerability 
match <- function(rotations, nrots, ranks, samples, eqtol, PDbonus){
  #tracking of algorithm
  fit.track <- data.frame(sample=rep(NA,samples),fit=rep(NA,samples))
  
  #Error Checking----
  #Duplicate rotations in rotations list
  if(sum(duplicated(rotations))>0) {
    print(noquote("List of rotations has duplicates"))
    print(noquote("Match algorithm not run"))
    return()
  }
  
  #Ranking error
  if(length(unique(table(ranks))) > 1){
    print(noquote("Intern's rankings contains an error"))
    print(noquote("Match algorithm not run"))
    return()
  }
  
  #Ranks don't match rotations
  if(!identical(unique(sort(ranks)), unique(sort(rotations)))){
    print(noquote("Rotations options and rankings list don't match up"))
    print(noquote("Match algorithm not run"))
    return()
  }
  
  #Mismatch number of rotations and interns
  if(length(rotations) != dim(ranks)[1]){
    print(noquote("Number of rotations doesn't equal number or interns"))
    print(noquote("Match algorithm not run"))
    return()
  }
  
  
  #Matching algorith----
  
  #calculate minimum fit to stop match
  fitmin <- length(rotations) * sum(1:nrots) - length(rotations) * PDbonus
  
  #split optimizing path based on whether there is a post-doc bonus
  #no PD bonus first
  if(PDbonus==0){
    #set random initial final assignment (to be overwritten if swapping helps)
    valid<-F
    while(!valid){
      final.assignment <- matrix(NA, nrow=length(rotations), ncol=nrots)
      for (n in 1:nrots){
        final.assignment[,n] <- sample(rotations)
      }
      valid <- check(final.assignment, rotations) && fit(final.assignment,ranks)[2]<= eqtol
    }
    
    #primary loop to go through random sampling and swapping/optimizing 
    for(x in 1:samples){
      print(noquote(x))  #UNDELETE AFTER SIMULATIONS
      #set optimum to Fasle -- when this round is over
      optimum <- F
      
      #assign initial rotations at random
      valid <- F
      while(!valid){
        assignment <- matrix(NA, nrow=length(rotations), ncol=nrots)
        for (n in 1:nrots){
          assignment[,n] <- sample(rotations)
        }
        valid <- check(assignment, rotations) && fit(assignment,ranks)[2]<= eqtol
      }
      #print(noquote(assignment))
      
      #swapping  algorithm to local optimum
      #algorithm breakdown
      #step through shuffled comb list and swap pairs within each column. 
      #see if each swap improves fit, maintaing acceptable equity, and is valid
      #if it does overright the previous assignment and break for loop
      #if not move on to next swap
      #set optimum to true if going through the whole thing doesn't improve fit. 
      
      while(!optimum){
        #make the shuffled list of combinations to swap
        comb <- do.call(cbind, replicate(dim(assignment)[2], combn(dim(assignment)[1], 2), simplify=F)) 
        
        #Assign rotation number to each
        comb <-rbind(matrix(sort(rep(1:dim(assignment)[2], choose(dim(assignment)[1],2))),nrow=1), comb)
        
        #shuffle combination matrix
        rand <- sample(ncol(comb)) 
        comb <- comb[,rand]
        
        
        #boolean of was there a swap
        swapped <- F
        
        for(s in 1:dim(comb)[2]){
          #compute original fit
          ofit <- fit(assignment, ranks)
          
          #print checking
          #print(noquote(paste("swap number:", s)))
          #print(noquote(paste("Original fit:", ofit[1], "  eq:", ofit[2])))
          
          
          #create temp assignment to swap
          tempassign <- assignment
          #create dummy
          dummy <- tempassign[comb[2,s], comb[1,s]]
          #overright first element to swap
          tempassign[comb[2,s], comb[1,s]] <- tempassign[comb[3,s], comb[1,s]]
          #overright seceond element
          tempassign[comb[3,s], comb[1,s]] <- dummy
          
          newfit <- fit(tempassign, ranks)
          
          #print checking
          #print(noquote(paste("New fit:", newfit[1], "  eq:", newfit[2], "valid", check(tempassign,rotations))))
          
          if(newfit[1] < ofit[1] && newfit[2]<= eqtol && check(tempassign,rotations)){
            assignment <- tempassign
            #print(noquote(paste("SWAPPED AT NUMBER:", s)))
            #print(noquote(assignment))
            swapped <- T
          }
        }
        
        if(!swapped){
          optimum <- T
        }
      }
      
      #print(noquote("Optimum Assignment this iteration"))
      #print(noquote(assignment))
      #print(noquote(paste("Optimum fit:", fit(assignment, ranks)[1], "  eq:", fit(assignment, ranks)[2])))
      
      if(fit(assignment, ranks)[1] < fit(final.assignment, ranks)[1]){
        final.assignment <- assignment
        
        #print(noquote("NEW FINAL ASSIGNMENT"))
        #print(noquote(final.assignment))
        #print(noquote(paste("Optimum fit:", fit(final.assignment, ranks)[1], "  eq:", fit(final.assignment, ranks)[2])))
      }
      
      fit.track$sample[x] <- x
      fit.track$fit[x] <-  fit(final.assignment, ranks)[1]
      
      if(fit(final.assignment, ranks)[1] == fitmin){
        break
      }
    }
    
    #compute and print out the rank assignments
    fits <- matrix(NA, nrow = dim(final.assignment)[1], ncol = dim(final.assignment)[2])
    for (i in 1:dim(final.assignment)[1]){
      for (r in 1:dim(final.assignment)[2]){
        fits[i,r] <- which(ranks[i,] == final.assignment[i,r], arr.ind = TRUE)
      }
    }
    
    #fit ratio - how close it is to theoretical minimum
    fit.ratio <- (sum(1:nrots)*dim(final.assignment)[1])/ fit(final.assignment, ranks)[1]
    
    #Print final assignment info
    print(noquote("FINAL ROTATION ASSIGNMENT"))
    print(noquote(matrix(paste(final.assignment, "(", fits,")", sep=""),nrow=length(rotations))))
    print(noquote(""))
    print(noquote(paste("Final Assignment fit ratio:", round(fit.ratio,2))))
    print(noquote(paste("Eqitability:", fit(final.assignment, ranks)[2])))
    
    #Output to txt
    # sink("output.txt")
    # print(noquote("FINAL ROTATION ASSIGNMENT"))
    # print(noquote(matrix(paste(final.assignment, "(", fits,")", sep=""),nrow=length(rotations))))
    # print(noquote(""))
    # print(noquote(paste("Final Assignment fit ratio:", round(fit.ratio,2))))
    # print(noquote(paste("Eqitability:", fit(final.assignment, ranks)[2])))
    # sink()
  }
  
  #if there is a PD bonus. 
  #everything the same except use fit.PD
  if(PDbonus>0){
    #need to adjust eqtol to account for PD bonus
    eqtol <- eqtol + PDbonus - 0.0001 #subtract small decimal to avoid adding a full eqtol for pdbonus of 0.5 or greater
    
    #set random initial final assignment (to be overwritten if swapping helps)
    valid<-F
    while(!valid){
      final.assignment <- matrix(NA, nrow=length(rotations), ncol=nrots)
      for (n in 1:nrots){
        final.assignment[,n] <- sample(rotations)
      }
      valid <- check(final.assignment, rotations) && fit(final.assignment,ranks)[2]<= eqtol
    }
    
    #primary loop to go through random sampling and swapping/optimizing 
    for(x in 1:samples){
      print(noquote(x))   #UNDELETE AFTER SIMULATIONS
      #set optimum to Fasle -- when this round is over
      optimum <- F
      
      #assign initial rotations at random
      valid <- F
      while(!valid){
        assignment <- matrix(NA, nrow=length(rotations), ncol=nrots)
        for (n in 1:nrots){
          assignment[,n] <- sample(rotations)
        }
        valid <- check(assignment, rotations) && fit.PD(assignment,ranks,PDbonus)[2]<= eqtol
      }
      #print(noquote(assignment))
      
      #swapping  algorithm to local optimum
      #algorithm breakdown
      #step through shuffled comb list and swap pairs within each column. 
      #see if each swap improves fit, maintaing acceptable equity, and is valid
      #if it does overright the previous assignment and break for loop
      #if not move on to next swap
      #set optimum to true if going through the whole thing doesn't improve fit. 
      
      while(!optimum){
        #make the shuffled list of combinations to swap
        comb <- do.call(cbind, replicate(dim(assignment)[2], combn(dim(assignment)[1], 2), simplify=F)) 
        
        #Assign rotation number to each
        comb <-rbind(matrix(sort(rep(1:dim(assignment)[2], choose(dim(assignment)[1],2))),nrow=1), comb)
        
        #shuffle combination matrix
        rand <- sample(ncol(comb)) 
        comb <- comb[,rand]
        
        
        #boolean of was there a swap
        swapped <- F
        
        for(s in 1:dim(comb)[2]){
          #compute original fit
          ofit <- fit.PD(assignment, ranks, PDbonus)
          
          #print checking
          #print(noquote(paste("swap number:", s)))
          #print(noquote(paste("Original fit:", ofit[1], "  eq:", ofit[2])))
          
          
          #create temp assignment to swap
          tempassign <- assignment
          #create dummy
          dummy <- tempassign[comb[2,s], comb[1,s]]
          #overright first element to swap
          tempassign[comb[2,s], comb[1,s]] <- tempassign[comb[3,s], comb[1,s]]
          #overright seceond element
          tempassign[comb[3,s], comb[1,s]] <- dummy
          
          newfit <- fit.PD(tempassign, ranks, PDbonus)
          
          #print checking
          #print(noquote(paste("New fit:", newfit[1], "  eq:", newfit[2], "valid", check(tempassign,rotations))))
          
          if(newfit[1] < ofit[1] && newfit[2]<= eqtol && check(tempassign,rotations)){
            assignment <- tempassign
            #print(noquote(paste("SWAPPED AT NUMBER:", s)))
            #print(noquote(assignment))
            swapped <- T
          }
        }
        
        if(!swapped){
          optimum <- T
        }
      }
      
      #print(noquote("Optimum Assignment this iteration"))
      #print(noquote(assignment))
      #print(noquote(paste("Optimum fit:", fit.PD(assignment, ranks, PDbonus)[1], "  eq:", fit.PD(assignment, ranks, PDbonus)[2])))
      
      if(fit.PD(assignment, ranks, PDbonus)[1] < fit.PD(final.assignment, ranks, PDbonus)[1]){
        final.assignment <- assignment
        
        #print(noquote("NEW FINAL ASSIGNMENT"))
        #print(noquote(final.assignment))
        #print(noquote(paste("Optimum fit:", fit.PD(final.assignment, ranks, PDbonus)[1], "  eq:", fit.PD(final.assignment, ranks, PDbonus)[2])))
        
      }
      
      fit.track$sample[x] <- x
      fit.track$fit[x] <-  fit(final.assignment, ranks)[1]
      
      if(fit.PD(final.assignment, ranks, PDbonus)[1] == fitmin){
        break
      }
    }
    
    #compute and print out the rank assignments
    fits <- matrix(NA, nrow = dim(final.assignment)[1], ncol = dim(final.assignment)[2])
    for (i in 1:dim(final.assignment)[1]){
      for (r in 1:dim(final.assignment)[2]){
        fits[i,r] <- which(ranks[i,] == final.assignment[i,r], arr.ind = TRUE)
      }
    }
    
    #fit ratio - how close it is to theoretical minimum
    fit.ratio <- (sum(1:nrots)*dim(final.assignment)[1])/ fit.PD(final.assignment, ranks, PDbonus)[1]
    
    #Print final assignment info
    print(noquote("FINAL ROTATION ASSIGNMENT"))
    print(noquote(matrix(paste(final.assignment, "(", fits,")", sep=""),nrow=length(rotations))))
    print(noquote(""))
    print(noquote(paste("Final Assignment fit ratio:", round(fit.ratio,2))))
    print(noquote(paste("Eqitability:", fit(final.assignment, ranks)[2])))
    
    #Output to txt
    # sink("output.txt")
    # print(noquote("FINAL ROTATION ASSIGNMENT"))
    # print(noquote(matrix(paste(final.assignment, "(", fits,")", sep=""),nrow=length(rotations))))
    # print(noquote(""))
    # print(noquote(paste("Final Assignment fit ratio:", round(fit.ratio,2))))
    # print(noquote(paste("Eqitability:", fit(final.assignment, ranks)[2])))
    # sink()
    
  }
  

  return(fit.track)
}

#Sample Data
rotations <- c('A', 'G', 'H', 'W')

assignment <- matrix(c('W', 'H', 
                       'G', 'A', 
                       'A', 'W', 
                       'H', 'G'), nrow=4, byrow=T)

assignment <- matrix(c('W', 'H', 'A',
                       'G', 'A', 'H',
                       'A', 'W', 'G',
                       'H', 'G', 'W'), nrow=4, byrow=T)

ranks <- matrix(c('A',	'H',	'G',	'W',
                  'H',	'W',	'G',	'A',
                  'A',	'W',	'H',	'G',
                  'A',	'G',	'H',	'W'), nrow=4, byrow=T)

#6 interns and rotations
rotations <- sort(c('A', 'G', 'H', 'P', 'T', 'W'))

ranks <- matrix(c('A', 'H',	'G', 'W', 'P', 'T',
                  'P', 'H',	'W', 'T',	'G', 'A',
                  'A', 'W', 'T', 'H', 'P', 'G',
                  'T', 'A',	'G', 'H', 'W', 'P',
                  'H', 'T', 'P', 'A', 'W', 'G',
                  'T', 'A',	'G', 'W', 'P', 'H'), nrow=6, byrow=T)

check(assignment, rotations)

fit(assignment, ranks)

eqtol <- 2

#check match algorithm
#works with any number of rotations
match(ranks=ranks, nrots=3, rotations=rotations, samples=500, eqtol=2, PDbonus=0)

#match works just great as it is now. 
#Still need to do




#maximizable ranks
ranks <- matrix(c('T', 'H',	'W', 'P', 'G', 'A',
                  'A', 'T',	'H', 'P', 'W', 'G',
                  'G', 'W', 'T', 'H', 'A', 'P',
                  'H', 'P', 'G', 'W', 'T', 'A',
                  'W', 'G', 'A', 'T', 'P', 'H',
                  'P', 'A', 'G', 'H', 'W', 'T'), ncol=length(rotations), byrow=T)

match(ranks=ranks, nrots=3, rotations=rotations, samples=20, eqtol=2, PDbonus=0)
#gets the same overall fit as what training staff came up with. 


#running with importing of xlsx
setwd("C:/Users/sbuja/Documents/Clinic/Internship/")
rotations <- sort(as.character(read.csv("Rotations.csv", header=F)$V1))
ranks <- as.matrix(read.csv("Rankings.csv", header=F))
colnames(ranks) <- NULL

#rotations <- sort(as.character(read.xlsx("Internship_Rotations_and_Rankings.xlsx", sheetName="Rotations", header=F)$X1))

ranks <- as.matrix(read.xlsx("Internship_Rotations_and_Rankings.xlsx", sheetName="Ranks", header=F))
colnames(ranks) <- NULL

specs <- read.xlsx("Internship_Rotations_and_Rankings.xlsx", sheetName="Specs")


match(ranks=ranks, 
      rotations=rotations,
      nrots=specs$nrots,  
      samples=specs$samples, 
      eqtol=2, 
      PDbonus=specs$PDBonus)


#diagnostics of match
samples=2000
sim=100
nrots=2

fit.track.sim <- data.frame(sim=NA, sample=NA, fit=NA)
for(y in (1:sim)){
  #shuffle ranking matrix within each intern
  sim.ranks <- ranks[sample(dim(ranks)[1]),sample(sample(dim(ranks)[1]))]
  
  fit.track <- match(ranks=sim.ranks, nrots=nrots, rotations=rotations, samples=samples, eqtol=2, PDbonus=0.5)
  fit.track$sim <- y
  fit.track.sim <- rbind(fit.track.sim, fit.track)
  
  print(noquote(paste("sim number", y)))
}

fit.track.sim <-na.exclude(fit.track.sim)

matching.plot <- ggplot(fit.track.sim, aes(x=sample, y=fit, colour=as.factor(sim))) + geom_line() + 
  annotate("text", label=paste(sim, "Simulations\n", dim(ranks)[1], "Interns\n",  nrots, "Rotations per intern"), 
           x=samples/2, y=max(fit.track.sim$fit), size=6, fontface="bold", hjust=.5, vjust=1) + 
  scale_x_continuous("Sampling Number") + scale_y_continuous("Fit Index (Lower is better)")+
  Sp.Theme()
matching.plot

ggsave(matching.plot, filename="matching.plot.png", width = 6, height=4.5, dpi=500)


#able to identify perfect matches?
samples=3000
sim=500
nrots=2
rotations <- sort(c('A', 'G', 'H', 'P', 'T', 'W'))

fit.perfect.sim <- rep(NA,sim)

for(y in (1:sim)){
  #make a perfectly matchable ranks
  sim.ranks <- matrix(NA,nrow=6, ncol=6)
  sim.ranks[,1] <- sample(rotations)
  valid <- F
  while(!valid){
    sim.ranks[,2] <- sample(rotations)
    valid <- all(!(sim.ranks[,1] == sim.ranks[,2]))
  }
  #fill in ranks for others one intern at a time
  for(r in 1:6){
    remove <- sim.ranks[r,1:2]
    sim.ranks[r,3:6] <- sample(setdiff(rotations, remove))
  }
  
  fit.track <- na.exclude(match(ranks=sim.ranks, nrots=nrots, rotations=rotations, samples=samples, eqtol=2, PDbonus=0))
  
  fit.perfect.sim[y] <- min(fit.track$fit)
  
  print(noquote(sim.ranks))
  print(noquote(paste("sim number", y)))
}

table(fit.perfect.sim)


#sample with the actual 2017-2018 class
rotations <- sort(c('A', 'G', 'H', 'P', 'T', 'W'))
#actual ranks
ranks <- matrix(c('T', 'H',	'W', 'P', 'G', 'A',
                  'A', 'T',	'H', 'P',	'W', 'G',
                  'G', 'W', 'T', 'H', 'A', 'P',
                  'H', 'P', 'G', 'W', 'T', 'A',
                  'W', 'T', 'A', 'G', 'P', 'H',
                  'W', 'A', 'G', 'H', 'P', 'T'), ncol=length(rotations), byrow=T)

#for presentation
rotations <- sort(c('A', 'G', 'H', 'P', 'T', 'W'))
ranks <- matrix(c('T', 'H',	'W', 'P', 'G', 'A',
                  'T', 'A',	'H', 'P', 'W', 'G',
                  'G', 'W', 'P', 'H', 'A', 'T',
                  'H', 'G', 'P', 'W', 'T', 'A',
                  'W', 'G', 'A', 'T', 'P', 'H',
                  'P', 'A', 'T', 'H', 'W', 'G'), ncol=length(rotations), byrow=T)


test <- match(ranks=ranks, rotations=rotations,
      nrots=3, samples=1000, eqtol=2, PDbonus=0.5)

