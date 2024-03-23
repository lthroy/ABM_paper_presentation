library(rjags)

mus <- c(1.53,2.851,3.779)
sigma <- 0.35
a_vec <- c()
J_vec <- c()
y_vec <- c()

getAccProb <- function(probs_a){
  probs_a = probs_a / sum(probs_a)
  for (i in 2:length(probs_a)){
    probs_a[i] = probs_a[i-1]+probs_a[i]
  }
  probs_a
}

probs <- rbind(c(0.1,0.3,0.6),c(0.25,0.4,0.35),c(0.05,0.15,0.8))
#generate some data from an age-specific gaussian mixture model
for (a in 1:3){
  acc_probs <- getAccProb(probs[a,])
  for (i in 1:100){
    r <- runif(1)
    
    
    J = -1
    for (j in 1:length(acc_probs)){
      if (r <= acc_probs[j]){
        J = j;
        break;
      }
    }
    y <- rnorm(1,mus[J],sigma)
    a_vec <- append(a_vec,a)
    J_vec <- append(J_vec,J)
    y_vec <- append(y_vec,y)
  }
  
}

df <- data.frame(a=a_vec,J=J_vec,y=y_vec)

inits1 <- list(
  list(p=matrix(rep(1/3,9),3,3),tau=1)
  
)
m2 <- jags.model("paper.bug",df,inits1,n.chains=1,n.adapt=2000)
update(m2,1000) #burin-in
x2 <- coda.samples(m2,c("mu","sigma","T"),n.iter=20000)
errors = 0
for (i in 1:300){
  Js <- x2[,paste0("T[",i,"]")]
  # Count occurrences of each category
  counts <- table(Js)
  
  # Find the index of the category with the highest count
  largest_category <- as.numeric(names(counts)[which.max(counts)])
  
  if (largest_category != df[i,"J"]){
    print(paste("person=",i,"predicted cluser=",largest_category,"actual cluster=",df[i,"J"]))
    errors = errors + 1
  }
}
print(paste("Accuracy=",1-errors/300,paste0(300-errors,"/",300)))