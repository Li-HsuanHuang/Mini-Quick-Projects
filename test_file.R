# Create 4 classes 

obs = sample(1:17,500,replace = T)
ind = 1:200
train = obs[ind]
test = obs[-ind]

# Create count vector for each class
prob_vec = numeric(17)
for (i in train){
    prob_vec[i] = prob_vec[i] + 1
}
print(prob_vec)

# Calculate probability distribution based on the training set.
prob_dist = prob_vec/sum(prob_vec)
print(prob_dist)

# Make a prediction based on prob_dist!
pred = sample(1:4,1,prob = prob_dist)

# Update count and probability
prob_dist = function(equilib,new_obs){
  equilib[new_obs] = equilib[new_obs] + 1
  return(equilib/sum(equilib))
}

# Make prediction!
predictions = numeric(length(test))
k = 1
for (i in test){
  predictions[k] = sample(1:17,1,prob=prob_dist(prob_vec,test[i]))
  k = k + 1
}

mean(predictions!=test)
