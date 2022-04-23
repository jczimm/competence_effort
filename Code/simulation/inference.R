library('rwebppl')
library('tidyverse')

## Joint
sim <- data.frame(model = 'joint',
                  contest = c(rep(1,6),rep(2,6),rep(3,6),rep(4,6),rep(5,6),rep(6,6)), 
                  agent = rep(c('A','B'), 18), 
                  scenario = c(rep('F,F;F,F',6),rep('F,F;F,L',6),rep('F,L;F,L',6),rep('F,F;L,L',6),rep('F,L;L,L',6),rep('L,L;L,L',6)),
                  effort = 0, strength = 0,
                  outcome = 0, prob = 0, round = rep(c(1,1,2,2,3,3), 6), 
                  reward = rep(c(10,10,20,20,20,20), 6))
sim$prob[sim$round==1] = NaN
sim$outcome = c(integer(6), integer(3),1,integer(2), 0,1,0,1,1,1, integer(2),integer(4)+1, 0,integer(5)+1, integer(6)+1)


########################################## R1 Effort Strength & R2 Probability ##########################################
# F,F
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1] // all possible efforts
var lift = function(strength,box,effort){ // lift function - when S*E is greater than or equal to box weight, the outcome is lift
  return (effort*strength >= weight(box))
} 

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        // marginalization by plugging the strength samples into the lift function one by one, then take the mean and minus the cost (alpha*effort)
        else
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))} // lift probability
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x1a = [], x1b = [] // arrays of strength samples
var samples1 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10) // draw strength samples from a uniform prior
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false) // condiition on the observations
  condition(outcome(sb,'box',lowR) == false)
  x1a.push(sa)
  x1b.push(sb)
  return 0
}})

var output = {aE: optE(x1a,'box',lowR), bE: optE(x1b,'box',lowR), aS: listMean(x1a), bS: listMean(x1b), aP: outcome(x1a,'box',highR), bP: outcome(x1b,'box',highR)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==1 & sim$agent=='A' & sim$scenario!='F,L;F,L' & sim$scenario!='F,L;L,L' & sim$scenario!='L,L;L,L'] <- a$aE*100
sim$effort[sim$round==1 & sim$agent=='B' & sim$scenario!='F,L;F,L' & sim$scenario!='F,L;L,L' & sim$scenario!='L,L;L,L'] <- a$bE*100
sim$strength[sim$round==1 & sim$agent=='A' & sim$scenario!='F,L;F,L' & sim$scenario!='F,L;L,L' & sim$scenario!='L,L;L,L'] <- a$aS
sim$strength[sim$round==1 & sim$agent=='B' & sim$scenario!='F,L;F,L' & sim$scenario!='F,L;L,L' & sim$scenario!='L,L;L,L'] <- a$bS
sim$prob[sim$round==2 & sim$agent=='A' & sim$scenario!='F,L;F,L' & sim$scenario!='F,L;L,L' & sim$scenario!='L,L;L,L'] <- a$aP*100
sim$prob[sim$round==2 & sim$agent=='B' & sim$scenario!='F,L;F,L' & sim$scenario!='F,L;L,L' & sim$scenario!='L,L;L,L'] <- a$bP*100


# F,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x1a = [], x1b = []
var samples1 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == true)
  x1a.push(sa)
  x1b.push(sb)
  return 0
}})

var output = {aE: optE(x1a,'box',lowR), bE: optE(x1b,'box',lowR), aS: listMean(x1a), bS: listMean(x1b), aP: outcome(x1a,'box',highR), bP: outcome(x1b,'box',highR,5)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==1 & sim$agent=='A' & sim$scenario!='F,F;F,F' & sim$scenario!='F,F;F,L' & sim$scenario!='F,F;L,L' & sim$scenario!='L,L;L,L'] <- a$aE*100
sim$effort[sim$round==1 & sim$agent=='B' & sim$scenario!='F,F;F,F' & sim$scenario!='F,F;F,L' & sim$scenario!='F,F;L,L' & sim$scenario!='L,L;L,L'] <- a$bE*100
sim$strength[sim$round==1 & sim$agent=='A' & sim$scenario!='F,F;F,F' & sim$scenario!='F,F;F,L' & sim$scenario!='F,F;L,L' & sim$scenario!='L,L;L,L'] <- a$aS
sim$strength[sim$round==1 & sim$agent=='B' & sim$scenario!='F,F;F,F' & sim$scenario!='F,F;F,L' & sim$scenario!='F,F;L,L' & sim$scenario!='L,L;L,L'] <- a$bS
sim$prob[sim$round==2 & sim$agent=='A' & sim$scenario!='F,F;F,F' & sim$scenario!='F,F;F,L' & sim$scenario!='F,F;L,L' & sim$scenario!='L,L;L,L'] <- a$aP*100
sim$prob[sim$round==2 & sim$agent=='B' & sim$scenario!='F,F;F,F' & sim$scenario!='F,F;F,L' & sim$scenario!='F,F;L,L' & sim$scenario!='L,L;L,L'] <- a$bP*100


# L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x1a = [], x1b = []
var samples1 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == true)
  condition(outcome(sb,'box',lowR) == true)
  x1a.push(sa)
  x1b.push(sb)
  return 0
}})

var output = {aE: optE(x1a,'box',lowR), bE: optE(x1b,'box',lowR), aS: listMean(x1a), bS: listMean(x1b), aP: outcome(x1a,'box',highR), bP: outcome(x1b,'box',highR)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==1 & sim$agent=='A' & sim$scenario=='L,L;L,L'] <- a$aE*100
sim$effort[sim$round==1 & sim$agent=='B' & sim$scenario=='L,L;L,L'] <- a$bE*100
sim$strength[sim$round==1 & sim$agent=='A' & sim$scenario=='L,L;L,L'] <- a$aS
sim$strength[sim$round==1 & sim$agent=='B' & sim$scenario=='L,L;L,L'] <- a$bS
sim$prob[sim$round==2 & sim$agent=='A' & sim$scenario=='L,L;L,L'] <- a$aP*100
sim$prob[sim$round==2 & sim$agent=='B' & sim$scenario=='L,L;L,L'] <- a$bP*100

########################################## R2 Effort Strength ##########################################
# F,F;F,F
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == false)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var output = {aE: optE(x2a,'box',highR), bE: optE(x2b,'box',highR), aS: listMean(x2a), bS: listMean(x2b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==2 & sim$agent=='A' & sim$scenario=='F,F;F,F'] <- a$aE*100
sim$effort[sim$round==2 & sim$agent=='B' & sim$scenario=='F,F;F,F'] <- a$bE*100
sim$strength[sim$round==2 & sim$agent=='A' & sim$scenario=='F,F;F,F'] <- a$aS
sim$strength[sim$round==2 & sim$agent=='B' & sim$scenario=='F,F;F,F'] <- a$bS


# F,F;F,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var output = {aE: optE(x2a,'box',highR), bE: optE(x2b,'box',highR), aS: listMean(x2a), bS: listMean(x2b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==2 & sim$agent=='A' & sim$scenario=='F,F;F,L'] <- a$aE*100
sim$effort[sim$round==2 & sim$agent=='B' & sim$scenario=='F,F;F,L'] <- a$bE*100
sim$strength[sim$round==2 & sim$agent=='A' & sim$scenario=='F,F;F,L'] <- a$aS
sim$strength[sim$round==2 & sim$agent=='B' & sim$scenario=='F,F;F,L'] <- a$bS


# F,L;F,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var output = {aE: optE(x2a,'box',highR), bE: optE(x2b,'box',highR), aS: listMean(x2a), bS: listMean(x2b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==2 & sim$agent=='A' & sim$scenario=='F,L;F,L'] <- a$aE*100
sim$effort[sim$round==2 & sim$agent=='B' & sim$scenario=='F,L;F,L'] <- a$bE*100
sim$strength[sim$round==2 & sim$agent=='A' & sim$scenario=='F,L;F,L'] <- a$aS
sim$strength[sim$round==2 & sim$agent=='B' & sim$scenario=='F,L;F,L'] <- a$bS


# F,F;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var output = {aE: optE(x2a,'box',highR), bE: optE(x2b,'box',highR), aS: listMean(x2a), bS: listMean(x2b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==2 & sim$agent=='A' & sim$scenario=='F,F;L,L'] <- a$aE*100
sim$effort[sim$round==2 & sim$agent=='B' & sim$scenario=='F,F;L,L'] <- a$bE*100
sim$strength[sim$round==2 & sim$agent=='A' & sim$scenario=='F,F;L,L'] <- a$aS
sim$strength[sim$round==2 & sim$agent=='B' & sim$scenario=='F,F;L,L'] <- a$bS


# F,L;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var output = {aE: optE(x2a,'box',highR), bE: optE(x2b,'box',highR), aS: listMean(x2a), bS: listMean(x2b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==2 & sim$agent=='A' & sim$scenario=='F,L;L,L'] <- a$aE*100
sim$effort[sim$round==2 & sim$agent=='B' & sim$scenario=='F,L;L,L'] <- a$bE*100
sim$strength[sim$round==2 & sim$agent=='A' & sim$scenario=='F,L;L,L'] <- a$aS
sim$strength[sim$round==2 & sim$agent=='B' & sim$scenario=='F,L;L,L'] <- a$bS


# L,L;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == true)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var output = {aE: optE(x2a,'box',highR), bE: optE(x2b,'box',highR), aS: listMean(x2a), bS: listMean(x2b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==2 & sim$agent=='A' & sim$scenario=='L,L;L,L'] <- a$aE*100
sim$effort[sim$round==2 & sim$agent=='B' & sim$scenario=='L,L;L,L'] <- a$bE*100
sim$strength[sim$round==2 & sim$agent=='A' & sim$scenario=='L,L;L,L'] <- a$aS
sim$strength[sim$round==2 & sim$agent=='B' & sim$scenario=='L,L;L,L'] <- a$bS


########################################## R3 Probability ##########################################
# F,F;F,F
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == false)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var jointUtility = function(init_effort,a_strength,b_strength){
  var r3_reward = highR // round 3 reward
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }

  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(depth,reward) {
      var effort2 = b(depth - 1,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(depth,reward) {
      var effort2 = depth===0 ? init_effort : a(depth,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var findDepth = function(x) { // find the depth that is needed to converge
     if (b(x,r3_reward) == b(x+1,r3_reward)) {
       return x;
     } else {
       return -1;
     }
   };

  var ds = [1,2,5,10]; // if converges in 1 round, then depth = 1; if not, then try 2, 5, 10.
  var d = function() {
     if (findDepth(ds[0]) > 0) {
       return ds[0]
     } else if (findDepth(ds[1]) > 0) {
       return ds[1]
     } else if (findDepth(ds[2]) > 0) {
       return ds[2]
     } else if (findDepth(ds[3]) > 0) {
       return ds[3]
     } else {
       display('Effort could not converge in ' + ds[3] + ' iterations. Increase the number of iterations and try again.')
     }
   };

  var depth = d()
  var aE = a(depth+1,r3_reward)
  var bE = b(depth,r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  // calculate agents' utility
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

// find the intial effort that maximizes the joint utility
var startingEffort = function(a_strength,b_strength) {
  return argMax(
    function(init_effort) {
      var tbl = jointUtility(init_effort,a_strength,b_strength)
      return tbl.jointU
    },
    efforts);
};

var startingE = startingEffort(x2a,x2b)
var output = {P: jointUtility(startingE,x2a,x2b).outcome}
output
"
a <- webppl(mdl)
sim$prob[sim$round==3 & sim$scenario=='F,F;F,F'] <- a$P*100


# F,F;F,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var jointUtility = function(init_effort,a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }

  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(depth,reward) {
      var effort2 = b(depth - 1,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(depth,reward) {
      var effort2 = depth===0 ? init_effort : a(depth,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var findDepth = function(x) {
     if (b(x,r3_reward) == b(x+1,r3_reward)) {
       return x;
     } else {
       return -1;
     }
   };

  var ds = [1,2,5,10];
  var d = function() {
     if (findDepth(ds[0]) > 0) {
       return ds[0]
     } else if (findDepth(ds[1]) > 0) {
       return ds[1]
     } else if (findDepth(ds[2]) > 0) {
       return ds[2]
     } else if (findDepth(ds[3]) > 0) {
       return ds[3]
     } else {
       display('Effort could not converge in ' + ds[3] + ' iterations. Increase the number of iterations and try again.')
     }
   };

  var depth = d()
  var aE = a(depth+1,r3_reward)
  var bE = b(depth,r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var startingEffort = function(a_strength,b_strength) {
  return argMax(
    function(init_effort) {
      var tbl = jointUtility(init_effort,a_strength,b_strength)
      return tbl.jointU
    },
    efforts);
};

var startingE = startingEffort(x2a,x2b)
var output = {P: jointUtility(startingE,x2a,x2b).outcome}
output
"
a <- webppl(mdl)
sim$prob[sim$round==3 & sim$scenario=='F,F;F,L'] <- a$P*100


# F,L;F,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var jointUtility = function(init_effort,a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }

  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(depth,reward) {
      var effort2 = b(depth - 1,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(depth,reward) {
      var effort2 = depth===0 ? init_effort : a(depth,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var findDepth = function(x) {
     if (b(x,r3_reward) == b(x+1,r3_reward)) {
       return x;
     } else {
       return -1;
     }
   };

  var ds = [1,2,5,10];
  var d = function() {
     if (findDepth(ds[0]) > 0) {
       return ds[0]
     } else if (findDepth(ds[1]) > 0) {
       return ds[1]
     } else if (findDepth(ds[2]) > 0) {
       return ds[2]
     } else if (findDepth(ds[3]) > 0) {
       return ds[3]
     } else {
       display('Effort could not converge in ' + ds[3] + ' iterations. Increase the number of iterations and try again.')
     }
   };

  var depth = d()
  var aE = a(depth+1,r3_reward)
  var bE = b(depth,r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var startingEffort = function(a_strength,b_strength) {
  return argMax(
    function(init_effort) {
      var tbl = jointUtility(init_effort,a_strength,b_strength)
      return tbl.jointU
    },
    efforts);
};

var startingE = startingEffort(x2a,x2b)
var output = {P: jointUtility(startingE,x2a,x2b).outcome}
output
"
a <- webppl(mdl)
sim$prob[sim$round==3 & sim$scenario=='F,L;F,L'] <- a$P*100


# F,F;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var jointUtility = function(init_effort,a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }

  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(depth,reward) {
      var effort2 = b(depth - 1,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(depth,reward) {
      var effort2 = depth===0 ? init_effort : a(depth,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var findDepth = function(x) {
     if (b(x,r3_reward) == b(x+1,r3_reward)) {
       return x;
     } else {
       return -1;
     }
   };

  var ds = [1,2,5,10];
  var d = function() {
     if (findDepth(ds[0]) > 0) {
       return ds[0]
     } else if (findDepth(ds[1]) > 0) {
       return ds[1]
     } else if (findDepth(ds[2]) > 0) {
       return ds[2]
     } else if (findDepth(ds[3]) > 0) {
       return ds[3]
     } else {
       display('Effort could not converge in ' + ds[3] + ' iterations. Increase the number of iterations and try again.')
     }
   };

  var depth = d()
  var aE = a(depth+1,r3_reward)
  var bE = b(depth,r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var startingEffort = function(a_strength,b_strength) {
  return argMax(
    function(init_effort) {
      var tbl = jointUtility(init_effort,a_strength,b_strength)
      return tbl.jointU
    },
    efforts);
};

var startingE = startingEffort(x2a,x2b)
var output = {P: jointUtility(startingE,x2a,x2b).outcome}
output
"
a <- webppl(mdl)
sim$prob[sim$round==3 & sim$scenario=='F,F;L,L'] <- a$P*100


# F,L;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var jointUtility = function(init_effort,a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }

  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(depth,reward) {
      var effort2 = b(depth - 1,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(depth,reward) {
      var effort2 = depth===0 ? init_effort : a(depth,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var findDepth = function(x) {
     if (b(x,r3_reward) == b(x+1,r3_reward)) {
       return x;
     } else {
       return -1;
     }
   };

  var ds = [1,2,5,10];
  var d = function() {
     if (findDepth(ds[0]) > 0) {
       return ds[0]
     } else if (findDepth(ds[1]) > 0) {
       return ds[1]
     } else if (findDepth(ds[2]) > 0) {
       return ds[2]
     } else if (findDepth(ds[3]) > 0) {
       return ds[3]
     } else {
       display('Effort could not converge in ' + ds[3] + ' iterations. Increase the number of iterations and try again.')
     }
   };

  var depth = d()
  var aE = a(depth+1,r3_reward)
  var bE = b(depth,r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var startingEffort = function(a_strength,b_strength) {
  return argMax(
    function(init_effort) {
      var tbl = jointUtility(init_effort,a_strength,b_strength)
      return tbl.jointU
    },
    efforts);
};

var startingE = startingEffort(x2a,x2b)
var output = {P: jointUtility(startingE,x2a,x2b).outcome}
output
"
a <- webppl(mdl)
sim$prob[sim$round==3 & sim$scenario=='F,L;L,L'] <- a$P*100


# L,L;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == true)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var jointUtility = function(init_effort,a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }

  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(depth,reward) {
      var effort2 = b(depth - 1,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(depth,reward) {
      var effort2 = depth===0 ? init_effort : a(depth,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var findDepth = function(x) {
     if (b(x,r3_reward) == b(x+1,r3_reward)) {
       return x;
     } else {
       return -1;
     }
   };

  var ds = [1,2,5,10];
  var d = function() {
     if (findDepth(ds[0]) > 0) {
       return ds[0]
     } else if (findDepth(ds[1]) > 0) {
       return ds[1]
     } else if (findDepth(ds[2]) > 0) {
       return ds[2]
     } else if (findDepth(ds[3]) > 0) {
       return ds[3]
     } else {
       display('Effort could not converge in ' + ds[3] + ' iterations. Increase the number of iterations and try again.')
     }
   };

  var depth = d()
  var aE = a(depth+1,r3_reward)
  var bE = b(depth,r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var startingEffort = function(a_strength,b_strength) {
  return argMax(
    function(init_effort) {
      var tbl = jointUtility(init_effort,a_strength,b_strength)
      return tbl.jointU
    },
    efforts);
};

var startingE = startingEffort(x2a,x2b)
var output = {P: jointUtility(startingE,x2a,x2b).outcome}
output
"
a <- webppl(mdl)
sim$prob[sim$round==3 & sim$scenario=='L,L;L,L'] <- a$P*100



########################################## R3 Effort Strength ##########################################
# F,F;F,F
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var jointUtility = function(init_effort,a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }

  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(depth,reward) {
      var effort2 = b(depth - 1,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(depth,reward) {
      var effort2 = depth===0 ? init_effort : a(depth,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var findDepth = function(x) {
     if (b(x,r3_reward) == b(x+1,r3_reward)) {
       return x;
     } else {
       return -1;
     }
   };

  var ds = [1,2,5,10];
  var d = function() {
     if (findDepth(ds[0]) > 0) {
       return ds[0]
     } else if (findDepth(ds[1]) > 0) {
       return ds[1]
     } else if (findDepth(ds[2]) > 0) {
       return ds[2]
     } else if (findDepth(ds[3]) > 0) {
       return ds[3]
     } else {
       display('Effort could not converge in ' + ds[3] + ' iterations. Increase the number of iterations and try again.')
     }
   };

  var depth = d()
  var aE = a(depth+1,r3_reward)
  var bE = b(depth,r3_reward)

  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var startingEffort = function(a_strength,b_strength) {
  return argMax(
    function(init_effort) {
      var tbl = jointUtility(init_effort,a_strength,b_strength)
      return tbl.jointU
    },
    efforts);
};

var x3a = [], x3b = []
var samples3 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == false)
  condition(jointUtility(startingEffort(sa,sb),sa,sb).outcome == false)
  x3a.push(sa)
  x3b.push(sb)
  return 0
}})

var startingE = startingEffort(x3a,x3b)
var ju = jointUtility(startingE,x3a,x3b)

var output = {aE: ju.aE, bE: ju.bE, aS: listMean(x3a), bS: listMean(x3b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==3 & sim$agent=='A' & sim$scenario=='F,F;F,F'] <- a$aE*100
sim$effort[sim$round==3 & sim$agent=='B' & sim$scenario=='F,F;F,F'] <- a$bE*100
sim$strength[sim$round==3 & sim$agent=='A' & sim$scenario=='F,F;F,F'] <- a$aS
sim$strength[sim$round==3 & sim$agent=='B' & sim$scenario=='F,F;F,F'] <- a$bS


# F,F;F,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var jointUtility = function(init_effort,a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }

  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(depth,reward) {
      var effort2 = b(depth - 1,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(depth,reward) {
      var effort2 = depth===0 ? init_effort : a(depth,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var findDepth = function(x) {
     if (b(x,r3_reward) == b(x+1,r3_reward)) {
       return x;
     } else {
       return -1;
     }
   };

  var ds = [1,2,5,10];
  var d = function() {
     if (findDepth(ds[0]) > 0) {
       return ds[0]
     } else if (findDepth(ds[1]) > 0) {
       return ds[1]
     } else if (findDepth(ds[2]) > 0) {
       return ds[2]
     } else if (findDepth(ds[3]) > 0) {
       return ds[3]
     } else {
       display('Effort could not converge in ' + ds[3] + ' iterations. Increase the number of iterations and try again.')
     }
   };

  var depth = d()
  var aE = a(depth+1,r3_reward)
  var bE = b(depth,r3_reward)

  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var startingEffort = function(a_strength,b_strength) {
  return argMax(
    function(init_effort) {
      var tbl = jointUtility(init_effort,a_strength,b_strength)
      return tbl.jointU
    },
    efforts);
};

var x3a = [], x3b = []
var samples3 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == true)
  condition(jointUtility(startingEffort(sa,sb),sa,sb).outcome == false)
  x3a.push(sa)
  x3b.push(sb)
  return 0
}})

var startingE = startingEffort(x3a,x3b)
var ju = jointUtility(startingE,x3a,x3b)

var output = {aE: ju.aE, bE: ju.bE, aS: listMean(x3a), bS: listMean(x3b)}
output
"
# a <- webppl(mdl)
# The model predicts that F,F;F,L + Round 3 Fail is not possible

sim$effort[sim$round==3 & sim$agent=='A' & sim$scenario=='F,F;F,L'] <- NaN
sim$effort[sim$round==3 & sim$agent=='B' & sim$scenario=='F,F;F,L'] <- NaN
sim$strength[sim$round==3 & sim$agent=='A' & sim$scenario=='F,F;F,L'] <- NaN
sim$strength[sim$round==3 & sim$agent=='B' & sim$scenario=='F,F;F,L'] <- NaN


# F,L;F,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var jointUtility = function(init_effort,a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }

  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(depth,reward) {
      var effort2 = b(depth - 1,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(depth,reward) {
      var effort2 = depth===0 ? init_effort : a(depth,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var findDepth = function(x) {
     if (b(x,r3_reward) == b(x+1,r3_reward)) {
       return x;
     } else {
       return -1;
     }
   };

  var ds = [1,2,5,10];
  var d = function() {
     if (findDepth(ds[0]) > 0) {
       return ds[0]
     } else if (findDepth(ds[1]) > 0) {
       return ds[1]
     } else if (findDepth(ds[2]) > 0) {
       return ds[2]
     } else if (findDepth(ds[3]) > 0) {
       return ds[3]
     } else {
       display('Effort could not converge in ' + ds[3] + ' iterations. Increase the number of iterations and try again.')
     }
   };

  var depth = d()
  var aE = a(depth+1,r3_reward)
  var bE = b(depth,r3_reward)

  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var startingEffort = function(a_strength,b_strength) {
  return argMax(
    function(init_effort) {
      var tbl = jointUtility(init_effort,a_strength,b_strength)
      return tbl.jointU
    },
    efforts);
};

var x3a = [], x3b = []
var samples3 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == true)
  condition(jointUtility(startingEffort(sa,sb),sa,sb).outcome == true)
  x3a.push(sa)
  x3b.push(sb)
  return 0
}})

var startingE = startingEffort(x3a,x3b)
var ju = jointUtility(startingE,x3a,x3b)

var output = {aE: ju.aE, bE: ju.bE, aS: listMean(x3a), bS: listMean(x3b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==3 & sim$agent=='A' & sim$scenario=='F,L;F,L'] <- a$aE*100
sim$effort[sim$round==3 & sim$agent=='B' & sim$scenario=='F,L;F,L'] <- a$bE*100
sim$strength[sim$round==3 & sim$agent=='A' & sim$scenario=='F,L;F,L'] <- a$aS
sim$strength[sim$round==3 & sim$agent=='B' & sim$scenario=='F,L;F,L'] <- a$bS


# F,F;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var jointUtility = function(init_effort,a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }

  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(depth,reward) {
      var effort2 = b(depth - 1,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(depth,reward) {
      var effort2 = depth===0 ? init_effort : a(depth,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var findDepth = function(x) {
     if (b(x,r3_reward) == b(x+1,r3_reward)) {
       return x;
     } else {
       return -1;
     }
   };

  var ds = [1,2,5,10];
  var d = function() {
     if (findDepth(ds[0]) > 0) {
       return ds[0]
     } else if (findDepth(ds[1]) > 0) {
       return ds[1]
     } else if (findDepth(ds[2]) > 0) {
       return ds[2]
     } else if (findDepth(ds[3]) > 0) {
       return ds[3]
     } else {
       display('Effort could not converge in ' + ds[3] + ' iterations. Increase the number of iterations and try again.')
     }
   };

  var depth = d()
  var aE = a(depth+1,r3_reward)
  var bE = b(depth,r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var startingEffort = function(a_strength,b_strength) {
  return argMax(
    function(init_effort) {
      var tbl = jointUtility(init_effort,a_strength,b_strength)
      return tbl.jointU
    },
    efforts);
};

var x3a = [], x3b = []
var samples3 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  condition(jointUtility(startingEffort(sa,sb),sa,sb).outcome == true)
  x3a.push(sa)
  x3b.push(sb)
  return 0
}})

var startingE = startingEffort(x3a,x3b)
var ju = jointUtility(startingE,x3a,x3b)

var output = {aE: ju.aE, bE: ju.bE, aS: listMean(x3a), bS: listMean(x3b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==3 & sim$agent=='A' & sim$scenario=='F,F;L,L'] <- a$aE*100
sim$effort[sim$round==3 & sim$agent=='B' & sim$scenario=='F,F;L,L'] <- a$bE*100
sim$strength[sim$round==3 & sim$agent=='A' & sim$scenario=='F,F;L,L'] <- a$aS
sim$strength[sim$round==3 & sim$agent=='B' & sim$scenario=='F,F;L,L'] <- a$bS


# F,L;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var jointUtility = function(init_effort,a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }

  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(depth,reward) {
      var effort2 = b(depth - 1,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(depth,reward) {
      var effort2 = depth===0 ? init_effort : a(depth,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var findDepth = function(x) {
     if (b(x,r3_reward) == b(x+1,r3_reward)) {
       return x;
     } else {
       return -1;
     }
   };

  var ds = [1,2,5,10];
  var d = function() {
     if (findDepth(ds[0]) > 0) {
       return ds[0]
     } else if (findDepth(ds[1]) > 0) {
       return ds[1]
     } else if (findDepth(ds[2]) > 0) {
       return ds[2]
     } else if (findDepth(ds[3]) > 0) {
       return ds[3]
     } else {
       display('Effort could not converge in ' + ds[3] + ' iterations. Increase the number of iterations and try again.')
     }
   };

  var depth = d()
  var aE = a(depth+1,r3_reward)
  var bE = b(depth,r3_reward)

  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var startingEffort = function(a_strength,b_strength) {
  return argMax(
    function(init_effort) {
      var tbl = jointUtility(init_effort,a_strength,b_strength)
      return tbl.jointU
    },
    efforts);
};

var x3a = [], x3b = []
var samples3 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  condition(jointUtility(startingEffort(sa,sb),sa,sb).outcome == true)
  x3a.push(sa)
  x3b.push(sb)
  return 0
}})

var startingE = startingEffort(x3a,x3b)
var ju = jointUtility(startingE,x3a,x3b)

var output = {aE: ju.aE, bE: ju.bE, aS: listMean(x3a), bS: listMean(x3b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==3 & sim$agent=='A' & sim$scenario=='F,L;L,L'] <- a$aE*100
sim$effort[sim$round==3 & sim$agent=='B' & sim$scenario=='F,L;L,L'] <- a$bE*100
sim$strength[sim$round==3 & sim$agent=='A' & sim$scenario=='F,L;L,L'] <- a$aS
sim$strength[sim$round==3 & sim$agent=='B' & sim$scenario=='F,L;L,L'] <- a$bS


# L,L;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var jointUtility = function(init_effort,a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }

  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(depth,reward) {
      var effort2 = b(depth - 1,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(depth,reward) {
      var effort2 = depth===0 ? init_effort : a(depth,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var findDepth = function(x) {
     if (b(x,r3_reward) == b(x+1,r3_reward)) {
       return x;
     } else {
       return -1;
     }
   };

  var ds = [1,2,5,10];
  var d = function() {
     if (findDepth(ds[0]) > 0) {
       return ds[0]
     } else if (findDepth(ds[1]) > 0) {
       return ds[1]
     } else if (findDepth(ds[2]) > 0) {
       return ds[2]
     } else if (findDepth(ds[3]) > 0) {
       return ds[3]
     } else {
       display('Effort could not converge in ' + ds[3] + ' iterations. Increase the number of iterations and try again.')
     }
   };

  var depth = d()
  var aE = a(depth+1,r3_reward)
  var bE = b(depth,r3_reward)

  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var startingEffort = function(a_strength,b_strength) {
  return argMax(
    function(init_effort) {
      var tbl = jointUtility(init_effort,a_strength,b_strength)
      return tbl.jointU
    },
    efforts);
};

var x3a = [], x3b = []
var samples3 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == true)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  condition(jointUtility(startingEffort(sa,sb),sa,sb).outcome == true)
  x3a.push(sa)
  x3b.push(sb)
  return 0
}})

var startingE = startingEffort(x3a,x3b)
var ju = jointUtility(startingE,x3a,x3b)

var output = {aE: ju.aE, bE: ju.bE, aS: listMean(x3a), bS: listMean(x3b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==3 & sim$agent=='A' & sim$scenario=='L,L;L,L'] <- a$aE*100
sim$effort[sim$round==3 & sim$agent=='B' & sim$scenario=='L,L;L,L'] <- a$bE*100
sim$strength[sim$round==3 & sim$agent=='A' & sim$scenario=='L,L;L,L'] <- a$aS
sim$strength[sim$round==3 & sim$agent=='B' & sim$scenario=='L,L;L,L'] <- a$bS

joint <- sim


## Solitary
sim <- data.frame(model = 'solitary',
                  contest = c(rep(1,6),rep(2,6),rep(3,6),rep(4,6),rep(5,6),rep(6,6)), 
                  agent = rep(c('A','B'), 18), 
                  scenario = c(rep('F,F;F,F',6),rep('F,F;F,L',6),rep('F,L;F,L',6),rep('F,F;L,L',6),rep('F,L;L,L',6),rep('L,L;L,L',6)),
                  effort = 0, strength = 0,
                  outcome = 0, prob = 0, round = rep(c(1,1,2,2,3,3), 6), 
                  reward = rep(c(10,10,20,20,20,20), 6))
sim$prob[sim$round==1] = NaN
sim$outcome = c(integer(6), integer(3),1,integer(2), 0,1,0,1,1,1, integer(2),integer(4)+1, 0,integer(5)+1, integer(6)+1)

########################################## R1 Effort Strength & R2 Probability ##########################################
# F,F
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x1a = [], x1b = []
var samples1 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  x1a.push(sa)
  x1b.push(sb)
  return 0
}})

var output = {aE: optE(x1a,'box',lowR), bE: optE(x1b,'box',lowR), aS: listMean(x1a), bS: listMean(x1b), aP: outcome(x1a,'box',highR), bP: outcome(x1b,'box',highR)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==1 & sim$agent=='A' & sim$scenario!='F,L;F,L' & sim$scenario!='F,L;L,L' & sim$scenario!='L,L;L,L'] <- a$aE*100
sim$effort[sim$round==1 & sim$agent=='B' & sim$scenario!='F,L;F,L' & sim$scenario!='F,L;L,L' & sim$scenario!='L,L;L,L'] <- a$bE*100
sim$strength[sim$round==1 & sim$agent=='A' & sim$scenario!='F,L;F,L' & sim$scenario!='F,L;L,L' & sim$scenario!='L,L;L,L'] <- a$aS
sim$strength[sim$round==1 & sim$agent=='B' & sim$scenario!='F,L;F,L' & sim$scenario!='F,L;L,L' & sim$scenario!='L,L;L,L'] <- a$bS
sim$prob[sim$round==2 & sim$agent=='A' & sim$scenario!='F,L;F,L' & sim$scenario!='F,L;L,L' & sim$scenario!='L,L;L,L'] <- a$aP*100
sim$prob[sim$round==2 & sim$agent=='B' & sim$scenario!='F,L;F,L' & sim$scenario!='F,L;L,L' & sim$scenario!='L,L;L,L'] <- a$bP*100


# F,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x1a = [], x1b = []
var samples1 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == true)
  x1a.push(sa)
  x1b.push(sb)
  return 0
}})

var output = {aE: optE(x1a,'box',lowR), bE: optE(x1b,'box',lowR), aS: listMean(x1a), bS: listMean(x1b), aP: outcome(x1a,'box',highR), bP: outcome(x1b,'box',highR)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==1 & sim$agent=='A' & sim$scenario!='F,F;F,F' & sim$scenario!='F,F;F,L' & sim$scenario!='F,F;L,L' & sim$scenario!='L,L;L,L'] <- a$aE*100
sim$effort[sim$round==1 & sim$agent=='B' & sim$scenario!='F,F;F,F' & sim$scenario!='F,F;F,L' & sim$scenario!='F,F;L,L' & sim$scenario!='L,L;L,L'] <- a$bE*100
sim$strength[sim$round==1 & sim$agent=='A' & sim$scenario!='F,F;F,F' & sim$scenario!='F,F;F,L' & sim$scenario!='F,F;L,L' & sim$scenario!='L,L;L,L'] <- a$aS
sim$strength[sim$round==1 & sim$agent=='B' & sim$scenario!='F,F;F,F' & sim$scenario!='F,F;F,L' & sim$scenario!='F,F;L,L' & sim$scenario!='L,L;L,L'] <- a$bS
sim$prob[sim$round==2 & sim$agent=='A' & sim$scenario!='F,F;F,F' & sim$scenario!='F,F;F,L' & sim$scenario!='F,F;L,L' & sim$scenario!='L,L;L,L'] <- a$aP*100
sim$prob[sim$round==2 & sim$agent=='B' & sim$scenario!='F,F;F,F' & sim$scenario!='F,F;F,L' & sim$scenario!='F,F;L,L' & sim$scenario!='L,L;L,L'] <- a$bP*100


# L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x1a = [], x1b = []
var samples1 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == true)
  condition(outcome(sb,'box',lowR) == true)
  x1a.push(sa)
  x1b.push(sb)
  return 0
}})

var output = {aE: optE(x1a,'box',lowR), bE: optE(x1b,'box',lowR), aS: listMean(x1a), bS: listMean(x1b), aP: outcome(x1a,'box',highR), bP: outcome(x1b,'box',highR)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==1 & sim$agent=='A' & sim$scenario=='L,L;L,L'] <- a$aE*100
sim$effort[sim$round==1 & sim$agent=='B' & sim$scenario=='L,L;L,L'] <- a$bE*100
sim$strength[sim$round==1 & sim$agent=='A' & sim$scenario=='L,L;L,L'] <- a$aS
sim$strength[sim$round==1 & sim$agent=='B' & sim$scenario=='L,L;L,L'] <- a$bS
sim$prob[sim$round==2 & sim$agent=='A' & sim$scenario=='L,L;L,L'] <- a$aP*100
sim$prob[sim$round==2 & sim$agent=='B' & sim$scenario=='L,L;L,L'] <- a$bP*100

########################################## R2 Effort Strength ##########################################
# F,F;F,F
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == false)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var output = {aE: optE(x2a,'box',highR), bE: optE(x2b,'box',highR), aS: listMean(x2a), bS: listMean(x2b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==2 & sim$agent=='A' & sim$scenario=='F,F;F,F'] <- a$aE*100
sim$effort[sim$round==2 & sim$agent=='B' & sim$scenario=='F,F;F,F'] <- a$bE*100
sim$strength[sim$round==2 & sim$agent=='A' & sim$scenario=='F,F;F,F'] <- a$aS
sim$strength[sim$round==2 & sim$agent=='B' & sim$scenario=='F,F;F,F'] <- a$bS


# F,F;F,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var output = {aE: optE(x2a,'box',highR), bE: optE(x2b,'box',highR), aS: listMean(x2a), bS: listMean(x2b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==2 & sim$agent=='A' & sim$scenario=='F,F;F,L'] <- a$aE*100
sim$effort[sim$round==2 & sim$agent=='B' & sim$scenario=='F,F;F,L'] <- a$bE*100
sim$strength[sim$round==2 & sim$agent=='A' & sim$scenario=='F,F;F,L'] <- a$aS
sim$strength[sim$round==2 & sim$agent=='B' & sim$scenario=='F,F;F,L'] <- a$bS


# F,L;F,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var output = {aE: optE(x2a,'box',highR), bE: optE(x2b,'box',highR), aS: listMean(x2a), bS: listMean(x2b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==2 & sim$agent=='A' & sim$scenario=='F,L;F,L'] <- a$aE*100
sim$effort[sim$round==2 & sim$agent=='B' & sim$scenario=='F,L;F,L'] <- a$bE*100
sim$strength[sim$round==2 & sim$agent=='A' & sim$scenario=='F,L;F,L'] <- a$aS
sim$strength[sim$round==2 & sim$agent=='B' & sim$scenario=='F,L;F,L'] <- a$bS


# F,F;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var output = {aE: optE(x2a,'box',highR), bE: optE(x2b,'box',highR), aS: listMean(x2a), bS: listMean(x2b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==2 & sim$agent=='A' & sim$scenario=='F,F;L,L'] <- a$aE*100
sim$effort[sim$round==2 & sim$agent=='B' & sim$scenario=='F,F;L,L'] <- a$bE*100
sim$strength[sim$round==2 & sim$agent=='A' & sim$scenario=='F,F;L,L'] <- a$aS
sim$strength[sim$round==2 & sim$agent=='B' & sim$scenario=='F,F;L,L'] <- a$bS


# F,L;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var output = {aE: optE(x2a,'box',highR), bE: optE(x2b,'box',highR), aS: listMean(x2a), bS: listMean(x2b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==2 & sim$agent=='A' & sim$scenario=='F,L;L,L'] <- a$aE*100
sim$effort[sim$round==2 & sim$agent=='B' & sim$scenario=='F,L;L,L'] <- a$bE*100
sim$strength[sim$round==2 & sim$agent=='A' & sim$scenario=='F,L;L,L'] <- a$aS
sim$strength[sim$round==2 & sim$agent=='B' & sim$scenario=='F,L;L,L'] <- a$bS


# L,L;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == true)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var output = {aE: optE(x2a,'box',highR), bE: optE(x2b,'box',highR), aS: listMean(x2a), bS: listMean(x2b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==2 & sim$agent=='A' & sim$scenario=='L,L;L,L'] <- a$aE*100
sim$effort[sim$round==2 & sim$agent=='B' & sim$scenario=='L,L;L,L'] <- a$bE*100
sim$strength[sim$round==2 & sim$agent=='A' & sim$scenario=='L,L;L,L'] <- a$aS
sim$strength[sim$round==2 & sim$agent=='B' & sim$scenario=='L,L;L,L'] <- a$bS


########################################## R3 Probability ##########################################
# F,F;F,F
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == false)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var jointUtility = function(a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }

  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(reward) {
      var effort2 = 0
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(reward) {
      var effort2 = 0
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }

  var aE = a(r3_reward)
  var bE = b(r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var output = {P: jointUtility(x2a,x2b).outcome}
output
"
a <- webppl(mdl)
sim$prob[sim$round==3 & sim$scenario=='F,F;F,F'] <- a$P*100


# F,F;F,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var jointUtility = function(a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }
  
  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(reward) {
      var effort2 = 0
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(reward) {
      var effort2 = 0
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      }
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var aE = a(r3_reward)
  var bE = b(r3_reward)

  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var output = {P: jointUtility(x2a,x2b).outcome}
output
"
a <- webppl(mdl)
sim$prob[sim$round==3 & sim$scenario=='F,F;F,L'] <- a$P*100


# F,L;F,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var jointUtility = function(a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }

  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(reward) {
      var effort2 = 0
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(reward) {
      var effort2 = 0
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }

  var aE = a(r3_reward)
  var bE = b(r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var output = {P: jointUtility(x2a,x2b).outcome}
output
"
a <- webppl(mdl)
sim$prob[sim$round==3 & sim$scenario=='F,L;F,L'] <- a$P*100


# F,F;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var jointUtility = function(a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }
  
  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(reward) {
      var effort2 = 0
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(reward) {
      var effort2 = 0
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var aE = a(r3_reward)
  var bE = b(r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var output = {P: jointUtility(x2a,x2b).outcome}
output
"
a <- webppl(mdl)
sim$prob[sim$round==3 & sim$scenario=='F,F;L,L'] <- a$P*100


# F,L;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var jointUtility = function(a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }
  
  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(reward) {
      var effort2 = 0
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(reward) {
      var effort2 = 0
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var aE = a(r3_reward)
  var bE = b(r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var output = {P: jointUtility(x2a,x2b).outcome}
output
"
a <- webppl(mdl)
sim$prob[sim$round==3 & sim$scenario=='F,L;L,L'] <- a$P*100


# L,L;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == true)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var jointUtility = function(a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }

  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(reward) {
      var effort2 = 0
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(reward) {
      var effort2 = 0
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var aE = a(r3_reward)
  var bE = b(r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var output = {P: jointUtility(x2a,x2b).outcome}
output
"
a <- webppl(mdl)
sim$prob[sim$round==3 & sim$scenario=='L,L;L,L'] <- a$P*100


########################################## R3 Effort Strength ##########################################
# F,F;F,F
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var jointUtility = function(a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }
  
  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(reward) {
      var effort2 = 0
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(reward) {
      var effort2 = 0
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var aE = a(r3_reward)
  var bE = b(r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var x3a = [], x3b = []
var samples3 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == false)
  condition(jointUtility(sa,sb).outcome == false)
  x3a.push(sa)
  x3b.push(sb)
  return 0
}})

var ju = jointUtility(x3a,x3b)

var output = {aE: ju.aE, bE: ju.bE, aS: listMean(x3a), bS: listMean(x3b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==3 & sim$agent=='A' & sim$scenario=='F,F;F,F'] <- a$aE*100
sim$effort[sim$round==3 & sim$agent=='B' & sim$scenario=='F,F;F,F'] <- a$bE*100
sim$strength[sim$round==3 & sim$agent=='A' & sim$scenario=='F,F;F,F'] <- a$aS
sim$strength[sim$round==3 & sim$agent=='B' & sim$scenario=='F,F;F,F'] <- a$bS


# F,F;F,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var jointUtility = function(a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }
  
  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(reward) {
      var effort2 = 0
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(reward) {
      var effort2 = 0
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }

  var aE = a(r3_reward)  
  var bE = b(r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var x3a = [], x3b = []
var samples3 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == true)
  condition(jointUtility(sa,sb).outcome == false)
  x3a.push(sa)
  x3b.push(sb)
  return 0
}})
 
var ju = jointUtility(x3a,x3b)

var output = {aE: ju.aE, bE: ju.bE, aS: listMean(x3a), bS: listMean(x3b)}
output
"
# a <- webppl(mdl)

sim$effort[sim$round==3 & sim$agent=='A' & sim$scenario=='F,F;F,L'] <- NaN
sim$effort[sim$round==3 & sim$agent=='B' & sim$scenario=='F,F;F,L'] <- NaN
sim$strength[sim$round==3 & sim$agent=='A' & sim$scenario=='F,F;F,L'] <- NaN
sim$strength[sim$round==3 & sim$agent=='B' & sim$scenario=='F,F;F,L'] <- NaN


# F,L;F,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var jointUtility = function(a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }
  
  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(reward) {
      var effort2 = 0
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(reward) {
      var effort2 = 0
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var aE = a(r3_reward)
  var bE = b(r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var x3a = [], x3b = []
var samples3 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == true)
  condition(jointUtility(sa,sb).outcome == true)
  x3a.push(sa)
  x3b.push(sb)
  return 0
}})
 
var ju = jointUtility(x3a,x3b)

var output = {aE: ju.aE, bE: ju.bE, aS: listMean(x3a), bS: listMean(x3b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==3 & sim$agent=='A' & sim$scenario=='F,L;F,L'] <- a$aE*100
sim$effort[sim$round==3 & sim$agent=='B' & sim$scenario=='F,L;F,L'] <- a$bE*100
sim$strength[sim$round==3 & sim$agent=='A' & sim$scenario=='F,L;F,L'] <- a$aS
sim$strength[sim$round==3 & sim$agent=='B' & sim$scenario=='F,L;F,L'] <- a$bS


# F,F;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var jointUtility = function(a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }
  
  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(reward) {
      var effort2 = 0
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(reward) {
      var effort2 = 0
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }

  var aE = a(r3_reward)
  var bE = b(r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var x3a = [], x3b = []
var samples3 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  condition(jointUtility(sa,sb).outcome == true)
  x3a.push(sa)
  x3b.push(sb)
  return 0
}})
 
var ju = jointUtility(x3a,x3b)

var output = {aE: ju.aE, bE: ju.bE, aS: listMean(x3a), bS: listMean(x3b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==3 & sim$agent=='A' & sim$scenario=='F,F;L,L'] <- a$aE*100
sim$effort[sim$round==3 & sim$agent=='B' & sim$scenario=='F,F;L,L'] <- a$bE*100
sim$strength[sim$round==3 & sim$agent=='A' & sim$scenario=='F,F;L,L'] <- a$aS
sim$strength[sim$round==3 & sim$agent=='B' & sim$scenario=='F,F;L,L'] <- a$bS


# F,L;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var jointUtility = function(a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }

  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(reward) {
      var effort2 = 0
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(reward) {
      var effort2 = 0
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var aE = a(r3_reward)
  var bE = b(r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var x3a = [], x3b = []
var samples3 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  condition(jointUtility(sa,sb).outcome == true)
  x3a.push(sa)
  x3b.push(sb)
  return 0
}})

var ju = jointUtility(x3a,x3b)

var output = {aE: ju.aE, bE: ju.bE, aS: listMean(x3a), bS: listMean(x3b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==3 & sim$agent=='A' & sim$scenario=='F,L;L,L'] <- a$aE*100
sim$effort[sim$round==3 & sim$agent=='B' & sim$scenario=='F,L;L,L'] <- a$bE*100
sim$strength[sim$round==3 & sim$agent=='A' & sim$scenario=='F,L;L,L'] <- a$aS
sim$strength[sim$round==3 & sim$agent=='B' & sim$scenario=='F,L;L,L'] <- a$bS


# L,L;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var jointUtility = function(a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }

  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(reward) {
      var effort2 = 0
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(reward) {
      var effort2 = 0
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var aE = a(r3_reward)
  var bE = b(r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var x3a = [], x3b = []
var samples3 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == true)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  condition(jointUtility(sa,sb).outcome == true)
  x3a.push(sa)
  x3b.push(sb)
  return 0
}})

var ju = jointUtility(x3a,x3b)

var output = {aE: ju.aE, bE: ju.bE, aS: listMean(x3a), bS: listMean(x3b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==3 & sim$agent=='A' & sim$scenario=='L,L;L,L'] <- a$aE*100
sim$effort[sim$round==3 & sim$agent=='B' & sim$scenario=='L,L;L,L'] <- a$bE*100
sim$strength[sim$round==3 & sim$agent=='A' & sim$scenario=='L,L;L,L'] <- a$aS
sim$strength[sim$round==3 & sim$agent=='B' & sim$scenario=='L,L;L,L'] <- a$bS

solitary <- sim


## Compensatory
sim <- data.frame(model = 'compensatory',
                  contest = c(rep(1,6),rep(2,6),rep(3,6),rep(4,6),rep(5,6),rep(6,6)), 
                  agent = rep(c('A','B'), 18), 
                  scenario = c(rep('F,F;F,F',6),rep('F,F;F,L',6),rep('F,L;F,L',6),rep('F,F;L,L',6),rep('F,L;L,L',6),rep('L,L;L,L',6)),
                  effort = 0, strength = 0,
                  outcome = 0, prob = 0, round = rep(c(1,1,2,2,3,3), 6), 
                  reward = rep(c(10,10,20,20,20,20), 6))
sim$prob[sim$round==1] = NaN
sim$outcome = c(integer(6), integer(3),1,integer(2), 0,1,0,1,1,1, integer(2),integer(4)+1, 0,integer(5)+1, integer(6)+1)

########################################## R1 Effort Strength & R2 Probability ##########################################
# F,F
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x1a = [], x1b = []
var samples1 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  x1a.push(sa)
  x1b.push(sb)
  return 0
}})

var output = {aE: optE(x1a,'box',lowR), bE: optE(x1b,'box',lowR), aS: listMean(x1a), bS: listMean(x1b), aP: outcome(x1a,'box',highR), bP: outcome(x1b,'box',highR)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==1 & sim$agent=='A' & sim$scenario!='F,L;F,L' & sim$scenario!='F,L;L,L' & sim$scenario!='L,L;L,L'] <- a$aE*100
sim$effort[sim$round==1 & sim$agent=='B' & sim$scenario!='F,L;F,L' & sim$scenario!='F,L;L,L' & sim$scenario!='L,L;L,L'] <- a$bE*100
sim$strength[sim$round==1 & sim$agent=='A' & sim$scenario!='F,L;F,L' & sim$scenario!='F,L;L,L' & sim$scenario!='L,L;L,L'] <- a$aS
sim$strength[sim$round==1 & sim$agent=='B' & sim$scenario!='F,L;F,L' & sim$scenario!='F,L;L,L' & sim$scenario!='L,L;L,L'] <- a$bS
sim$prob[sim$round==2 & sim$agent=='A' & sim$scenario!='F,L;F,L' & sim$scenario!='F,L;L,L' & sim$scenario!='L,L;L,L'] <- a$aP*100
sim$prob[sim$round==2 & sim$agent=='B' & sim$scenario!='F,L;F,L' & sim$scenario!='F,L;L,L' & sim$scenario!='L,L;L,L'] <- a$bP*100


# F,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x1a = [], x1b = []
var samples1 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == true)
  x1a.push(sa)
  x1b.push(sb)
  return 0
}})

var output = {aE: optE(x1a,'box',lowR), bE: optE(x1b,'box',lowR), aS: listMean(x1a), bS: listMean(x1b), aP: outcome(x1a,'box',highR), bP: outcome(x1b,'box',highR)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==1 & sim$agent=='A' & sim$scenario!='F,F;F,F' & sim$scenario!='F,F;F,L' & sim$scenario!='F,F;L,L' & sim$scenario!='L,L;L,L'] <- a$aE*100
sim$effort[sim$round==1 & sim$agent=='B' & sim$scenario!='F,F;F,F' & sim$scenario!='F,F;F,L' & sim$scenario!='F,F;L,L' & sim$scenario!='L,L;L,L'] <- a$bE*100
sim$strength[sim$round==1 & sim$agent=='A' & sim$scenario!='F,F;F,F' & sim$scenario!='F,F;F,L' & sim$scenario!='F,F;L,L' & sim$scenario!='L,L;L,L'] <- a$aS
sim$strength[sim$round==1 & sim$agent=='B' & sim$scenario!='F,F;F,F' & sim$scenario!='F,F;F,L' & sim$scenario!='F,F;L,L' & sim$scenario!='L,L;L,L'] <- a$bS
sim$prob[sim$round==2 & sim$agent=='A' & sim$scenario!='F,F;F,F' & sim$scenario!='F,F;F,L' & sim$scenario!='F,F;L,L' & sim$scenario!='L,L;L,L'] <- a$aP*100
sim$prob[sim$round==2 & sim$agent=='B' & sim$scenario!='F,F;F,F' & sim$scenario!='F,F;F,L' & sim$scenario!='F,F;L,L' & sim$scenario!='L,L;L,L'] <- a$bP*100


# L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x1a = [], x1b = []
var samples1 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == true)
  condition(outcome(sb,'box',lowR) == true)
  x1a.push(sa)
  x1b.push(sb)
  return 0
}})

var output = {aE: optE(x1a,'box',lowR), bE: optE(x1b,'box',lowR), aS: listMean(x1a), bS: listMean(x1b), aP: outcome(x1a,'box',highR), bP: outcome(x1b,'box',highR)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==1 & sim$agent=='A' & sim$scenario=='L,L;L,L'] <- a$aE*100
sim$effort[sim$round==1 & sim$agent=='B' & sim$scenario=='L,L;L,L'] <- a$bE*100
sim$strength[sim$round==1 & sim$agent=='A' & sim$scenario=='L,L;L,L'] <- a$aS
sim$strength[sim$round==1 & sim$agent=='B' & sim$scenario=='L,L;L,L'] <- a$bS
sim$prob[sim$round==2 & sim$agent=='A' & sim$scenario=='L,L;L,L'] <- a$aP*100
sim$prob[sim$round==2 & sim$agent=='B' & sim$scenario=='L,L;L,L'] <- a$bP*100

########################################## R2 Effort Strength ##########################################
# F,F;F,F
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == false)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var output = {aE: optE(x2a,'box',highR), bE: optE(x2b,'box',highR), aS: listMean(x2a), bS: listMean(x2b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==2 & sim$agent=='A' & sim$scenario=='F,F;F,F'] <- a$aE*100
sim$effort[sim$round==2 & sim$agent=='B' & sim$scenario=='F,F;F,F'] <- a$bE*100
sim$strength[sim$round==2 & sim$agent=='A' & sim$scenario=='F,F;F,F'] <- a$aS
sim$strength[sim$round==2 & sim$agent=='B' & sim$scenario=='F,F;F,F'] <- a$bS


# F,F;F,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var output = {aE: optE(x2a,'box',highR), bE: optE(x2b,'box',highR), aS: listMean(x2a), bS: listMean(x2b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==2 & sim$agent=='A' & sim$scenario=='F,F;F,L'] <- a$aE*100
sim$effort[sim$round==2 & sim$agent=='B' & sim$scenario=='F,F;F,L'] <- a$bE*100
sim$strength[sim$round==2 & sim$agent=='A' & sim$scenario=='F,F;F,L'] <- a$aS
sim$strength[sim$round==2 & sim$agent=='B' & sim$scenario=='F,F;F,L'] <- a$bS


# F,L;F,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var output = {aE: optE(x2a,'box',highR), bE: optE(x2b,'box',highR), aS: listMean(x2a), bS: listMean(x2b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==2 & sim$agent=='A' & sim$scenario=='F,L;F,L'] <- a$aE*100
sim$effort[sim$round==2 & sim$agent=='B' & sim$scenario=='F,L;F,L'] <- a$bE*100
sim$strength[sim$round==2 & sim$agent=='A' & sim$scenario=='F,L;F,L'] <- a$aS
sim$strength[sim$round==2 & sim$agent=='B' & sim$scenario=='F,L;F,L'] <- a$bS


# F,F;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var output = {aE: optE(x2a,'box',highR), bE: optE(x2b,'box',highR), aS: listMean(x2a), bS: listMean(x2b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==2 & sim$agent=='A' & sim$scenario=='F,F;L,L'] <- a$aE*100
sim$effort[sim$round==2 & sim$agent=='B' & sim$scenario=='F,F;L,L'] <- a$bE*100
sim$strength[sim$round==2 & sim$agent=='A' & sim$scenario=='F,F;L,L'] <- a$aS
sim$strength[sim$round==2 & sim$agent=='B' & sim$scenario=='F,F;L,L'] <- a$bS


# F,L;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var output = {aE: optE(x2a,'box',highR), bE: optE(x2b,'box',highR), aS: listMean(x2a), bS: listMean(x2b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==2 & sim$agent=='A' & sim$scenario=='F,L;L,L'] <- a$aE*100
sim$effort[sim$round==2 & sim$agent=='B' & sim$scenario=='F,L;L,L'] <- a$bE*100
sim$strength[sim$round==2 & sim$agent=='A' & sim$scenario=='F,L;L,L'] <- a$aS
sim$strength[sim$round==2 & sim$agent=='B' & sim$scenario=='F,L;L,L'] <- a$bS


# L,L;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == true)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var output = {aE: optE(x2a,'box',highR), bE: optE(x2b,'box',highR), aS: listMean(x2a), bS: listMean(x2b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==2 & sim$agent=='A' & sim$scenario=='L,L;L,L'] <- a$aE*100
sim$effort[sim$round==2 & sim$agent=='B' & sim$scenario=='L,L;L,L'] <- a$bE*100
sim$strength[sim$round==2 & sim$agent=='A' & sim$scenario=='L,L;L,L'] <- a$aS
sim$strength[sim$round==2 & sim$agent=='B' & sim$scenario=='L,L;L,L'] <- a$bS


########################################## R3 Probability ##########################################
# F,F;F,F
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == false)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var jointUtility = function(a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }

  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(reward) {
      var effort2 = 1
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(reward) {
      var effort2 = 1
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }

  var aE = a(r3_reward)
  var bE = b(r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var output = {P: jointUtility(x2a,x2b).outcome}
output
"
a <- webppl(mdl)
sim$prob[sim$round==3 & sim$scenario=='F,F;F,F'] <- a$P*100


# F,F;F,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var jointUtility = function(a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }
  
  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(reward) {
      var effort2 = 1
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(reward) {
      var effort2 = 1
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var aE = a(r3_reward)
  var bE = b(r3_reward)
 
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var output = {P: jointUtility(x2a,x2b).outcome}
output
"
a <- webppl(mdl)
sim$prob[sim$round==3 & sim$scenario=='F,F;F,L'] <- a$P*100


# F,L;F,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var jointUtility = function(a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }

  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(reward) {
      var effort2 = 1
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(reward) {
      var effort2 = 1
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }

  var aE = a(r3_reward)
  var bE = b(r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var ju = jointUtility(x2a,x2b)
var output = {P: ju.outcome, aE: ju.aE, bE: ju.bE, aS: x2a, bS: x2b}
output
"
a <- webppl(mdl)
sim$prob[sim$round==3 & sim$scenario=='F,L;F,L'] <- a$P*100


# F,F;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var jointUtility = function(a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }
  
  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(reward) {
      var effort2 = 1
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(reward) {
      var effort2 = 1
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var aE = a(r3_reward)
  var bE = b(r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var output = {P: jointUtility(x2a,x2b).outcome}
output
"
a <- webppl(mdl)
sim$prob[sim$round==3 & sim$scenario=='F,F;L,L'] <- a$P*100


# F,L;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var jointUtility = function(a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }
  
  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(reward) {
      var effort2 = 1
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(reward) {
      var effort2 = 1
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var aE = a(r3_reward)
  var bE = b(r3_reward)

  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var output = {P: jointUtility(x2a,x2b).outcome}
output
"
a <- webppl(mdl)
sim$prob[sim$round==3 & sim$scenario=='F,L;L,L'] <- a$P*100


# L,L;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == true)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var jointUtility = function(a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }

  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(reward) {
      var effort2 = 1
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(reward) {
      var effort2 = 1
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var aE = a(r3_reward)
  var bE = b(r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var output = {P: jointUtility(x2a,x2b).outcome}
output
"
a <- webppl(mdl)
sim$prob[sim$round==3 & sim$scenario=='L,L;L,L'] <- a$P*100


########################################## R3 Effort Strength ##########################################
# F,F;F,F
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var jointUtility = function(a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }
  
  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(reward) {
      var effort2 = 1
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(reward) {
      var effort2 = 1
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var aE = a(r3_reward)
  var bE = b(r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var x3a = [], x3b = []
var samples3 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == false)
  condition(jointUtility(sa,sb).outcome == false)
  x3a.push(sa)
  x3b.push(sb)
  return 0
}})
 
var ju = jointUtility(x3a,x3b)

var output = {aE: ju.aE, bE: ju.bE, aS: listMean(x3a), bS: listMean(x3b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==3 & sim$agent=='A' & sim$scenario=='F,F;F,F'] <- a$aE*100
sim$effort[sim$round==3 & sim$agent=='B' & sim$scenario=='F,F;F,F'] <- a$bE*100
sim$strength[sim$round==3 & sim$agent=='A' & sim$scenario=='F,F;F,F'] <- a$aS
sim$strength[sim$round==3 & sim$agent=='B' & sim$scenario=='F,F;F,F'] <- a$bS


# F,F;F,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var jointUtility = function(a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }
  
  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(reward) {
      var effort2 = 1
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(reward) {
      var effort2 = 1
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var aE = a(r3_reward)
  var bE = b(r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var x3a = [], x3b = []
var samples3 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == true)
  condition(jointUtility(sa,sb).outcome == false)
  x3a.push(sa)
  x3b.push(sb)
  return 0
}})
 
var ju = jointUtility(x3a,x3b)

var output = {aE: ju.aE, bE: ju.bE, aS: listMean(x3a), bS: listMean(x3b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==3 & sim$agent=='A' & sim$scenario=='F,F;F,L'] <- a$aE*100
sim$effort[sim$round==3 & sim$agent=='B' & sim$scenario=='F,F;F,L'] <- a$bE*100
sim$strength[sim$round==3 & sim$agent=='A' & sim$scenario=='F,F;F,L'] <- a$aS
sim$strength[sim$round==3 & sim$agent=='B' & sim$scenario=='F,F;F,L'] <- a$bS


# F,L;F,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var jointUtility = function(a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }
  
  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(reward) {
      var effort2 = 1
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(reward) {
      var effort2 = 1
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var aE = a(r3_reward)
  var bE = b(r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var x3a = [], x3b = []
var samples3 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == true)
  condition(jointUtility(sa,sb).outcome == true)
  x3a.push(sa)
  x3b.push(sb)
  return 0
}})
 
var ju = jointUtility(x3a,x3b)

var output = {aE: ju.aE, bE: ju.bE, aS: listMean(x3a), bS: listMean(x3b)}
output
"
# a <- webppl(mdl)

sim$effort[sim$round==3 & sim$agent=='A' & sim$scenario=='F,L;F,L'] <- NaN
sim$effort[sim$round==3 & sim$agent=='B' & sim$scenario=='F,L;F,L'] <- NaN
sim$strength[sim$round==3 & sim$agent=='A' & sim$scenario=='F,L;F,L'] <- NaN
sim$strength[sim$round==3 & sim$agent=='B' & sim$scenario=='F,L;F,L'] <- NaN


# F,F;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var jointUtility = function(a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }

  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(reward) {
      var effort2 = 1
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(reward) {
      var effort2 = 1
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var aE = a(r3_reward)
  var bE = b(r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var x3a = [], x3b = []
var samples3 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  condition(jointUtility(sa,sb).outcome == true)
  x3a.push(sa)
  x3b.push(sb)
  return 0
}})

var ju = jointUtility(x3a,x3b)

var output = {aE: ju.aE, bE: ju.bE, aS: listMean(x3a), bS: listMean(x3b)}
output
"
# a <- webppl(mdl)

sim$effort[sim$round==3 & sim$agent=='A' & sim$scenario=='F,F;L,L'] <- NaN
sim$effort[sim$round==3 & sim$agent=='B' & sim$scenario=='F,F;L,L'] <- NaN
sim$strength[sim$round==3 & sim$agent=='A' & sim$scenario=='F,F;L,L'] <- NaN
sim$strength[sim$round==3 & sim$agent=='B' & sim$scenario=='F,F;L,L'] <- NaN


# F,L;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var jointUtility = function(a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }

  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(reward) {
      var effort2 = 1
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(reward) {
      var effort2 = 1
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var aE = a(r3_reward)
  var bE = b(r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var x3a = [], x3b = []
var samples3 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  condition(jointUtility(sa,sb).outcome == true)
  x3a.push(sa)
  x3b.push(sb)
  return 0
}})

var ju = jointUtility(x3a,x3b)

var output = {aE: ju.aE, bE: ju.bE, aS: listMean(x3a), bS: listMean(x3b)}
output
"
# a <- webppl(mdl)

sim$effort[sim$round==3 & sim$agent=='A' & sim$scenario=='F,L;L,L'] <- NaN
sim$effort[sim$round==3 & sim$agent=='B' & sim$scenario=='F,L;L,L'] <- NaN
sim$strength[sim$round==3 & sim$agent=='A' & sim$scenario=='F,L;L,L'] <- NaN
sim$strength[sim$round==3 & sim$agent=='B' & sim$scenario=='F,L;L,L'] <- NaN


# L,L;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var jointUtility = function(a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }

  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(reward) {
      var effort2 = 1
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(reward) {
      var effort2 = 1
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var aE = a(r3_reward)
  var bE = b(r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var x3a = [], x3b = []
var samples3 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == true)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  condition(jointUtility(sa,sb).outcome == true)
  x3a.push(sa)
  x3b.push(sb)
  return 0
}})

var ju = jointUtility(x3a,x3b)

var output = {aE: ju.aE, bE: ju.bE, aS: listMean(x3a), bS: listMean(x3b)}
output
"
# a <- webppl(mdl)

sim$effort[sim$round==3 & sim$agent=='A' & sim$scenario=='L,L;L,L'] <- NaN
sim$effort[sim$round==3 & sim$agent=='B' & sim$scenario=='L,L;L,L'] <- NaN
sim$strength[sim$round==3 & sim$agent=='A' & sim$scenario=='L,L;L,L'] <- NaN
sim$strength[sim$round==3 & sim$agent=='B' & sim$scenario=='L,L;L,L'] <- NaN

compensatory <- sim


## Maximum
sim <- data.frame(model = 'maximum',
                  contest = c(rep(1,6),rep(2,6),rep(3,6),rep(4,6),rep(5,6),rep(6,6)), 
                  agent = rep(c('A','B'), 18), 
                  scenario = c(rep('F,F;F,F',6),rep('F,F;F,L',6),rep('F,L;F,L',6),rep('F,F;L,L',6),rep('F,L;L,L',6),rep('L,L;L,L',6)),
                  effort = 0, strength = 0,
                  outcome = 0, prob = 0, round = rep(c(1,1,2,2,3,3), 6), 
                  reward = rep(c(10,10,20,20,20,20), 6))
sim$prob[sim$round==1] = NaN
sim$outcome = c(integer(6), integer(3),1,integer(2), 0,1,0,1,1,1, integer(2),integer(4)+1, 0,integer(5)+1, integer(6)+1)

########################################## R1 Effort Strength & R2 Probability ##########################################
# F,F
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x1a = [], x1b = []
var samples1 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  x1a.push(sa)
  x1b.push(sb)
  return 0
}})

var output = {aE: optE(x1a,'box',lowR), bE: optE(x1b,'box',lowR), aS: listMean(x1a), bS: listMean(x1b), aP: outcome(x1a,'box',highR), bP: outcome(x1b,'box',highR)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==1 & sim$agent=='A' & sim$scenario!='F,L;F,L' & sim$scenario!='F,L;L,L' & sim$scenario!='L,L;L,L'] <- a$aE*100
sim$effort[sim$round==1 & sim$agent=='B' & sim$scenario!='F,L;F,L' & sim$scenario!='F,L;L,L' & sim$scenario!='L,L;L,L'] <- a$bE*100
sim$strength[sim$round==1 & sim$agent=='A' & sim$scenario!='F,L;F,L' & sim$scenario!='F,L;L,L' & sim$scenario!='L,L;L,L'] <- a$aS
sim$strength[sim$round==1 & sim$agent=='B' & sim$scenario!='F,L;F,L' & sim$scenario!='F,L;L,L' & sim$scenario!='L,L;L,L'] <- a$bS
sim$prob[sim$round==2 & sim$agent=='A' & sim$scenario!='F,L;F,L' & sim$scenario!='F,L;L,L' & sim$scenario!='L,L;L,L'] <- a$aP*100
sim$prob[sim$round==2 & sim$agent=='B' & sim$scenario!='F,L;F,L' & sim$scenario!='F,L;L,L' & sim$scenario!='L,L;L,L'] <- a$bP*100


# F,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x1a = [], x1b = []
var samples1 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == true)
  x1a.push(sa)
  x1b.push(sb)
  return 0
}})

var output = {aE: optE(x1a,'box',lowR), bE: optE(x1b,'box',lowR), aS: listMean(x1a), bS: listMean(x1b), aP: outcome(x1a,'box',highR), bP: outcome(x1b,'box',highR)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==1 & sim$agent=='A' & sim$scenario!='F,F;F,F' & sim$scenario!='F,F;F,L' & sim$scenario!='F,F;L,L' & sim$scenario!='L,L;L,L'] <- a$aE*100
sim$effort[sim$round==1 & sim$agent=='B' & sim$scenario!='F,F;F,F' & sim$scenario!='F,F;F,L' & sim$scenario!='F,F;L,L' & sim$scenario!='L,L;L,L'] <- a$bE*100
sim$strength[sim$round==1 & sim$agent=='A' & sim$scenario!='F,F;F,F' & sim$scenario!='F,F;F,L' & sim$scenario!='F,F;L,L' & sim$scenario!='L,L;L,L'] <- a$aS
sim$strength[sim$round==1 & sim$agent=='B' & sim$scenario!='F,F;F,F' & sim$scenario!='F,F;F,L' & sim$scenario!='F,F;L,L' & sim$scenario!='L,L;L,L'] <- a$bS
sim$prob[sim$round==2 & sim$agent=='A' & sim$scenario!='F,F;F,F' & sim$scenario!='F,F;F,L' & sim$scenario!='F,F;L,L' & sim$scenario!='L,L;L,L'] <- a$aP*100
sim$prob[sim$round==2 & sim$agent=='B' & sim$scenario!='F,F;F,F' & sim$scenario!='F,F;F,L' & sim$scenario!='F,F;L,L' & sim$scenario!='L,L;L,L'] <- a$bP*100


# L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x1a = [], x1b = []
var samples1 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == true)
  condition(outcome(sb,'box',lowR) == true)
  x1a.push(sa)
  x1b.push(sb)
  return 0
}})

var output = {aE: optE(x1a,'box',lowR), bE: optE(x1b,'box',lowR), aS: listMean(x1a), bS: listMean(x1b), aP: outcome(x1a,'box',highR), bP: outcome(x1b,'box',highR)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==1 & sim$agent=='A' & sim$scenario=='L,L;L,L'] <- a$aE*100
sim$effort[sim$round==1 & sim$agent=='B' & sim$scenario=='L,L;L,L'] <- a$bE*100
sim$strength[sim$round==1 & sim$agent=='A' & sim$scenario=='L,L;L,L'] <- a$aS
sim$strength[sim$round==1 & sim$agent=='B' & sim$scenario=='L,L;L,L'] <- a$bS
sim$prob[sim$round==2 & sim$agent=='A' & sim$scenario=='L,L;L,L'] <- a$aP*100
sim$prob[sim$round==2 & sim$agent=='B' & sim$scenario=='L,L;L,L'] <- a$bP*100

########################################## R2 Effort Strength ##########################################
# F,F;F,F
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == false)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var output = {aE: optE(x2a,'box',highR), bE: optE(x2b,'box',highR), aS: listMean(x2a), bS: listMean(x2b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==2 & sim$agent=='A' & sim$scenario=='F,F;F,F'] <- a$aE*100
sim$effort[sim$round==2 & sim$agent=='B' & sim$scenario=='F,F;F,F'] <- a$bE*100
sim$strength[sim$round==2 & sim$agent=='A' & sim$scenario=='F,F;F,F'] <- a$aS
sim$strength[sim$round==2 & sim$agent=='B' & sim$scenario=='F,F;F,F'] <- a$bS


# F,F;F,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var output = {aE: optE(x2a,'box',highR), bE: optE(x2b,'box',highR), aS: listMean(x2a), bS: listMean(x2b)}
output
"
# a <- webppl(mdl)

sim$effort[sim$round==2 & sim$agent=='A' & sim$scenario=='F,F;F,L'] <- NaN
sim$effort[sim$round==2 & sim$agent=='B' & sim$scenario=='F,F;F,L'] <- NaN
sim$strength[sim$round==2 & sim$agent=='A' & sim$scenario=='F,F;F,L'] <- NaN
sim$strength[sim$round==2 & sim$agent=='B' & sim$scenario=='F,F;F,L'] <- NaN


# F,L;F,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var output = {aE: optE(x2a,'box',highR), bE: optE(x2b,'box',highR), aS: listMean(x2a), bS: listMean(x2b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==2 & sim$agent=='A' & sim$scenario=='F,L;F,L'] <- a$aE*100
sim$effort[sim$round==2 & sim$agent=='B' & sim$scenario=='F,L;F,L'] <- a$bE*100
sim$strength[sim$round==2 & sim$agent=='A' & sim$scenario=='F,L;F,L'] <- a$aS
sim$strength[sim$round==2 & sim$agent=='B' & sim$scenario=='F,L;F,L'] <- a$bS


# F,F;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var output = {aE: optE(x2a,'box',highR), bE: optE(x2b,'box',highR), aS: listMean(x2a), bS: listMean(x2b)}
output
"
# a <- webppl(mdl)

sim$effort[sim$round==2 & sim$agent=='A' & sim$scenario=='F,F;L,L'] <- NaN
sim$effort[sim$round==2 & sim$agent=='B' & sim$scenario=='F,F;L,L'] <- NaN
sim$strength[sim$round==2 & sim$agent=='A' & sim$scenario=='F,F;L,L'] <- NaN
sim$strength[sim$round==2 & sim$agent=='B' & sim$scenario=='F,F;L,L'] <- NaN


# F,L;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var output = {aE: optE(x2a,'box',highR), bE: optE(x2b,'box',highR), aS: listMean(x2a), bS: listMean(x2b)}
output
"
# a <- webppl(mdl)

sim$effort[sim$round==2 & sim$agent=='A' & sim$scenario=='F,L;L,L'] <- NaN
sim$effort[sim$round==2 & sim$agent=='B' & sim$scenario=='F,L;L,L'] <- NaN
sim$strength[sim$round==2 & sim$agent=='A' & sim$scenario=='F,L;L,L'] <- NaN
sim$strength[sim$round==2 & sim$agent=='B' & sim$scenario=='F,L;L,L'] <- NaN


# L,L;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == true)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var output = {aE: optE(x2a,'box',highR), bE: optE(x2b,'box',highR), aS: listMean(x2a), bS: listMean(x2b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==2 & sim$agent=='A' & sim$scenario=='L,L;L,L'] <- a$aE*100
sim$effort[sim$round==2 & sim$agent=='B' & sim$scenario=='L,L;L,L'] <- a$bE*100
sim$strength[sim$round==2 & sim$agent=='A' & sim$scenario=='L,L;L,L'] <- a$aS
sim$strength[sim$round==2 & sim$agent=='B' & sim$scenario=='L,L;L,L'] <- a$bS


########################################## R3 Probability ##########################################
# F,F;F,F
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == false)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var jointUtility = function(init_effort,a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }

  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(depth,reward) {
      var effort2 = b(depth - 1,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(depth,reward) {
      var effort2 = depth===0 ? init_effort : a(depth,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var findDepth = function(x) {
     if (b(x,r3_reward) == b(x+1,r3_reward)) {
       return x;
     } else {
       return -1;
     }
   };

  var ds = [1,2,5,10];
  var d = function() {
     if (findDepth(ds[0]) > 0) {
       return ds[0]
     } else if (findDepth(ds[1]) > 0) {
       return ds[1]
     } else if (findDepth(ds[2]) > 0) {
       return ds[2]
     } else if (findDepth(ds[3]) > 0) {
       return ds[3]
     } else {
       display('Effort could not converge in ' + ds[3] + ' iterations. Increase the number of iterations and try again.')
     }
   };

  var depth = d()
  var aE = a(depth+1,r3_reward)
  var bE = b(depth,r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var startingEffort = function(a_strength,b_strength) {
  return argMax(
    function(init_effort) {
      var tbl = jointUtility(init_effort,a_strength,b_strength)
      return tbl.jointU
    },
    efforts);
};

var startingE = startingEffort(x2a,x2b)
var output = {P: jointUtility(startingE,x2a,x2b).outcome}
output
"
a <- webppl(mdl)
sim$prob[sim$round==3 & sim$scenario=='F,F;F,F'] <- a$P*100


# F,F;F,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var jointUtility = function(init_effort,a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }

  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(depth,reward) {
      var effort2 = b(depth - 1,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(depth,reward) {
      var effort2 = depth===0 ? init_effort : a(depth,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var findDepth = function(x) {
     if (b(x,r3_reward) == b(x+1,r3_reward)) {
       return x;
     } else {
       return -1;
     }
   };

  var ds = [1,2,5,10];
  var d = function() {
     if (findDepth(ds[0]) > 0) {
       return ds[0]
     } else if (findDepth(ds[1]) > 0) {
       return ds[1]
     } else if (findDepth(ds[2]) > 0) {
       return ds[2]
     } else if (findDepth(ds[3]) > 0) {
       return ds[3]
     } else {
       display('Effort could not converge in ' + ds[3] + ' iterations. Increase the number of iterations and try again.')
     }
   };

  var depth = d()
  var aE = a(depth+1,r3_reward)
  var bE = b(depth,r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var startingEffort = function(a_strength,b_strength) {
  return argMax(
    function(init_effort) {
      var tbl = jointUtility(init_effort,a_strength,b_strength)
      return tbl.jointU
    },
    efforts);
};

var startingE = startingEffort(x2a,x2b)
var output = {P: jointUtility(startingE,x2a,x2b).outcome}
output
"
# a <- webppl(mdl)
sim$prob[sim$round==3 & sim$scenario=='F,F;F,L'] <- NaN


# F,L;F,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var jointUtility = function(init_effort,a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }

  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(depth,reward) {
      var effort2 = b(depth - 1,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(depth,reward) {
      var effort2 = depth===0 ? init_effort : a(depth,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var findDepth = function(x) {
     if (b(x,r3_reward) == b(x+1,r3_reward)) {
       return x;
     } else {
       return -1;
     }
   };

  var ds = [1,2,5,10];
  var d = function() {
     if (findDepth(ds[0]) > 0) {
       return ds[0]
     } else if (findDepth(ds[1]) > 0) {
       return ds[1]
     } else if (findDepth(ds[2]) > 0) {
       return ds[2]
     } else if (findDepth(ds[3]) > 0) {
       return ds[3]
     } else {
       display('Effort could not converge in ' + ds[3] + ' iterations. Increase the number of iterations and try again.')
     }
   };

  var depth = d()
  var aE = a(depth+1,r3_reward)
  var bE = b(depth,r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var startingEffort = function(a_strength,b_strength) {
  return argMax(
    function(init_effort) {
      var tbl = jointUtility(init_effort,a_strength,b_strength)
      return tbl.jointU
    },
    efforts);
};

var startingE = startingEffort(x2a,x2b)
var output = {P: jointUtility(startingE,x2a,x2b).outcome}
output
"
a <- webppl(mdl)
sim$prob[sim$round==3 & sim$scenario=='F,L;F,L'] <- a$P*100


# F,F;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var jointUtility = function(init_effort,a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }

  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(depth,reward) {
      var effort2 = b(depth - 1,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(depth,reward) {
      var effort2 = depth===0 ? init_effort : a(depth,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var findDepth = function(x) {
     if (b(x,r3_reward) == b(x+1,r3_reward)) {
       return x;
     } else {
       return -1;
     }
   };

  var ds = [1,2,5,10];
  var d = function() {
     if (findDepth(ds[0]) > 0) {
       return ds[0]
     } else if (findDepth(ds[1]) > 0) {
       return ds[1]
     } else if (findDepth(ds[2]) > 0) {
       return ds[2]
     } else if (findDepth(ds[3]) > 0) {
       return ds[3]
     } else {
       display('Effort could not converge in ' + ds[3] + ' iterations. Increase the number of iterations and try again.')
     }
   };

  var depth = d()
  var aE = a(depth+1,r3_reward)
  var bE = b(depth,r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var startingEffort = function(a_strength,b_strength) {
  return argMax(
    function(init_effort) {
      var tbl = jointUtility(init_effort,a_strength,b_strength)
      return tbl.jointU
    },
    efforts);
};

var startingE = startingEffort(x2a,x2b)
var output = {P: jointUtility(startingE,x2a,x2b).outcome}
output
"
# a <- webppl(mdl)
sim$prob[sim$round==3 & sim$scenario=='F,F;L,L'] <- NaN


# F,L;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var jointUtility = function(init_effort,a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }

  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(depth,reward) {
      var effort2 = b(depth - 1,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(depth,reward) {
      var effort2 = depth===0 ? init_effort : a(depth,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var findDepth = function(x) {
     if (b(x,r3_reward) == b(x+1,r3_reward)) {
       return x;
     } else {
       return -1;
     }
   };

  var ds = [1,2,5,10];
  var d = function() {
     if (findDepth(ds[0]) > 0) {
       return ds[0]
     } else if (findDepth(ds[1]) > 0) {
       return ds[1]
     } else if (findDepth(ds[2]) > 0) {
       return ds[2]
     } else if (findDepth(ds[3]) > 0) {
       return ds[3]
     } else {
       display('Effort could not converge in ' + ds[3] + ' iterations. Increase the number of iterations and try again.')
     }
   };

  var depth = d()
  var aE = a(depth+1,r3_reward)
  var bE = b(depth,r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var startingEffort = function(a_strength,b_strength) {
  return argMax(
    function(init_effort) {
      var tbl = jointUtility(init_effort,a_strength,b_strength)
      return tbl.jointU
    },
    efforts);
};

var startingE = startingEffort(x2a,x2b)
var output = {P: jointUtility(startingE,x2a,x2b).outcome}
output
"
# a <- webppl(mdl)
sim$prob[sim$round==3 & sim$scenario=='F,L;L,L'] <- NaN


# L,L;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var x2a = [], x2b = []
var samples2 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == true)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  x2a.push(sa)
  x2b.push(sb)
  return 0
}})

var jointUtility = function(init_effort,a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }

  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(depth,reward) {
      var effort2 = b(depth - 1,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(depth,reward) {
      var effort2 = depth===0 ? init_effort : a(depth,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var findDepth = function(x) {
     if (b(x,r3_reward) == b(x+1,r3_reward)) {
       return x;
     } else {
       return -1;
     }
   };

  var ds = [1,2,5,10];
  var d = function() {
     if (findDepth(ds[0]) > 0) {
       return ds[0]
     } else if (findDepth(ds[1]) > 0) {
       return ds[1]
     } else if (findDepth(ds[2]) > 0) {
       return ds[2]
     } else if (findDepth(ds[3]) > 0) {
       return ds[3]
     } else {
       display('Effort could not converge in ' + ds[3] + ' iterations. Increase the number of iterations and try again.')
     }
   };

  var depth = d()
  var aE = a(depth+1,r3_reward)
  var bE = b(depth,r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var startingEffort = function(a_strength,b_strength) {
  return argMax(
    function(init_effort) {
      var tbl = jointUtility(init_effort,a_strength,b_strength)
      return tbl.jointU
    },
    efforts);
};

var startingE = startingEffort(x2a,x2b)
var output = {P: jointUtility(startingE,x2a,x2b).outcome}
output
"
a <- webppl(mdl)
sim$prob[sim$round==3 & sim$scenario=='L,L;L,L'] <- a$P*100


########################################## R3 Effort Strength ##########################################
# F,F;F,F
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var jointUtility = function(init_effort,a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }

  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(depth,reward) {
      var effort2 = b(depth - 1,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(depth,reward) {
      var effort2 = depth===0 ? init_effort : a(depth,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var findDepth = function(x) {
     if (b(x,r3_reward) == b(x+1,r3_reward)) {
       return x;
     } else {
       return -1;
     }
   };

  var ds = [1,2,5,10];
  var d = function() {
     if (findDepth(ds[0]) > 0) {
       return ds[0]
     } else if (findDepth(ds[1]) > 0) {
       return ds[1]
     } else if (findDepth(ds[2]) > 0) {
       return ds[2]
     } else if (findDepth(ds[3]) > 0) {
       return ds[3]
     } else {
       display('Effort could not converge in ' + ds[3] + ' iterations. Increase the number of iterations and try again.')
     }
   };

  var depth = d()
  var aE = a(depth+1,r3_reward)
  var bE = b(depth,r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var startingEffort = function(a_strength,b_strength) {
  return argMax(
    function(init_effort) {
      var tbl = jointUtility(init_effort,a_strength,b_strength)
      return tbl.jointU
    },
    efforts);
};

var x3a = [], x3b = []
var samples3 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == false)
  condition(jointUtility(startingEffort(sa,sb),sa,sb).outcome == false)
  x3a.push(sa)
  x3b.push(sb)
  return 0
}})

var startingE = startingEffort(x3a,x3b)
var ju = jointUtility(startingE,x3a,x3b)

var output = {aE: ju.aE, bE: ju.bE, aS: listMean(x3a), bS: listMean(x3b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==3 & sim$agent=='A' & sim$scenario=='F,F;F,F'] <- a$aE*100
sim$effort[sim$round==3 & sim$agent=='B' & sim$scenario=='F,F;F,F'] <- a$bE*100
sim$strength[sim$round==3 & sim$agent=='A' & sim$scenario=='F,F;F,F'] <- a$aS
sim$strength[sim$round==3 & sim$agent=='B' & sim$scenario=='F,F;F,F'] <- a$bS


# F,F;F,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var jointUtility = function(init_effort,a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }

  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(depth,reward) {
      var effort2 = b(depth - 1,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(depth,reward) {
      var effort2 = depth===0 ? init_effort : a(depth,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var findDepth = function(x) {
     if (b(x,r3_reward) == b(x+1,r3_reward)) {
       return x;
     } else {
       return -1;
     }
   };

  var ds = [1,2,5,10];
  var d = function() {
     if (findDepth(ds[0]) > 0) {
       return ds[0]
     } else if (findDepth(ds[1]) > 0) {
       return ds[1]
     } else if (findDepth(ds[2]) > 0) {
       return ds[2]
     } else if (findDepth(ds[3]) > 0) {
       return ds[3]
     } else {
       display('Effort could not converge in ' + ds[3] + ' iterations. Increase the number of iterations and try again.')
     }
   };

  var depth = d()
  var aE = a(depth+1,r3_reward)
  var bE = b(depth,r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var startingEffort = function(a_strength,b_strength) {
  return argMax(
    function(init_effort) {
      var tbl = jointUtility(init_effort,a_strength,b_strength)
      return tbl.jointU
    },
    efforts);
};

var x3a = [], x3b = []
var samples3 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == true)
  condition(jointUtility(startingEffort(sa,sb),sa,sb).outcome == false)
  x3a.push(sa)
  x3b.push(sb)
  return 0
}})

var startingE = startingEffort(x3a,x3b)
var ju = jointUtility(startingE,x3a,x3b)

var output = {aE: ju.aE, bE: ju.bE, aS: listMean(x3a), bS: listMean(x3b)}
output
"
# a <- webppl(mdl)

sim$effort[sim$round==3 & sim$agent=='A' & sim$scenario=='F,F;F,L'] <- NaN
sim$effort[sim$round==3 & sim$agent=='B' & sim$scenario=='F,F;F,L'] <- NaN
sim$strength[sim$round==3 & sim$agent=='A' & sim$scenario=='F,F;F,L'] <- NaN
sim$strength[sim$round==3 & sim$agent=='B' & sim$scenario=='F,F;F,L'] <- NaN


# F,L;F,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var jointUtility = function(init_effort,a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }

  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(depth,reward) {
      var effort2 = b(depth - 1,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(depth,reward) {
      var effort2 = depth===0 ? init_effort : a(depth,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var findDepth = function(x) {
     if (b(x,r3_reward) == b(x+1,r3_reward)) {
       return x;
     } else {
       return -1;
     }
   };

  var ds = [1,2,5,10];
  var d = function() {
     if (findDepth(ds[0]) > 0) {
       return ds[0]
     } else if (findDepth(ds[1]) > 0) {
       return ds[1]
     } else if (findDepth(ds[2]) > 0) {
       return ds[2]
     } else if (findDepth(ds[3]) > 0) {
       return ds[3]
     } else {
       display('Effort could not converge in ' + ds[3] + ' iterations. Increase the number of iterations and try again.')
     }
   };

  var depth = d()
  var aE = a(depth+1,r3_reward)
  var bE = b(depth,r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var startingEffort = function(a_strength,b_strength) {
  return argMax(
    function(init_effort) {
      var tbl = jointUtility(init_effort,a_strength,b_strength)
      return tbl.jointU
    },
    efforts);
};

var x3a = [], x3b = []
var samples3 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == false)
  condition(outcome(sb,'box',highR) == true)
  condition(jointUtility(startingEffort(sa,sb),sa,sb).outcome == true)
  x3a.push(sa)
  x3b.push(sb)
  return 0
}})

var startingE = startingEffort(x3a,x3b)
var ju = jointUtility(startingE,x3a,x3b)

var output = {aE: ju.aE, bE: ju.bE, aS: listMean(x3a), bS: listMean(x3b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==3 & sim$agent=='A' & sim$scenario=='F,L;F,L'] <- a$aE*100
sim$effort[sim$round==3 & sim$agent=='B' & sim$scenario=='F,L;F,L'] <- a$bE*100
sim$strength[sim$round==3 & sim$agent=='A' & sim$scenario=='F,L;F,L'] <- a$aS
sim$strength[sim$round==3 & sim$agent=='B' & sim$scenario=='F,L;F,L'] <- a$bS


# F,F;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var jointUtility = function(init_effort,a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }

  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(depth,reward) {
      var effort2 = b(depth - 1,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(depth,reward) {
      var effort2 = depth===0 ? init_effort : a(depth,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var findDepth = function(x) {
     if (b(x,r3_reward) == b(x+1,r3_reward)) {
       return x;
     } else {
       return -1;
     }
   };

  var ds = [1,2,5,10];
  var d = function() {
     if (findDepth(ds[0]) > 0) {
       return ds[0]
     } else if (findDepth(ds[1]) > 0) {
       return ds[1]
     } else if (findDepth(ds[2]) > 0) {
       return ds[2]
     } else if (findDepth(ds[3]) > 0) {
       return ds[3]
     } else {
       display('Effort could not converge in ' + ds[3] + ' iterations. Increase the number of iterations and try again.')
     }
   };

  var depth = d()
  var aE = a(depth+1,r3_reward)
  var bE = b(depth,r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var startingEffort = function(a_strength,b_strength) {
  return argMax(
    function(init_effort) {
      var tbl = jointUtility(init_effort,a_strength,b_strength)
      return tbl.jointU
    },
    efforts);
};

var x3a = [], x3b = []
var samples3 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == false)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  condition(jointUtility(startingEffort(sa,sb),sa,sb).outcome == true)
  x3a.push(sa)
  x3b.push(sb)
  return 0
}})

var startingE = startingEffort(x3a,x3b)
var ju = jointUtility(startingE,x3a,x3b)

var output = {aE: ju.aE, bE: ju.bE, aS: listMean(x3a), bS: listMean(x3b)}
output
"
# a <- webppl(mdl)

sim$effort[sim$round==3 & sim$agent=='A' & sim$scenario=='F,F;L,L'] <- NaN
sim$effort[sim$round==3 & sim$agent=='B' & sim$scenario=='F,F;L,L'] <- NaN
sim$strength[sim$round==3 & sim$agent=='A' & sim$scenario=='F,F;L,L'] <- NaN
sim$strength[sim$round==3 & sim$agent=='B' & sim$scenario=='F,F;L,L'] <- NaN


# F,L;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var jointUtility = function(init_effort,a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }

  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(depth,reward) {
      var effort2 = b(depth - 1,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(depth,reward) {
      var effort2 = depth===0 ? init_effort : a(depth,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var findDepth = function(x) {
     if (b(x,r3_reward) == b(x+1,r3_reward)) {
       return x;
     } else {
       return -1;
     }
   };

  var ds = [1,2,5,10];
  var d = function() {
     if (findDepth(ds[0]) > 0) {
       return ds[0]
     } else if (findDepth(ds[1]) > 0) {
       return ds[1]
     } else if (findDepth(ds[2]) > 0) {
       return ds[2]
     } else if (findDepth(ds[3]) > 0) {
       return ds[3]
     } else {
       display('Effort could not converge in ' + ds[3] + ' iterations. Increase the number of iterations and try again.')
     }
   };

  var depth = d()
  var aE = a(depth+1,r3_reward)
  var bE = b(depth,r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var startingEffort = function(a_strength,b_strength) {
  return argMax(
    function(init_effort) {
      var tbl = jointUtility(init_effort,a_strength,b_strength)
      return tbl.jointU
    },
    efforts);
};

var x3a = [], x3b = []
var samples3 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == false)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  condition(jointUtility(startingEffort(sa,sb),sa,sb).outcome == true)
  x3a.push(sa)
  x3b.push(sb)
  return 0
}})

var startingE = startingEffort(x3a,x3b)
var ju = jointUtility(startingE,x3a,x3b)

var output = {aE: ju.aE, bE: ju.bE, aS: listMean(x3a), bS: listMean(x3b)}
output
"
# a <- webppl(mdl)

sim$effort[sim$round==3 & sim$agent=='A' & sim$scenario=='F,L;L,L'] <- NaN
sim$effort[sim$round==3 & sim$agent=='B' & sim$scenario=='F,L;L,L'] <- NaN
sim$strength[sim$round==3 & sim$agent=='A' & sim$scenario=='F,L;L,L'] <- NaN
sim$strength[sim$round==3 & sim$agent=='B' & sim$scenario=='F,L;L,L'] <- NaN


# L,L;L,L
mdl <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 13.5, beta = 24.5
var weight = mem(function (box) {return 5})
var lowR = 10
var highR = 20

var efforts = [1]
var lift = function(strength,box,effort){
  return (effort*strength >= weight(box))
}

var optE = function(strength,box,reward) {
  return argMax(
    function(effort) {
      if (strength.length > 1)
        {return reward*listMean(map(function(i){return lift(i,box,effort)}, strength)) - alpha*effort}
        else 
          {return reward*lift(strength,box,effort) - alpha*effort}
    },
    efforts);
};

var outcome = function(strength,box,reward) {
  if (strength.length > 1)
    { var opt_effort = optE(strength,box,reward)
      return listMean(map(function(i){return lift(i,box,opt_effort)}, strength))}
  else 
  {return lift(strength,box,optE(strength,box,reward))}
}

var jointUtility = function(init_effort,a_strength,b_strength){
  var r3_reward = highR
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }

  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(depth,reward) {
      var effort2 = b(depth - 1,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(depth,reward) {
      var effort2 = depth===0 ? init_effort : a(depth,reward)
      var optEffort = function(strength,strength2,box,reward) {
        return argMax(
          function(effort) {
            if (a_strength.length > 1) {
              return reward*listMean(map2(function(i,j){return lift2(i,j,box,effort,effort2)}, strength,strength2)) - alpha*effort - beta*gini(effort,effort2)
            } else {
              return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
            }
          },
          efforts);
      };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var findDepth = function(x) {
     if (b(x,r3_reward) == b(x+1,r3_reward)) {
       return x;
     } else {
       return -1;
     }
   };

  var ds = [1,2,5,10];
  var d = function() {
     if (findDepth(ds[0]) > 0) {
       return ds[0]
     } else if (findDepth(ds[1]) > 0) {
       return ds[1]
     } else if (findDepth(ds[2]) > 0) {
       return ds[2]
     } else if (findDepth(ds[3]) > 0) {
       return ds[3]
     } else {
       display('Effort could not converge in ' + ds[3] + ' iterations. Increase the number of iterations and try again.')
     }
   };

  var depth = d()
  var aE = a(depth+1,r3_reward)
  var bE = b(depth,r3_reward)
  
  var outcome2 = function(a_strength,b_strength,box) {
    if (a_strength.length > 1) { 
      return listMean(map2(function(i,j){return lift2(i,j,box,aE,bE)}, a_strength,b_strength))
    } else {
      return lift2(a_strength,b_strength,box,aE,bE)
    }
  }
    
  if (a_strength.length > 1) {
    var aU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',aE,bE)}, a_strength,b_strength)) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*listMean(map2(function(i,j){return lift2(i,j,'box',bE,aE)}, b_strength,a_strength)) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  } else {
    var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
    var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
    var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
    return table
  }
}

var startingEffort = function(a_strength,b_strength) {
  return argMax(
    function(init_effort) {
      var tbl = jointUtility(init_effort,a_strength,b_strength)
      return tbl.jointU
    },
    efforts);
};

var x3a = [], x3b = []
var samples3 = Infer({ method: 'MCMC', kernel: 'MH', samples: 10000, burn: 1000, model() {
  var sa = uniform(1,10)
  var sb = uniform(1,10)
  condition(outcome(sa,'box',lowR) == true)
  condition(outcome(sb,'box',lowR) == true)
  condition(outcome(sa,'box',highR) == true)
  condition(outcome(sb,'box',highR) == true)
  condition(jointUtility(startingEffort(sa,sb),sa,sb).outcome == true)
  x3a.push(sa)
  x3b.push(sb)
  return 0
}})

var startingE = startingEffort(x3a,x3b)
var ju = jointUtility(startingE,x3a,x3b)

var output = {aE: ju.aE, bE: ju.bE, aS: listMean(x3a), bS: listMean(x3b)}
output
"
a <- webppl(mdl)

sim$effort[sim$round==3 & sim$agent=='A' & sim$scenario=='L,L;L,L'] <- a$aE*100
sim$effort[sim$round==3 & sim$agent=='B' & sim$scenario=='L,L;L,L'] <- a$bE*100
sim$strength[sim$round==3 & sim$agent=='A' & sim$scenario=='L,L;L,L'] <- a$aS
sim$strength[sim$round==3 & sim$agent=='B' & sim$scenario=='L,L;L,L'] <- a$bS

maximum <- sim

inference <- rbind(joint,solitary,compensatory,maximum)
write.csv(inference,"inference.csv", row.names = FALSE)


