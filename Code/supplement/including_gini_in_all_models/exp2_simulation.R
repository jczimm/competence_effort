library('rwebppl')
library('tidyverse')

# Minimum incentives for the stronger contestant and the weaker contestant
# to lift the box of weight 5 alone:
strong <- c(65,65,65,75,75,75,85,85,85,95,95,95,105,105,105)
weak <- c(65,95,125,75,95,125,85,105,125,95,115,125,105,115,125)

########################################## Joint and maximum ##########################################
effort_space_joint <- "var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]"
effort_space_maximum <- "var efforts = [1]"

mdl1 <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 125, beta = 24.5
var weight = mem(function (box) {return 5})

var min_incentive = ["
mdl2 <- "]
var strength = map(function(x){return alpha*weight('box')/x}, min_incentive)

var jointUtility = function(init_effort,a_strength,b_strength,r3_reward){
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }
  
  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(depth,reward) {
    var effort2 = b(depth - 1,reward)
    var optEffort = function(strength,strength2,box,reward) {
      return argMax(
        function(effort) {
          return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
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
          return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
        },
        efforts);
    };
    return optEffort(b_strength,a_strength,'box',reward)
  }
  
  var findDepth = function(x) {
    if (Math.abs(b(x,r3_reward) - b(x+1,r3_reward)) < 0.06) {
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
    return lift2(a_strength,b_strength,box,aE,bE)
  }
  
  var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
  var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
  var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
  return table
}

var startingEffort = function(a_strength,b_strength,r3_reward) {
  return argMax(
    function(init_effort) {
      var tbl = jointUtility(init_effort,a_strength,b_strength,r3_reward)
      return tbl.jointU
    },
    efforts);
};

var max_incentive = 200
var output = mapN(function(x){ return jointUtility(startingEffort(strength[0],strength[1],x),strength[0],strength[1],x).outcome; }, max_incentive+1)
output
"

incentive_joint <- NULL
for (i in 1:15) {
  mdl <- paste0(effort_space_joint, mdl1, as.character(strong[i]), ",", as.character(weak[i]), mdl2)
  a <- webppl(mdl)
  incentive <- which(a==T)[1]-1
  incentive_joint <- c(incentive_joint, incentive)
}

incentive_maximum <- NULL
for (i in 1:15) {
  mdl <- paste0(effort_space_maximum, mdl1, as.character(strong[i]), ",", as.character(weak[i]), mdl2)
  a <- webppl(mdl)
  incentive <- which(a==T)[1]-1
  incentive_maximum <- c(incentive_maximum, incentive)
}


########################################## Solitary and compensatory ##########################################
others_effort_solitary <- "var others_effort = 0"
others_effort_compensatory <- "var others_effort = 1"

mdl1 <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};

var alpha = 125, beta = 24.5
var weight = mem(function (box) {return 5})

var min_incentive = ["
mdl2 <- "]
var strength = map(function(x){return alpha*weight('box')/x}, min_incentive)
var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]

var jointUtility = function(a_strength,b_strength,r3_reward){
  
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }
  
  var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}
  
  var a = function(reward) {
    var effort2 = others_effort
    var optEffort = function(strength,strength2,box,reward) {
      return argMax(
        function(effort) {
          return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
        },
        efforts);
    };
    return optEffort(a_strength,b_strength,'box',reward)
  }
  
  var b = function(reward) {
    var effort2 = others_effort
    var optEffort = function(strength,strength2,box,reward) {
      return argMax(
        function(effort) {
          return reward*lift2(strength,strength2,box,effort,effort2) - alpha*effort - beta*gini(effort,effort2)
        },
        efforts);
    };
    return optEffort(b_strength,a_strength,'box',reward)
  }

  var aE = a(r3_reward)
  var bE = b(r3_reward)

  var outcome2 = function(a_strength,b_strength,box) {
    return lift2(a_strength,b_strength,box,aE,bE)
  }
  
  var aU = r3_reward*lift2(a_strength,b_strength,'box',aE,bE) - alpha*aE - beta*gini(aE,bE)
  var bU = r3_reward*lift2(b_strength,a_strength,'box',bE,aE) - alpha*bE - beta*gini(bE,aE)
  var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength};
  return table
}

var max_incentive = 200
var output = mapN(function(x){ return jointUtility(strength[0],strength[1],x).outcome; }, max_incentive+1)
output
"

incentive_solitary <- NULL
for (i in 1:15) {
  mdl <- paste0(others_effort_solitary, mdl1, as.character(strong[i]), ",", as.character(weak[i]), mdl2)
  a <- webppl(mdl)
  incentive <- which(a==T)[1]-1
  incentive_solitary <- c(incentive_solitary, incentive)
}

incentive_compensatory <- NULL
for (i in 1:15) {
  mdl <- paste0(others_effort_compensatory, mdl1, as.character(strong[i]), ",", as.character(weak[i]), mdl2)
  a <- webppl(mdl)
  incentive <- which(a==T)[1]-1
  incentive_compensatory <- c(incentive_compensatory, incentive)
}

incentive <- c(incentive_joint, incentive_solitary, incentive_compensatory, incentive_maximum)
incentive[is.na(incentive)==T] <- 0

df <- data.frame(model=c(rep('joint',15),rep('solitary',15),rep('compensatory',15),rep('maximum',15)))
df$model <- factor(df$model,levels = c('joint','solitary','compensatory','maximum'), ordered = T)
# stronger contestant's strength
df$strong_strength <- rep(c(9.5, 9.5, 9.5, 8.0, 8.0, 8.0, 7.5, 7.5, 7.5, 6.5, 6.5, 6.5, 6.0, 6.0, 6.0), 4)
# weaker contestant's strength
df$weak_strength <- rep(c(9.5, 6.5, 5.0, 8.0, 6.5, 5.0, 7.5, 6.0, 5.0, 6.5, 5.5, 5.0, 6.0, 5.5, 5.0), 4)
# stronger contestant's minimum incentive to lift the box alone
df$strong_incentive <- rep(c(65, 65, 65, 75, 75, 75, 85, 85, 85, 95, 95, 95, 105, 105, 105), 4)
# weaker contestant's minimum incentive to lift the box alone
df$weak_incentive <- rep(c(65, 95, 125, 75, 95, 125, 85, 105, 125, 95, 115, 125, 105, 115, 125), 4)
df$incentive <- incentive

df <- arrange(df, model, strong_strength, weak_strength)
df <- df[, c('model', 'strong_strength', 'strong_incentive', 'weak_strength', 'weak_incentive', 'incentive')]
write.csv(df,"exp2_simulation.csv", row.names = FALSE)
