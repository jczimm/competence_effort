library('rwebppl')
library('tidyverse')

box_weights <- c(6,7,10)

regular <- "
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= weight(box)
  }
  "
safe <- "
  var k = 3.5
  var lift2 = function(strength,strength2,box,effort,effort2){
    return (effort*strength + effort2*strength2) >= (weight(box) + ((1-effort)+(1-effort2))*k)
  }
"
with_gini <- "var gini = function(effort, effort2) {return (effort == effort2 ? 0 : Math.abs(effort-effort2)/4/(effort+effort2))}"
without_gini <- "var gini = function(effort, effort2) {return 0}"

mdl1 <- "
var argMax = function(f, ar){
  return maxWith(f, ar)[0]
};
var s = [9,5,2], alpha = [40,20,2], beta = 24.5
var weight = mem(function (box) {return "
mdl2 <- "})
var efforts = [0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1]

var jointUtility = function(init_effort,contestant1,contestant2,contestant1_reward,contestant2_reward){
  var a_strength = s[contestant1], b_strength = s[contestant2]
  var a_alpha = alpha[contestant1], b_alpha = alpha[contestant2]
"
mdl3 <- "
  var a = function(depth) {
    var effort2 = b(depth - 1,contestant2_reward)
    var optEffort = function(strength,strength2,box) {
      return argMax(
        function(effort) {
          return contestant1_reward*lift2(strength,strength2,box,effort,effort2) - a_alpha*effort - beta*gini(effort,effort2)
        },
        efforts);
    };
    return optEffort(a_strength,b_strength,'box')
  }
  
  var b = function(depth) {
    var effort2 = depth===0 ? init_effort : a(depth,contestant1_reward)
    var optEffort = function(strength,strength2,box) {
      return argMax(
        function(effort) {
          return contestant2_reward*lift2(strength,strength2,box,effort,effort2) - b_alpha*effort - beta*gini(effort,effort2)
        },
        efforts);
    };
    return optEffort(b_strength,a_strength,'box')
  }
  
  var findDepth = function(x) {
    if (Math.abs(b(x) - b(x+1)) < 0.06) {
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
  var aE = a(depth+1)
  var bE = b(depth)

  var outcome2 = function(a_strength,b_strength,box) {
    return lift2(a_strength,b_strength,box,aE,bE)
  }
  
  var aU = contestant1_reward*lift2(a_strength,b_strength,'box',aE,bE) - a_alpha*aE - beta*gini(aE,bE)
  var bU = contestant2_reward*lift2(b_strength,a_strength,'box',bE,aE) - b_alpha*bE - beta*gini(bE,aE)
  var table = { aU: aU, bU: bU, aE: aE, bE: bE, jointU: aU+bU, outcome: outcome2(a_strength,b_strength,'box'), a_strength: a_strength, b_strength: b_strength, a_alpha: a_alpha, b_alpha: b_alpha, contestant1: contestant1, contestant2: contestant2, contestant1_reward: contestant1_reward, contestant2_reward: contestant2_reward};
  return table
}

var startingEffort = function(contestant1,contestant2,contestant1_reward,contestant2_reward) {
  return argMax(
    function(init_effort) {
      var tbl = jointUtility(init_effort,contestant1,contestant2,contestant1_reward,contestant2_reward)
      return tbl.jointU
    },
    efforts);
};

var budget = 50
var contestant1 = "
mdl4 <- ", contestant2 ="
mdl5 <- "
var fn = function(contestant1_reward){return mapN(function(contestant2_reward){ return jointUtility(startingEffort(contestant1,contestant2,contestant1_reward,contestant2_reward),contestant1,contestant2,contestant1_reward,contestant2_reward); }, budget-contestant1_reward+1)}
var output = mapN(fn, budget+1)
output
"

for (m in 1:3) {
  if (m == 1) {
    gini <- with_gini
    lift2 <- regular
  }
  else if (m == 2) {
    gini <- without_gini
    lift2 <- regular
  }
  else {
    gini <- with_gini
    lift2 <- safe
  }
  incentive_all <- NULL
  for (c in 1:3) {
    incentive_contest <- NULL
    for (j in 1:3) {
      if (j == 1) {
        mdl <- paste0(mdl1, as.character(box_weights[c]), mdl2, gini, lift2, mdl3, as.character(0), mdl4, as.character(1), mdl5)
      } else if (j == 2) {
        mdl <- paste0(mdl1, as.character(box_weights[c]), mdl2, gini, lift2, mdl3, as.character(0), mdl4, as.character(2), mdl5)
      } else {
        mdl <- paste0(mdl1, as.character(box_weights[c]), mdl2, gini, lift2, mdl3, as.character(1), mdl4, as.character(2), mdl5)
      }
      a <- webppl(mdl)
      df <- bind_rows(a)
      if (j == 1) {
        df$contestantA <- df$contestant1_reward
        df$contestantB <- df$contestant2_reward
        df$contestantC <- 0
        df$team <- 'AB'
      } else if (j == 2) {
        df$contestantA <- df$contestant1_reward
        df$contestantB <- 0
        df$contestantC <- df$contestant2_reward
        df$team <- 'AC'
      } else {
        df$contestantA <- 0
        df$contestantB <- df$contestant1_reward
        df$contestantC <- df$contestant2_reward
        df$team <- 'BC'
      }
      df$weight <- box_weights[c]
      df <- df %>% mutate(total = contestantA + contestantB + contestantC)
      df <- df[,c('weight','outcome','contestantA','contestantB','contestantC','team','total')]
      
      if (length(which(df$outcome==T)) == 0) {
        df <- df[1,]
      } else {
        df <- df %>% filter(outcome == T)
        df <- df[df$total == min(df$total),]
        df <- df %>% group_by(weight, outcome, team, total) %>%
          dplyr::summarize(contestantA = mean(contestantA), 
                           contestantB = mean(contestantB), 
                           contestantC = mean(contestantC))
      }
      incentive_contest <- rbind(incentive_contest, df)
    }
    if (length(which(incentive_contest$outcome==T)) != 0) {
      incentive_contest <- incentive_contest[incentive_contest$outcome == T,]
      incentive_contest <- incentive_contest[incentive_contest$total == min(incentive_contest$total),]
    }
    incentive_contest <- gather(incentive_contest, contestant, incentive, contestantA, contestantB, contestantC)
    incentive_contest <- incentive_contest %>% mutate(contestant = case_when(contestant == 'contestantA' ~ 'A',
                                                                             contestant == 'contestantB' ~ 'B',
                                                                             T ~ 'C')) %>% 
      mutate(select = case_when(contestant == 'A' & team != 'BC' ~ 1,
                                contestant == 'A' & team == 'BC' ~ 0,
                                contestant == 'B' & team != 'AC' ~ 1,
                                contestant == 'B' & team == 'AC' ~ 0,
                                contestant == 'C' & team != 'AB' ~ 1,
                                contestant == 'C' & team == 'AB' ~ 0)) %>% 
      mutate(select = select/(nrow(incentive_contest)/3))
    incentive_contest <- incentive_contest %>% group_by(weight, outcome, total, contestant) %>%
      dplyr::summarize(incentive = mean(incentive), 
                       Freq = sum(select))
    incentive_all <- rbind(incentive_all, incentive_contest)
  }
  if (m == 1) {
    joint_w_gini <- incentive_all %>% mutate(model = 'joint_w_gini')
  }
  else if (m == 2) {
    joint_wo_gini <- incentive_all %>% mutate(model = 'joint_wo_gini')
  }
  else {
    safe_joint_w_gini <- incentive_all %>% mutate(model = 'safe_joint_w_gini')
  }
}

df <- rbind(joint_w_gini, joint_wo_gini, safe_joint_w_gini)
df$model <- factor(df$model,levels = c('joint_w_gini','joint_wo_gini','safe_joint_w_gini'), ordered = T)
df <- arrange(df, model, weight, contestant)
df <- df[, c('model','weight','contestant','Freq','incentive','total','outcome')]
write.csv(df,"exp3_simulation.csv", row.names = FALSE)
