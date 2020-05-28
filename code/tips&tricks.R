#convert to symbol
var <- sym(l_co.comor[1])

# force `var`, which substitutes it with `height`

for(n in 1:length(l_co.comor)){
var <- sym(l_co.comor[n])
print(cmr %>%
  summarise(avg = mean(!!var, na.rm = TRUE)))
}

#The bang-bang operator !! forces a single object. One common case for !! is to substitute an environment-variable (created with <-) with a data-variable (inside a data frame).

#The big-bang operator !!! forces-splice a list of objects. The elements of the list are spliced in place, meaning that they each become one single argument.

for(n in 1:length(l_comor)){
  var1 <- sym(l_co.comor[n])
  var2 <- sym(l_comor[n])
  cmr <- cmr %>% mutate(var1 = ifelse(var2 == "Yes",1,ifelse(var2 == "No",0,NA))) 
}


#! GET FUNCTION AND [[]]
#print responses
l_response <- paste0("response.0",1:5)
for(n in 1:length(l_response)){
  print(get(l_response[[n]]))
}


cmr[,l_co.comor[[1]]]
