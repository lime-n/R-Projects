#Helpful R code

#Group data, and mutate proportions across
data_with_proportion %>%
  group_by(type1, type2) %>%
  mutate(proportion = weight / sum(weight)) %>%
  ungroup() %>%
  mutate(across(5:7,
         ~.*proportion,   #apply a function here; In this case multply by proportion
         .names = "{col}_new"    #assign new column names
         ))