# Align Medical Centers to Ebola cases
 
# Step 1. Upload data
# Step 2. Create distance matrix
# Step 3. Run LP for MCs (current vs future)
 
##########
# Step 1
##########
 
# upload data
mc <- read.csv("NetworkPlanningTest/Test/MedCenterClean.csv",
           	stringsAsFactors = F)
str(mc)
# One medical center per location
# This assumes that each MC has unlimited capacity
# Capacity constraints can be the next iteration of the model
 
 
cases <- read.csv("NetworkPlanningTest/Test/Ebola.csv")
str(cases) # change value to numeric
 
 
##########
# Step 2
##########
 
#create a distance matrix between the demand points and current MCs (in meters)
cases_mc_dist1 <- geosphere::distm(
  x = cbind(case_sum$Long, case_sum$Lat),
  y = cbind(mc_current$Long, mc_current$Lat))
 
 
# convert meters to miles then
# multiply the distance by the demand
cases_mc_dist1a <- (cases_mc_dist1/1600) * case_sum$Cases
 
 
#define scalars for the number of disease locations and medical centers
case_count <- 79
mc_count <- 28
 
#now make optimization model
mc_location_model <- ompr::MIPModel() %>%
  add_variable(customer_dc_align[customerindex,dcindex],
           	customerindex=1:case_count,
           	dcindex=1:mc_count,type='binary') %>%
  add_variable(open_dc_binary[dcindex],dcindex=1:mc_count,type='binary') %>%
  add_constraint(sum_expr(customer_dc_align[customerindex,dcindex],
                      	dcindex=1:mc_count)==1,
             	customerindex=1:case_count) %>%
  add_constraint(sum_expr(customer_dc_align[customerindex,dcindex],
                      	customerindex=1:case_count)<=
               	99999*open_dc_binary[dcindex],dcindex=1:mc_count) %>%
  add_constraint(sum_expr(open_dc_binary[dcindex],dcindex=1:mc_count)==28) %>%
  set_objective(sum_expr(customer_dc_align[customerindex,dcindex]*
                       	cases_mc_dist1a[customerindex,dcindex],
                     	customerindex=1:case_count,
                     	dcindex=1:mc_count),sense='min')
 
 
solution <- ompr::solve_model(mc_location_model,with_ROI(solver = "glpk"))
 
c_mc28 <- get_solution(solution,customer_dc_align[customerindex,dcindex]) %>%
  dplyr::filter(value==1)
 
# get distance between each DC and customer
# see how distance changes as we increase # of DCs (elbow curve)


dist_long <- gather(cases_mc_dist1, dcindex, distance, V1:V28, factor_key = TRUE)
dist_long$dcindex <- as.numeric(gsub("V", "", dist_long$dcindex))
dist_long$customerindex <- as.numeric(as.character(dist_long$customerindex))
c_mc28a <- left_join(c_mc28, dist_long,
                     by= c("dcindex", "customerindex"))
 
# Tie Locality back to index for disease location and MC
case_sum <- case_sum %>%
  ungroup() %>%
  mutate(customerindex = row_number())
 
mc_current <- mutate(mc_current, dcindex = row_number())
c_mc28a <- left_join(c_mc28a, case_sum, by = "customerindex")
c_mc28a <- left_join(c_mc28a, mc_current, by = "dcindex")
 
 # Export 
write.csv(c_mc28a, "NetworkPlanningTest/Test/mc_assign_current.csv")
 
