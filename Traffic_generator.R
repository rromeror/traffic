traffic_pattern <- function (service_type,commited = 1, peak = 200, gap, range = 1:2, time_intervals = 1, distribution){
 
 traffic <- vector("numeric", length = time_intervals)
 traffic = rnorm(length(traffic),commited,gap)
  if(service_type == 1){
       traffic[range] = rnorm(length(range),peak,gap) 
       m = (peak-commited)/(range[1]-4)
       traffic[range[1]-3] = -2*m + peak + rnorm(1,0,gap)
       traffic[range[1]-2] = -m + peak + rnorm(1,0,gap)
       traffic[range[1]-1] = peak + rnorm(1,0,gap)
       traffic[range[length(range)]+1] = -m  + peak + rnorm(1,0,gap)
       traffic[range[length(range)]+2] = -2*m + peak + rnorm(1,0,gap)
       traffic[range[length(range)]+3] = -3*m + peak + rnorm(1,0,gap)
       print("Normal Business")
   }else if (service_type == 2){
       traffic[range] = rnorm(length(range),peak,gap)     
       print("Transactional Business")  
   }else if (service_type == 3){
       print("Internet")
   }else {
       print("Non existing service profile!")
       traffic <- NULL
   }
   print(traffic)
   plot(traffic)
   traffic <- data.frame("Time Slot" = 1:length(traffic), "Bandwidth" = traffic )
   traffic
}



