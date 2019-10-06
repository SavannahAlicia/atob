#' Generate Lists of Longitudes and Latitudes for Points Along Routes
#'
#' This function returns lists of latitudes and longitudes for the points along routes randomly sampled between start and end points.
#' @param start_long_lat The starting point as a longitude and latitude pair
#' @param end_long_lat The end point as a longitude and latitude pair
#' @param max_travel_spd The maximum traveling speed (be sure to use consitent distance and time units)
#' @param step_time_interval The time interval for each step between start and end point
#' @param total_travel_time The total amount of time it takes to get from start to end point (use consistend time units)
#' @param min_spatial_res The minimum spatial resolution of spatial variables. Passed to circle_overlap_point function
#' @param number_of_routes How many random routes you wish to generate

#' @keywords spatial movement modeling route random
#' @export
#' @examples
#' coord_routes()



coord_routes <- function(start_long_lat, end_long_lat, max_travel_spd, step_time_interval, total_travel_time, min_spatial_res, number_of_routes){

  deltax <- -1*distGeo(c(start_long_lat[1], start_long_lat[2]),c(end_long_lat[1], start_long_lat[2]))
  deltay <- distGeo(c(start_long_lat[1], start_long_lat[2]),c(start_long_lat[1], end_long_lat[2]))
  start.point = cbind(0,0) #will do all calculations in meters and offset from start_long_lat.
  #convert m back to lat/long at the end by destPoint()
  end.point = cbind(deltax,deltay)
  total.steps = total_travel_time/step_time_interval



  #Final df
  coord.lists <- data.frame(matrix(NA,ncol=1,nrow=total.steps))[-1]

  ##################



  ######################################RANDOMLY CHOOSING STEP#####################################
  for (q in 1:number_of_routes){



    #Starting run, setting up empty lists
    final.step.df <- data.frame(x=double(), y=double())
    for (m in 1:total.steps){ #initialize empty list for all steps
      final.step.df[m,] <- c(NaN,NaN)
    }
    final.step.df[total.steps,] <- end.point

    final.coord.df <- data.frame(long=double(), lat=double())
    for (t in 1:total.steps){ #initialize empty list for all coords
      final.coord.df[t,] <- c(NaN,NaN)
    }
    final.coord.df[total.steps,] <- end_long_lat


    needed.step.list = c()
    for (n in 1:(total.steps-1)){ #initialize list of unassigned steps (does not include 0 or last step, which is destination)
      needed.step.list <- append(needed.step.list, n)
    }



    #Function to iterate through all steps and assign them coordinates
    for (k in 1:(total.steps-1)){

      if(length(needed.step.list)>1){#while more than one step needs to be chosen
        stepi = sample(needed.step.list, 1) #choose random element of needed step
        curr.start.pt <- start.point
        curr.end.pt <- end.point
        c1rad <- stepi
        c2rad <- total.steps-stepi
        #see if there is a lesser step that exists. this is start
        if(stepi>1){
          for(h in 1:(stepi-1)){
            if (!is.na(final.step.df[stepi-h,1])){
              curr.start.pt <- final.step.df[stepi-h,]
              c1rad <- h
              break
            }


          }
        }
        #see if there is a greater step that exists. this is end
        if(stepi < (total.steps-1)){
          for(p in 1:(total.steps-stepi-1)){
            if (!is.na(final.step.df[stepi+p,1])){
              curr.end.pt <- final.step.df[stepi+p,]
              c2rad <- p
              break
            }
          }
        }
        circle.overlap.pt <- circle_overlap_point(curr.start.pt, c1rad*max_travel_spd, curr.end.pt, c2rad*max_travel_spd,min_spatial_res)
        steploci =  c(destPoint(c(start_long_lat[1], start_long_lat[2]), 90, circle.overlap.pt[1])[1],destPoint(c(start_long_lat[1], start_long_lat[2]), 0, circle.overlap.pt[2])[2])#define the point for this step
        final.step.df[stepi,] <- circle.overlap.pt #write it into the list
        final.coord.df[stepi,] <- steploci
        needed.step.list <- needed.step.list[needed.step.list != stepi] #remove the step from the list of needed steps


      }else{#last one (has to be different because of the nature of the "sample" function)
        stepi = needed.step.list
        if(stepi>1){
          curr.start.pt <- final.step.df[stepi-1,] #earlier step (unless stepi is first step) don't need loop because all other steps exist
        }else{curr.start.pt <- start.point}
        curr.end.pt <- final.step.df[stepi+1,] #later step, don't need loop because all other steps exist
        c1rad <- 1
        c2rad <- 1
        circle.overlap.pt <- circle_overlap_point(curr.start.pt, c1rad*max_travel_spd, curr.end.pt, c2rad*max_travel_spd,min_spatial_res)
        steploci =  c(destPoint(c(start_long_lat[1], start_long_lat[2]), 90, circle.overlap.pt[1])[1],destPoint(c(start_long_lat[1], start_long_lat[2]), 0, circle.overlap.pt[2])[2])#define the point for this step
        final.step.df[stepi,] <- circle.overlap.pt #write it into the list
        final.coord.df[stepi,] <- steploci
        needed.step.list <- NULL #no more steps needed

      }
    }





    #Add coordinates to final dataframe
    coord.lists[,paste("long",q, sep ="")] <- final.coord.df[,1]
    coord.lists[,paste("lat",q, sep ="")] <- final.coord.df[,2]
  }
  return(coord.lists)
}
