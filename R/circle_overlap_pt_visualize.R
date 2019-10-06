#' Visualize Circle Overlap Point
#'
#' This function returns a point uniformly sampled in the overlap of two circles and plots a visual aid of the circles the parallelogram sampled points that fail and the final point selected
#' @param circle1_center The center of the first circle as a dataframe with two columns one row
#' @param circle1_radius The radius of the first circle
#' @param circle2_center The center of the second circle as a dataframe with two columns one row
#' @param circle2_radius The radius of the second circle
#' @param min_spat_res The minimum spatial resolution of spatial variables. If sample space has a width less than this value, the middle of the overlap space is chosen as the point.
#' @keywords spatial movement modeling route random
#' @export
#' @examples
#' circle_overlap_point_visualize()


circle_overlap_point_visualize <- function(circle1_center,circle1_radius, circle2_center, circle2_radius, min_spat_res) {


  #Check what they look like
  minx <- min(circle1_center[,1]-circle1_radius, circle2_center[,1] - circle2_radius)
  maxx <- max(circle1_center[,1]+circle1_radius, circle2_center[,1] + circle2_radius)
  miny <- min(circle1_center[,2]-circle1_radius, circle2_center[,2] - circle2_radius)
  maxy <- max(circle1_center[,2]+circle1_radius, circle2_center[,2] + circle2_radius)
  plot(1, type="n", xlab="", ylab="", xlim=c(minx-5, maxx+5), ylim=c(miny-5, maxy+5), asp =1)
  points(circle1_center[1], circle1_center[2])
  draw.circle(circle1_center[,1],circle1_center[,2],circle1_radius)

  points(circle2_center[1], circle2_center[2])
  draw.circle(circle2_center[,1],circle2_center[,2],circle2_radius)

  #Define the center points
  x_1 = circle1_center[,1]
  y_1 = circle1_center[,2]
  x_2 = circle2_center[,1]
  y_2 = circle2_center[,2]

  #Find intersect points
  #Some math bs to determine the intersection points
  #between the two circles
  d = sqrt((x_1-x_2)^2 + (y_1-y_2)^2) #IF the distance between
  #the two centers is exactly equal to the sum of the radii,
  #there is one intersection and that is your point. If the
  #distance is less than or equal to the radius of one circle
  #minus the radius of the other, sample the larger circle. If
  #the distance between the two centers is less than the sum
  #of the radii and greater than the difference, use below:
  circle1_radius+circle2_radius
  circle1_radius-circle2_radius
  d

  l = (circle1_radius^2 - circle2_radius^2 + d^2)/(2*d)
  h = suppressWarnings(sqrt(circle1_radius^2 - l^2))

  #first intersection point
  x_i1 = (l/d)*(x_2-x_1)-(h/d)*(y_2-y_1)+x_1
  y_i1 = (l/d)*(y_2-y_1)+(h/d)*(x_2-x_1)+y_1

  #second
  x_i2 = (l/d)*(x_2-x_1)+(h/d)*(y_2-y_1)+x_1
  y_i2 = (l/d)*(y_2-y_1)-(h/d)*(x_2-x_1)+y_1

  #look at intersections
  points(x_i1,y_i1,col="red")
  points(x_i2,y_i2,col="red")

  #IF NO INTERSECT POINTS
  if (is.na(x_i1) & is.na(x_i2)){ #if both points don't exist
    if (sqrt((y_1-y_2)^2 +(x_1-x_2)^2) < circle1_radius+circle2_radius){ #but if the circle centers are so close they indicate one is inside the other
      if (circle1_radius<circle2_radius){ #if circle 1 is the smaller
        x.ran <- runif(1,min=(x_1 - circle1_radius), max=(x_1 + circle1_radius))#randomly sample inside circle 1 box
        y.ran <- runif(1,min=(y_1 - circle1_radius), max=(y_1 + circle1_radius))
        while (sqrt((x.ran-x_1)^2+(y.ran-y_1)^2) >= circle1_radius){#check that point is actually inside circle 1
          x.ran <- runif(1,min=(x_1 - circle1_radius), max=(x_1 + circle1_radius))#randomly sample inside circle 1 box
          y.ran <- runif(1,min=(y_1 - circle1_radius), max=(y_1 + circle1_radius))
        }
      } else {#circle 1 is not smaller, so either they are same size or circle 2 is smaller
        x.ran <- runif(1,min=(x_2 - circle2_radius), max=(x_2 + circle2_radius))#randomly sample inside circle 2 box
        y.ran <- runif(1,min=(y_2 - circle2_radius), max=(y_2 + circle2_radius))
        while (sqrt((x.ran-x_2)^2+(y.ran-y_2)^2) >= circle2_radius){#check that point is actually inside circle 2
          x.ran <- runif(1,min=(x_2 - circle2_radius), max=(x_2 + circle2_radius))#randomly sample inside circle 2 box
          y.ran <- runif(1,min=(y_2 - circle2_radius), max=(y_2 + circle2_radius))
        }
      }

    } else {
      x.ran = NaN
      y.ran = NaN
      stop('ERROR: No overlap between circles')}
  }else{
    if (x_i1 == x_i2 & y_i1 == y_i2){ #otherwise, if intersect points are identical
      if (sqrt((y_1-y_2)^2 +(x_1-x_2)^2) < circle1_radius+circle2_radius){ #but if the circle centers are so close they indicate one is inside the other
        if (circle1_radius<circle2_radius){ #if circle 1 is the smaller
          x.ran <- runif(1,min=(x_1 - circle1_radius), max=(x_1 + circle1_radius))#randomly sample inside circle 1 box
          y.ran <- runif(1,min=(y_1 - circle1_radius), max=(y_1 + circle1_radius))
          while (sqrt((x.ran-x_1)^2+(y.ran-y_1)^2) >= circle1_radius){#check that point is actually inside circle 1
            x.ran <- runif(1,min=(x_1 - circle1_radius), max=(x_1 + circle1_radius))#randomly sample inside circle 1 box
            y.ran <- runif(1,min=(y_1 - circle1_radius), max=(y_1 + circle1_radius))
          }
        } else {#circle 1 is not smaller, so either they are same size or circle 2 is smaller
          x.ran <- runif(1,min=(x_2 - circle2_radius), max=(x_2 + circle2_radius))#randomly sample inside circle 2 box
          y.ran <- runif(1,min=(y_2 - circle2_radius), max=(y_2 + circle2_radius))
          while (sqrt((x.ran-x_2)^2+(y.ran-y_2)^2) >= circle2_radius){#check that point is actually inside circle 2
            x.ran <- runif(1,min=(x_2 - circle2_radius), max=(x_2 + circle2_radius))#randomly sample inside circle 2 box
            y.ran <- runif(1,min=(y_2 - circle2_radius), max=(y_2 + circle2_radius))
          }
        }


      }else{
        #single intersect point must be only point circles touch
        x.ran <- x_i1
        y.ran <- y_i1
      }
    }


    else { #there must be two unique intersect points


      #Define tangent points on circles for bounding box
      #tangent point for circle 1
      m1 = (y_2-y_1)/(x_2-x_1)
      if (m1 == Inf)
      {
        x_b1_n = x_1
        x_b1_p = x_1
        y_b1_n = (2*y_1 - sqrt((2*y_1)^2 - 4*(y_1^2 - circle1_radius^2)))/2
        y_b1_p = (2*y_1 + sqrt((2*y_1)^2 - 4*(y_1^2 - circle1_radius^2)))/2
      }else{

        b1 = y_2-m1*x_2
        qa1 = m1^2 +1
        qb1 = 2*(b1-y_1)*m1 - 2*x_1
        qc1 = (b1-y_1)^2 + x_1^2 - circle1_radius^2

        x_b1_p = (-qb1 + sqrt(qb1^2 - 4*qa1*qc1))/(2*qa1)
        x_b1_n = (-qb1 - sqrt(qb1^2 - 4*qa1*qc1))/(2*qa1)

        y_b1_p = m1*x_b1_p + b1
        y_b1_n = m1*x_b1_n + b1

      }
      if ( sqrt((y_2 - y_b1_p)^2 + (x_2 - x_b1_p)^2) < sqrt((y_2 - y_b1_n)^2 + (x_2 - x_b1_n)^2)){
        x_b1 = x_b1_p
        y_b1 = y_b1_p
      } else {
        x_b1 = x_b1_n
        y_b1 = y_b1_n
      }
      points(x_b1,y_b1,col="red")


      #tangent point for circle 2
      m2 = (y_2-y_1)/(x_2-x_1)
      if (m1 == Inf)
      {
        x_b2_n = x_1
        x_b2_p = x_1
        y_b2_n = (2*y_2 - sqrt((2*y_2)^2 - 4*(y_2^2 - circle2_radius^2)))/2
        y_b2_p = (2*y_2 + sqrt((2*y_2)^2 - 4*(y_2^2 - circle2_radius^2)))/2
      }else{
        b2 = y_2-m2*x_2
        qa2 = m2^2 +1
        qb2 = 2*(b2-y_2)*m2 - 2*x_2
        qc2 = (b2-y_2)^2 + x_2^2 - circle2_radius^2

        x_b2_p = (-qb2 + sqrt(qb2^2 - 4*qa2*qc2))/(2*qa2)
        x_b2_n = (-qb2 - sqrt(qb2^2 - 4*qa2*qc2))/(2*qa2)

        y_b2_p = m2*x_b2_p + b2
        y_b2_n = m2*x_b2_n + b2


      }
      if ( sqrt((y_1 - y_b2_p)^2 + (x_1 - x_b2_p)^2) < sqrt((y_1 - y_b2_n)^2 + (x_1 - x_b2_n)^2)){
        x_b2 = x_b2_p
        y_b2 = y_b2_p
      } else {
        x_b2 = x_b2_n
        y_b2 = y_b2_n
      }
      points(x_b2,y_b2,col="red")


      #draw boxes overlap (BOX is ABDC so that A and D are opposite corners)
      slopei = (y_b1-y_b2)/(x_b1-x_b2)
      slopeb = (y_i1-y_i2)/(x_i1-x_i2)
      if (slopeb == Inf | slopeb == -Inf){
        #square corners if slope of line between intersects inf
        Ax = x_b2
        Ay = y_i2
        Bx = x_b2
        By = y_i1
        Cx = x_b1
        Cy = y_i2
        Dx = x_b1
        Dy = y_i1

      }else{
        if (slopei == Inf | slopei == -Inf){
          #square corners if slope of line between boundaries inf
          Ax = x_i2
          Ay = y_b2
          Bx = x_i2
          By = y_b1
          Cx = x_i1
          Cy = y_b2
          Dx = x_i1
          Dy = y_b1

        }else{
          b_b2 = y_b2-slopeb*x_b2
          b_b1 = y_b1-slopeb*x_b1
          b_i1 = y_i1-slopei*x_i1
          b_i2 = y_i2-slopei*x_i2

          Ax = (b_b2-b_i2)/(slopei-slopeb)
          Ay = slopei*Ax+b_i2
          Bx = (b_b2-b_i1)/(slopei-slopeb)
          By = slopei*Bx+b_i1
          Cx = (b_b1-b_i2)/(slopei-slopeb)
          Cy = slopei*Cx+b_i2
          Dx = (b_b1-b_i1)/(slopei-slopeb)
          Dy = slopei*Dx+b_i1
        }
      }


      segments(Ax, Ay, x1=c(Bx, Cx), y1=c(By, Cy), col="blue")
      segments(Dx, Dy, x1=c(Bx,Cx), y1=c(By, Cy), col="blue")

      #IF OVERLAP elipse WIDTH IS SMALLER THAN MINIMUM SPATIAL RESOLUTION
      if (sqrt((y_b1-y_b2)^2+(x_b1-x_b2)^2) <= min_spat_res){
        #PT IS CENTER OF BOX
        x.ran <- .5*(Ax-Dx)+Dx
        y.ran <- .5*(Ay-Dy)+Dy

      }else{
        u = runif(1,0,1)
        v = runif(1,0,1)
        x.ran <- Ax + u*(Bx-Ax) + v*(Cx-Ax)
        y.ran <- Ay + u*(By-Ay) + v*(Cy-Ay)


        #Check if in circle 1 and in circle 2
        while (sqrt((x.ran-x_1)^2+(y.ran-y_1)^2) >= circle1_radius | sqrt((x.ran-x_2)^2+(y.ran-y_2)^2) >= circle2_radius){
          points(x.ran, y.ran, col="blue")
          u = runif(1,0,1)
          v = runif(1,0,1)
          x.ran <- Ax + u*(Bx-Ax) + v*(Cx-Ax)
          y.ran <- Ay + u*(By-Ay) + v*(Cy-Ay)
        }
      }
    }
  }
  points(x.ran, y.ran, col="green")
  return(c(x.ran,y.ran))
}
