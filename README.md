# atob
This R package was developed for plotting random routes between two points per Technitis et al. 2015<sup>1</sup> algorithm. This algorithm uniformly samples the overlaps between two circles. This implementation of the algorithm is meant for smaller spatial areas as it is written in 2 dimensional space. It is not meant for trans-continental or global migration routes. Future updates may include this ability.<br>
<br>There are three functions included in the package:<br>
circle_overlap_point() will return a point in x, y coordinates that is randomly sampled in the overlap space between user defined circles. If the circles do not overlap, it will return an error. If the overlap between the circles is less than a user-defined minimum spatial resolution, the algorithm will choose the midpoint of the overlap instead of a random point.<br>  
circle_overlap_point_visualize() will return a point just like circle_overlap_point(), but it will also plot the two circles, the rectangle around their overlap that is used for sampling in the algorithm, and the random point(s) chosen. This is nice for visualizing how the algorithm works.<br>
coord_routes() uses the full algorithm in two-dimensional space to choose all the points along a route between points a and b. It takes the points as longitude latitude pairs and returns a dataframe of longitude latitude pairs. The points along the route are possible locations that the subject occupied at each given time. 


<br><br><br>


<sup>1</sup>Georgios Technitis, Walied Othman, Kamran Safi & Robert Weibel
(2015) From A to B, randomly: a point-to-point random trajectory generator for animal
movement, International Journal of Geographical Information Science, 29:6, 912-934, DOI:
10.1080/13658816.2014.999682
<br><br>
Recommended install with devtools package and "install_github()" command.
