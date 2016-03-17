# Name: Barnabas Forgo
# Email: psybf@nottingham.ac.uk
# Student ID: 4211949

require(FuzzyToolkitUoN);
require(rgl);

fis1 <- function() {
  fis= newFIS("emergency")
  
  fis= addVar(fis,"input", "temperature", c(0, 10));
  
  fis= addMF(fis, "input", 1, gaussMF( "very low", c(0:10), c(1, 0,  1)));
  fis= addMF(fis, "input", 1, gaussMF(      "low", c(0:10), c(1, 2.5, 1)));
  fis= addMF(fis, "input", 1, gaussMF(   "normal", c(0:10), c(1, 5,  1)));
  fis= addMF(fis, "input", 1, gaussMF(     "high", c(0:10), c(1, 7.5, 1)));
  fis= addMF(fis, "input", 1, gaussMF("very high", c(0:10), c(1, 10,  1)));
  
  fis= addMF(fis, "input", 1,  trapMF("undefined", c(0:10), c(0,0,10,10, 1)));
  
  
  
  fis= addVar(fis,"input", "headache", c(0, 10));
  
  fis= addMF(fis, "input", 2, trapMF(   "low", c(0:10), c(0, 0, 1, 3, 1)));
  fis= addMF(fis, "input", 2, trapMF(  "mild", c(0:10), c(1, 4, 6, 9, 1)));
  fis= addMF(fis, "input", 2, trapMF("severe", c(0:10), c(7, 9, 10, 10, 1)));  
  
  fis= addMF(fis, "input", 2,  trapMF("undefined", c(0:10), c(0,0,10,10, 1)));
  
  
  
  fis= addVar(fis, "output", "urgency", 0:100);
  
  fis= addMF(fis, "output", 1, gaussMF(      "low", c(0:100), c(15, 0, 1)));
  fis= addMF(fis, "output", 1, gaussMF(   "medium", c(0:100), c(15, 50, 1)));
  fis= addMF(fis, "output", 1, gaussMF("emergency", c(0:100), c(15, 100, 1)));

  rulelist = rbind(
    c(1,4,3,1,1), # IF temperature is very low THEN urgency is emergency
    c(2,4,2,1,1), # IF temperature is low THEN urgency is medium
    c(3,4,1,1,1), # IF temperature is normal THEN urgency is very low
    c(4,4,2,1,1), # IF temperature is high THEN urgency is medium
    c(5,4,3,1,1) # IF temperature is very high THEN urgency is emergency    
  );
  fis= addRule(fis, rulelist);
  
  return(fis);
}
fis2 <- function() {
  fis= newFIS("emergency")
  
  fis= addVar(fis,"input", "temperature", c(0, 10));
  
  fis= addMF(fis, "input", 1, gaussMF( "very low", c(0:10), c(1, 0,  1)));
  fis= addMF(fis, "input", 1, gaussMF(      "low", c(0:10), c(1, 2.5, 1)));
  fis= addMF(fis, "input", 1, gaussMF(   "normal", c(0:10), c(1, 5,  1)));
  fis= addMF(fis, "input", 1, gaussMF(     "high", c(0:10), c(1, 7.5, 1)));
  fis= addMF(fis, "input", 1, gaussMF("very high", c(0:10), c(1, 10,  1)));
  
  fis= addMF(fis, "input", 1,  trapMF("undefined", c(0:10), c(0,0,10,10, 1)));
  
  
  
  fis= addVar(fis,"input", "headache", c(0, 10));
  
  fis= addMF(fis, "input", 2, trapMF(   "low", c(0:10), c(0, 0, 1, 3, 1)));
  fis= addMF(fis, "input", 2, trapMF(  "mild", c(0:10), c(1, 4, 6, 9, 1)));
  fis= addMF(fis, "input", 2, trapMF("severe", c(0:10), c(7, 9, 10, 10, 1)));  
  
  fis= addMF(fis, "input", 2,  trapMF("undefined", c(0:10), c(0,0,10,10, 1)));
  
  
  
  fis= addVar(fis, "output", "urgency", 0:100);
  
  fis= addMF(fis, "output", 1, gaussMF(      "low", c(0:100), c(15, 0, 1)));
  fis= addMF(fis, "output", 1, gaussMF(   "medium", c(0:100), c(15, 50, 1)));
  fis= addMF(fis, "output", 1, gaussMF("emergency", c(0:100), c(15, 100, 1)));

  rulelist = rbind(
    c(1,4,3,1,1),
    
    c(2,1,1,1,1),
    c(2,2,2,1,1),
    c(2,3,3,1,1),
    
    c(3,1,1,1,1),
    c(3,2,1,1,1),
    c(3,3,2,1,1),
    
    c(4,1,1,1,1),
    c(4,2,2,1,1),
    c(4,3,3,1,1),
    
    c(5,4,3,1,1)
  );
  fis= addRule(fis, rulelist);
  
  return(fis);
}
fis3 <- function() {
  fis= newFIS("emergency")
  
  fis= addVar(fis,"input", "temperature", c(0, 10));
  
  fis= addMF(fis, "input", 1, gaussMF( "very low", c(0:10), c(1, 0,  1)));
  fis= addMF(fis, "input", 1, gaussMF(      "low", c(0:10), c(1, 2.5, 1)));
  fis= addMF(fis, "input", 1, gaussMF(   "normal", c(0:10), c(1, 5,  1)));
  fis= addMF(fis, "input", 1, gaussMF(     "high", c(0:10), c(1, 7.5, 1)));
  fis= addMF(fis, "input", 1, gaussMF("very high", c(0:10), c(1, 10,  1)));
  
  fis= addMF(fis, "input", 1,  trapMF("undefined", c(0:10), c(0,0,10,10, 1)));
  
  
  
  fis= addVar(fis,"input", "headache", c(0, 10));
  
  fis= addMF(fis, "input", 2, gaussMF(   "very low", c(0:10), c(1, 0,  1)));
  fis= addMF(fis, "input", 2, gaussMF(        "low", c(0:10), c(1, 2.5, 1)));
  fis= addMF(fis, "input", 2, gaussMF(     "medium", c(0:10), c(1, 5,  1)));
  fis= addMF(fis, "input", 2, gaussMF(     "severe", c(0:10), c(1, 7.5, 1)));
  fis= addMF(fis, "input", 2, gaussMF("very severe", c(0:10), c(1, 10,  1)));
  
  fis= addMF(fis, "input", 2,  trapMF("undefined", c(0:10), c(0,0,10,10, 1)));
  
  
  
  fis= addVar(fis, "output", "urgency", 0:100);
  
  fis= addMF(fis, "output", 1, gaussMF(      "low", c(0:100), c(15, 0, 1)));
  fis= addMF(fis, "output", 1, gaussMF(   "medium", c(0:100), c(15, 50, 1)));
  fis= addMF(fis, "output", 1, gaussMF("emergency", c(0:100), c(15, 100, 1)));

  rulelist = rbind(
    c(1,6,3,1,1),
    
    c(2,1,1,1,1),
    c(2,2,1,1,1),
    c(2,3,2,1,1),
    c(2,4,2,1,1),
    c(2,5,3,1,1),
    
    c(3,1,1,1,1),
    c(3,2,1,1,1),
    c(3,3,1,1,1),
    c(3,4,2,1,1),
    c(3,5,2,1,1),
    
    c(4,1,1,1,1),
    c(4,2,1,1,1),
    c(4,3,2,1,1),
    c(4,4,2,1,1),
    c(4,5,3,1,1),
    
    c(5,6,3,1,1)
  );
  fis= addRule(fis, rulelist);
  
  return(fis);
}
fis4 <- function() {
  fis= newFIS("emergency")
  
  fis= addVar(fis,"input", "temperature", c(0, 10));
  
  fis= addMF(fis, "input", 1, gaussMF( "very low", c(0:10), c(1, 0,  1)));
  fis= addMF(fis, "input", 1, gaussMF(      "low", c(0:10), c(1, 2.5, 1)));
  fis= addMF(fis, "input", 1, gaussMF(   "normal", c(0:10), c(1, 5,  1)));
  fis= addMF(fis, "input", 1, gaussMF(     "high", c(0:10), c(1, 7.5, 1)));
  fis= addMF(fis, "input", 1, gaussMF("very high", c(0:10), c(1, 10,  1)));
  
  fis= addMF(fis, "input", 1,  trapMF("undefined", c(0:10), c(0,0,10,10, 1)));
  
  
  
  fis= addVar(fis,"input", "headache", c(0, 10));
  
  fis= addMF(fis, "input", 2, gaussMF(   "very low", c(0:10), c(1, 0,  1)));
  fis= addMF(fis, "input", 2, gaussMF(        "low", c(0:10), c(1, 2.5, 1)));
  fis= addMF(fis, "input", 2, gaussMF(     "medium", c(0:10), c(1, 5,  1)));
  fis= addMF(fis, "input", 2, gaussMF(     "severe", c(0:10), c(1, 7.5, 1)));
  fis= addMF(fis, "input", 2, gaussMF("very severe", c(0:10), c(1, 10,  1)));
  
  fis= addMF(fis, "input", 2,  trapMF("undefined", c(0:10), c(0,0,10,10, 1)));
  
  
  
  fis= addVar(fis, "output", "urgency", 0:100);
  
  fis= addMF(fis, "output", 1, gaussMF( "very low", c(0:100), c(10, 0, 1)));
  fis= addMF(fis, "output", 1, gaussMF(      "low", c(0:100), c(10, 25, 1)));
  fis= addMF(fis, "output", 1, gaussMF(   "medium", c(0:100), c(10, 50, 1)));
  fis= addMF(fis, "output", 1, gaussMF(     "high", c(0:100), c(10, 75, 1)));
  fis= addMF(fis, "output", 1, gaussMF("emergency", c(0:100), c(10, 100, 1)));

  rulelist = rbind(
    c(1,6,5,1,1),
    
    c(2,1,1,1,1),
    c(2,2,1,1,1),
    c(2,3,2,1,1),
    c(2,4,3,1,1),
    c(2,5,4,1,1),
    
    c(3,1,1,1,1),
    c(3,2,1,1,1),
    c(3,3,1,1,1),
    c(3,4,2,1,1),
    c(3,5,4,1,1),
    
    c(4,1,1,1,1),
    c(4,2,1,1,1),
    c(4,3,2,1,1),
    c(4,4,3,1,1),
    c(4,5,4,1,1),
    
    c(5,6,5,1,1)
  );
  fis= addRule(fis, rulelist);
  
  return(fis);
}
fis5 <- function() {
  fis= newFIS("emergency")
  
  fis= addVar(fis,"input", "temperature", c(0, 10));
  
  fis= addMF(fis, "input", 1, gaussMF( "very low", c(0:10), c(1, -1, 1.5)));
  fis= addMF(fis, "input", 1, gaussMF(      "low", c(0:10), c(1, 2, 1)));
  fis= addMF(fis, "input", 1, gaussMF(   "normal", c(0:10), c(1, 5, 1)));
  fis= addMF(fis, "input", 1, gaussMF(     "high", c(0:10), c(1, 8, 1)));
  fis= addMF(fis, "input", 1, gaussMF("very high", c(0:10), c(1, 11, 1.5)));
  
  fis= addMF(fis, "input", 1,  trapMF("undefined", c(0:10), c(0,0,10,10, 1)));
  
  
  
  fis= addVar(fis,"input", "headache", c(0, 10));
  
  fis= addMF(fis, "input", 2, triMF(   "very low", seq(0, 10, by=0.1), c(0,0,2.5,1)));
  fis= addMF(fis, "input", 2, triMF(        "low", seq(0, 10, by=0.1), c(0,2.5,5,1)));
  fis= addMF(fis, "input", 2, triMF(       "mild", seq(0, 10, by=0.1), c(2.5,5,7.5,1)));
  fis= addMF(fis, "input", 2, triMF(     "severe", seq(0, 10, by=0.1), c(5,7.5,10,1)));
  fis= addMF(fis, "input", 2, triMF("very severe", seq(0, 10, by=0.1), c(7.5,10,10,1)));
  
  fis= addMF(fis, "input", 2,  trapMF("undefined", c(0:10), c(0,0,10,10, 1)));
  
  
  
  fis= addVar(fis, "output", "urgency", 0:100);
  
  fis= addMF(fis, "output", 1, gaussMF( "very low", c(0:100), c(10, -15, 3)));
  fis= addMF(fis, "output", 1, gaussMF(      "low", c(0:100), c(10, 25, 1)));
  fis= addMF(fis, "output", 1, gaussMF(   "medium", c(0:100), c(10, 50, 1)));
  fis= addMF(fis, "output", 1, gaussMF(     "high", c(0:100), c(10, 75, 1)));
  fis= addMF(fis, "output", 1, gaussMF("emergency", c(0:100), c(10, 115, 3)));

  rulelist = rbind(
    c(1,6,5,1,1),
    
    c(2,1,1,1,1),
    c(2,2,1,1,1),
    c(2,3,2,1,1),
    c(2,4,3,1,1),
    c(2,5,4,1,1),
    
    c(3,1,1,1,1),
    c(3,2,1,1,1),
    c(3,3,1,1,1),
    c(3,4,2,1,1),
    c(3,5,4,1,1),
    
    c(4,1,1,1,1),
    c(4,2,1,1,1),
    c(4,3,2,1,1),
    c(4,4,3,1,1),
    c(4,5,4,1,1),
    
    c(5,6,5,1,1)
  );
  fis= addRule(fis, rulelist);
  
  return(fis);
}
fis6 <- function() {
  fis= newFIS("emergency")
  
  fis= addVar(fis,"input", "temperature", c(0, 10));
  
  fis= addMF(fis, "input", 1, gaussMF( "very low", c(0:10), c(1, -1, 1.5)));
  fis= addMF(fis, "input", 1, gaussMF(      "low", c(0:10), c(1, 2, 1)));
  fis= addMF(fis, "input", 1, gaussMF(   "normal", c(0:10), c(1, 5, 1)));
  fis= addMF(fis, "input", 1, gaussMF(     "high", c(0:10), c(1, 8, 1)));
  fis= addMF(fis, "input", 1, gaussMF("very high", c(0:10), c(1, 11, 1.5)));
  
  fis= addMF(fis, "input", 1,  trapMF("undefined", c(0:10), c(0,0,10,10, 1)));
  
  
  
  fis= addVar(fis,"input", "headache", c(0, 10));
  
  fis= addMF(fis, "input", 2, triMF(   "very low", seq(0, 10, by=0.1), c(0,0,2.5,1)));
  fis= addMF(fis, "input", 2, triMF(        "low", seq(0, 10, by=0.1), c(0,2.5,5,1)));
  fis= addMF(fis, "input", 2, triMF(       "mild", seq(0, 10, by=0.1), c(2.5,5,7.5,1)));
  fis= addMF(fis, "input", 2, triMF(     "severe", seq(0, 10, by=0.1), c(5,7.5,10,1)));
  fis= addMF(fis, "input", 2, triMF("very severe", seq(0, 10, by=0.1), c(7.5,10,10,1)));
  
  fis= addMF(fis, "input", 2,  trapMF("undefined", c(0:10), c(0,0,10,10, 1)));
  
  
  
  fis= addVar(fis, "output", "urgency", 0:100);
  
  fis= addMF(fis, "output", 1, gaussMF( "very low", c(0:100), c(10, -15, 3)));
  fis= addMF(fis, "output", 1, gaussMF(      "low", c(0:100), c(10, 25, 1)));
  fis= addMF(fis, "output", 1, gaussMF(   "medium", c(0:100), c(10, 50, 1)));
  fis= addMF(fis, "output", 1, gaussMF(     "high", c(0:100), c(10, 75, 1)));
  fis= addMF(fis, "output", 1, gaussMF("emergency", c(0:100), c(10, 115, 3)));

  rulelist = rbind(
    c(1,6,5,3,1),

    c(2,1,1,1,1),
    c(2,2,2,1,1),
    c(2,3,3,1,1),
    c(2,4,4,1,1),
    c(2,5,4,1,1),

    c(3,1,1,1,1),
    c(3,2,1,1,1),
    c(3,3,2,1,1),
    c(3,4,3,1,1),
    c(3,5,4,1,1),

    c(4,1,1,1,1),
    c(4,2,2,1,1),
    c(4,3,3,1,1),
    c(4,4,4,1,1),
    c(4,5,4,1,1),

    c(5,6,5,3,1) 
  );
  fis= addRule(fis, rulelist);
  
  return(fis);
}

fisFinal <- function() {
  fis= newFIS("emergency", defuzzMethod="bisector", impMethod="prod")
  
  fis= addVar(fis,"input", "temperature", c(0, 10));
  
  fis= addMF(fis, "input", 1, gaussMF( "very low", c(0:10), c(1, -1, 1.5)));
  fis= addMF(fis, "input", 1, gaussMF(      "low", c(0:10), c(1, 2, 1)));
  fis= addMF(fis, "input", 1, gaussMF(   "normal", c(0:10), c(1, 5, 1)));
  fis= addMF(fis, "input", 1, gaussMF(     "high", c(0:10), c(1, 8, 1)));
  fis= addMF(fis, "input", 1, gaussMF("very high", c(0:10), c(1, 11, 1.5)));
  
  fis= addMF(fis, "input", 1,  trapMF("undefined", c(0:10), c(0,0,10,10, 1)));
  
  
  fis= addVar(fis,"input", "headache", c(0, 10));
    
  fis= addMF(fis, "input", 2, triMF(   "very low", seq(0, 10, by=0.1), c(0,0,2.5,1)));
  fis= addMF(fis, "input", 2, triMF(        "low", seq(0, 10, by=0.1), c(0,2.5,5,1)));
  fis= addMF(fis, "input", 2, triMF(       "mild", seq(0, 10, by=0.1), c(2.5,5,7.5,1)));
  fis= addMF(fis, "input", 2, triMF(     "severe", seq(0, 10, by=0.1), c(5,7.5,10,1)));
  fis= addMF(fis, "input", 2, triMF("very severe", seq(0, 10, by=0.1), c(7.5,10,10,1)));
  
  
  fis= addMF(fis, "input", 2,  trapMF("undefined", c(0:10), c(0,0,10,10, 1)));
  
  
  fis= addVar(fis, "output", "urgency", 0:100);
  fis= addMF(fis, "output", 1, gaussMF( "very low", c(0:100), c(10, -15, 3)));
  fis= addMF(fis, "output", 1, gaussMF(      "low", c(0:100), c(10, 25, 1)));
  fis= addMF(fis, "output", 1, gaussMF(   "medium", c(0:100), c(10, 50, 1)));
  fis= addMF(fis, "output", 1, gaussMF(     "high", c(0:100), c(10, 75, 1)));
  fis= addMF(fis, "output", 1, gaussMF("emergency", c(0:100), c(10, 115, 3)));
  
  
  rulelist = rbind(
    c(1,6,5,3,1), # IF temperature is very low THEN urgency is emergency
    
    c(2,1,1,1,1), # IF temperature is low AND headache is very low THEN urgenc#y is very low
    c(2,2,2,1,1), # IF temperature is low AND headache is low THEN urgency is low
    c(2,3,3,1,1), # IF temperature is low AND headache is mild THEN urgency is medium
    c(2,4,4,1,1), # IF temperature is low AND headache is severe THEN urgency is high
    c(2,5,4,1,1), # IF temperature is low AND headache is very severe THEN urgency is high
  
    c(3,1,1,1,1), # IF temperature is normal AND headache is very low THEN urgency is very low
    c(3,2,1,1,1), # IF temperature is normal AND headache is low THEN urgency is very low
    c(3,3,2,1,1), # IF temperature is normal AND headache is mild THEN urgency is low
    c(3,4,3,1,1), # IF temperature is normal AND headache is severe THEN urgency is medium
    c(3,5,4,1,1), # IF temperature is normal AND headache is very severe THEN urgency is high
  
    c(4,1,1,1,1), # IF temperature is high AND headache is very low THEN urgency is very low
    c(4,2,2,1,1), # IF temperature is high AND headache is low THEN urgency is low
    c(4,3,3,1,1), # IF temperature is high AND headache is mild THEN urgency is medium
    c(4,4,4,1,1), # IF temperature is high AND headache is severe THEN urgency is high
    c(4,5,4,1,1), # IF temperature is high AND headache is very severe THEN urgency is high
    
    c(5,6,5,3,1)  # IF temperature is very high THEN urgency is emergency
  );
  fis= addRule(fis, rulelist);
  
  return(fis);
}

showMFS <- function(fis) {
  par(mfrow=c(3,1));
  plotMF(fis,"input",1);
  plotMF(fis,"input",2);
  plotMF(fis,"output",1);
}

intsurf <- function(fis, ix1=1, ix2=2, ox1=1, accuracy=15) {
	require(rgl);
  
	i1 = fis$inputList[[ix1]]
	i2 = fis$inputList[[ix2]]
	o1 = fis$outputList[[ox1]]

	i1b = i1$inputBounds
	i2b = i2$inputBounds

	i1_min = i1b[1]
	i1_max = i1b[length(i1b)]

	i2_min = i2b[1]
	i2_max = i2b[length(i2b)]
  
	x_values = seq(i1_min, i1_max, length = accuracy)
	y_values = seq(i2_min, i2_max, length = accuracy)
	
	m_values = meshgrid(x_values, y_values)

	o_values = evalFIS(cbind(c(m_values$x), c(m_values$y)), fis)


	z_values = matrix(o_values[,ox1], accuracy, accuracy, byrow=TRUE)

  plot3d(i1b, i2b, o1$outputBounds,
    xlab=i1$inputName, ylab=i2$inputName, zlab=o1$outputName,
    type='n'
  );
  
  zlim <- range(o1$outputBounds)
  zlen <- zlim[2] - zlim[1] + 1

  colorlut <- rainbow(zlen, start=2/6, end=0)

  col <- colorlut[ z_values-zlim[1]+1 ]
  
  surface3d(x_values, y_values, z_values, color=col, lit=T, smooth=F, shininess=128)
  # persp3d(x_values, y_values, z_values, color=col, lit=T, smooth=F, shininess=128)
}

projectPng <- function(projectName, dir, ...) {
  # get a list of existing plots for the project
  plots <- list.files(path=dir, pattern=glob2rx(paste0(projectName, "*.png")))
  # pull out numeric component of the plot files
  nums <- as.numeric(gsub(paste(projectName, ".png", sep="|"), "", plots))
  last <- max(nums)
  if (last == -Inf) last <- 0;
  # Create a new file name with an incremented counter.
  newFile <- paste0(projectName, sprintf("%03d", last + 1), ".png")
  # now call png
  png(file.path(dir, newFile), ...)
}

fis= fisFinal();

# projectPng("cw", "C:\\work\\fuz", width=1000, height=1000, pointsize=20)
# intsurf(fis, accuracy=20);
# dev.off()
# intsurf(fis, accuracy=100);
# rgl.snapshot("C:\\work\\fuz\\lastfis.png")
# projectPng("cw", "C:\\work\\fuz", width=800, height=1000, pointsize=20)
# showMFS(fis);
# dev.off()


values= rbind(
    c(5,0),   # 1
    c(3,0),   # 2
    c(7,0),   # 3
    c(5,3),   # 4
    c(5,5),   # 5
    c(5,7),   # 6
    c(5,10),  # 7
    c(0,5),   # 8
    c(0,0),   # 9
    c(10,5),  #10
    c(0,10),  #11
    c(10,0),  #12
    c(10,10)  #13
  )

expected= rbind(
    0,   # 1
    0,   # 2
    0,   # 3
    15,  # 4
    25,  # 5
    50,  # 6
    75,  # 7
    100, # 8
    100, # 9
    100, #10
    100, #11
    100, #12
    100  #13
  )

rmse <- function(actual, expected) {
  return(sqrt(mean((actual - expected)^2)))
}


print(rmse(evalFIS(values, fis1()), expected))
print(rmse(evalFIS(values, fis2()), expected))
print(rmse(evalFIS(values, fis3()), expected))
print(rmse(evalFIS(values, fis4()), expected))
print(rmse(evalFIS(values, fis5()), expected))
print(rmse(evalFIS(values, fis6()), expected))
print(rmse(evalFIS(values, fisFinal()), expected))
