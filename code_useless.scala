//code inutile

def calculOctant(dx : Double, dy : Double) : Int = {

   if (dy >=0) {
     if(dx >= 0) {
       if(dx>=dy) {
         return 0
       }
       else {
         return 1
       }
     }
     else {
       if(dy >= -dx) {
         return 2
       }
       else {
         return 3
       }
     }
   }
   else {
     if(dx <= 0) {
       if(dx <= dy) {
         return 4
       }
       else {
         return 5
       }
     }
     else {
       if(-dy >= dx) {
         return 6
       }
       else {
         return 7
       }
     }
   }
 }

 def tracerSegmentInput(x1:Int,y1:Int,x2:Int,y2:Int) = calculOctant(x2-x1,y2-y1) match {
   case 0 => tracerSegment(x1,y1,x2,y2)
   case 1 => tracerSegment(x1,y1,y2,x2)
   case 2 => tracerSegment(x1,y1,y2,-x2)
   case 3 => tracerSegment(x1,y1,-x2,y2)
   case 4 => tracerSegment(x1,y1,-x2,-y2)
   case 5 => tracerSegment(x1,y1,-y2,-x2)
   case 6 => tracerSegment(x1,y1,-y2,x2)
   case 7 => tracerSegment(x1,y1,x2,-y2)
 }

 def tracerSegment(x1 : Int, y1 : Int, x2: Int, y2: Int) : List[Array[Int]]= { //Selon l'algorithme de Bresenham
   var resultat : List[Array[Int]]= List()
   var y = y1
   var dy :Double= y2 -y1
   var dx :Double= x2 -x1

   var e = 0.0
   var e10 : Double = dy / dx
   var e01 = -1.0

   for(x<-x1 to x2) {
     var cord = Array.ofDim[Int](2)
     cord(0) = x
     cord(1) = y
     resultat = cord :: resultat
     //println(resultat.head(0),resultat.head(1))
     e = e+ e10
     if (e >= 0.5) {
       y += 1
       e = e + e01
     }
   }
   return tracerSegmentOutput(x1,y2,y1,y2,resultat)
 }

 def tracerSegmentOutput(x1:Int,y1:Int,x2:Int,y2:Int,liste:List[Array[Int]]) = calculOctant(x2-x1,y2-y1) match {
   case _ => reverse(liste,false,false,false)
   case 1 => reverse(liste,true,false,false)
   case 2 => reverse(liste,true,false,true)
   case 3 => reverse(liste,false,true,false)
   case 4 => reverse(liste,false,true,true)
   case 5 => reverse(liste,true,true,true)
   case 6 => reverse(liste,true,true,false)
   case 7 => reverse(liste,false,false,true)
 }






 def test_API() = {                                                          //Just a test


   var fileName : String = "sampleImage.jpg";
   var wrappedImage : ImageWrapper = new ImageWrapper(fileName);
   var image2D : Array[Array[Int]] = wrappedImage.getImage();
   println("The image height is: " + wrappedImage.height + " px.");
   println("The image width is: " + wrappedImage.width + " px.");
   for (row <- 0 to 40) {
     for (col <-0 to 80) {
       image2D(row)(col) = 0xFF0000FF; // Set these pixels to RGB blue
     }
   }
   // Destination image file - Note that the image file name must contain the file type extension for the image to be saved correctly.
   var outputFile : String = "outputImage.jpg";
   // Save the result
   wrappedImage.saveImage(outputFile);
 }


 
def conv2D(x: Array[Array[Int]],h: Array[Array[Int]]) : Array[Array[Int]] = { //Ca marche pas, c'est triste
  var kCenterX = h.length / 2;
  var kCenterY = h(0).length / 2;
  var out = Array.ofDim[Int](x.length, x(0).length)

  for(i<-0 to x.length-1)              // rows
  {
      for(j<-0 to x(0).length-1)          // columns
      {
          for(m<-0 to h.length-1)     // kernel rows
          {
              var mm = h.length - 2 - m;      // row index of flipped kernel

              for(n<-0 to h(0).length) // kernel columns
              {
                  var nn = h(0).length - 2 - n;  // column index of flipped kernel

                  // index of input signal, used for checking boundary
                  var ii = i + (m - kCenterY) -1;
                  var jj = j + (n - kCenterX) -1;

                  // ignore input samples which are out of bound
                  if( ii >= 0 && ii < x.length-1 && jj >= 0 && jj < x(0).length-1 ) {
                      out(i)(j) += x(ii)(jj) * h(mm)(nn);
                  }
              }
          }
      }
  }
  return out
}
