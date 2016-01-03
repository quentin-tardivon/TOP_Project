#!/bin/sh
exec scala -classpath "./source/API/imagewrapper_2.11-1.0.0.jar" "$0" "$@"
!#

////////////////////////////////////////////////////////////////////////////////

object Main {
  def main(args: Array[String]) : Unit = {
    import com.tncy.top.image.ImageWrapper;
    import scala.util.control.Breaks._;

////////////////////////////////////////////////////////////////////////////////

    var fileName : String = "toAnalyze.jpg";
    var wrappedImage : ImageWrapper = new ImageWrapper(fileName);
    var toAnalyze : Array[Array[Int]] = wrappedImage.getImage();
    var fileName2 : String = "contour.png"
    var wrappedImage2 : ImageWrapper = new ImageWrapper(fileName2)
    var imagetest : Array[Array[Int]] = wrappedImage2.getImage();

    var matA = Array.ofDim[Int](3, 3)
    var matB = Array.ofDim[Int](3, 3)
    matA(0)(0) = -1
    matA(0)(1) = 0
    matA(0)(2) = 1
    matA(1)(0) = -2
    matA(1)(1) = 0
    matA(1)(2) = 2
    matA(2)(0) = -1
    matA(2)(1) = 0
    matA(2)(2) = 1

    matB(0)(0) = -1
    matB(0)(1) = -2
    matB(0)(2) = -1
    matB(1)(0) = 0
    matB(1)(1) = 0
    matB(1)(2) = 0
    matB(2)(0) = 1
    matB(2)(1) = 2
    matB(2)(2) = 1

////////////////////////////////////////////////////////////////////////////////

    def powInt(num: Int, n: Int): Int = n match {
      case 0 => return 1
      case _ => num * powInt(num, n-1)
    }

    def addMatrice(mat1 : Array[Array[Int]],mat2 : Array[Array[Int]] ) : Array[Array[Int]] = {
      var resultat =  Array.ofDim[Int](mat1.length, mat2(0).length)
      for (i<-0 to mat1.length-1) {
        for (j<-0 to mat2(0).length-1) {
            resultat(i)(j) = mat1(i)(j) + mat2(i)(j)
        }
      }
      return resultat
    }

    def sqrtMatrice(mat1 : Array[Array[Int]]) : Array[Array[Int]] = {
      var resultat =  Array.ofDim[Int](mat1.length, mat1(0).length)
      for (i<-0 to mat1.length-1) {
        for (j<-0 to mat1(0).length-1) {
            resultat(i)(j) = math.sqrt(mat1(i)(j)).toInt
        }
      }
      return resultat
    }

    def prod_elparel(mat1 : Array[Array[Int]],mat2 : Array[Array[Int]] ) : Array[Array[Int]] = {
      var resultat =  Array.ofDim[Int](mat1.length, mat1(0).length)
      for (i<-0 to mat1.length-1) {
        for (j<-0 to mat1(0).length-1) {
            resultat(i)(j) = powInt(mat1(i)(j),2)
        }
      }
      return resultat
    }

    def prodMatrice(mat1 : Array[Array[Int]],mat2 : Array[Array[Int]] ) : Array[Array[Int]] = {
      var resultat =  Array.ofDim[Int](mat1.length, mat2(0).length)
      for (i<-0 to mat1.length-1) {
        for (j<-0 to mat2(0).length-1) {
          var somme = 0
          for (k<- 0 to mat2.length-1) {
            somme += mat1(i)(k) * mat2(k)(j)
          }
          resultat(i)(j) = somme
        }
      }
      return resultat
    }

    def tracerLigne (x:Int,y:Int,dir:String,long:Int) : List[Array[Int]]= dir match {
      case "E" => var resultat : List[Array[Int]]= List()
        for (i<- 0 to long-1) {
          var cord = Array.ofDim[Int](2)
          cord(0) = x
          cord(1) = y+i
          resultat = cord::resultat
        }

      return resultat

      case "S" => var resultat : List[Array[Int]]= List()
        for (i<- 0 to long-1) {
          var cord = Array.ofDim[Int](2)
          cord(0) = x+i
          cord(1) = y
          resultat= cord::resultat
        }

      return resultat

      case "W" => var resultat : List[Array[Int]]= List()
        for (i<- 0 to long-1) {
          var cord = Array.ofDim[Int](2)
          cord(0) = x
          cord(1) = y-i
          resultat = cord::resultat
        }

      return resultat

      case "N" =>   var resultat : List[Array[Int]]= List()
        for (i<- 0 to long-1) {
          var cord = Array.ofDim[Int](2)
          cord(0) = x-i
          cord(1) = y
          resultat = cord::resultat
        }

      return resultat

      case "NW" =>var resultat : List[Array[Int]]= List()
        for (i<- 0 to long-1) {
          var cord = Array.ofDim[Int](2)
          cord(0) = x-i
          cord(1) = y-i
          resultat = cord::resultat
        }

      return resultat

      case "NE" => var resultat : List[Array[Int]]= List()
        for (i<- 0 to long-1) {
          var cord = Array.ofDim[Int](2)
          cord(0) = x-i
          cord(1) = y+i
          resultat = cord::resultat
        }

      return resultat

      case "SW" => var resultat : List[Array[Int]]= List()
        for (i<- 0 to long-1) {
          var cord = Array.ofDim[Int](2)
          cord(0) = x-i
          cord(1) = y+i
          resultat = cord::resultat
        }

      return resultat

      case "SE" => var resultat : List[Array[Int]]= List()
        for (i<- 0 to long-1) {
          var cord = Array.ofDim[Int](2)
          cord(0) = x+i
          cord(1) = y+i
          resultat = cord::resultat
        }

      return resultat
    }

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


    def reverse(l : List[Array[Int]],swap:Boolean,signeX:Boolean,signeY:Boolean) : List[Array[Int]] = {
      l match {
      case List() => List()
      case h::t =>
      if(signeX) {
        h(0) = -h(0)
      }
      if(signeY) {
        h(1) = -h(1)
      }
      if(swap) {
        var c = h(0)
        h(0) = h(1)
        h(1) = c
      }

      return reverse(t,swap,signeX,signeY):::List(h)
      }
    }

    def calcul_cout(src:Array[Array[Int]],l:List[Array[Int]]) = {
      var liste = l
      var cout = 0.0

        cout = cout + (calculVariance(src,liste) / liste.length)


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

    def copy (src: Array[Array[Int]]) : Array[Array[Int]] = {                    //Done
      var tab = Array.ofDim[Int](src.length, src(0).length)
      for (i<-0 to src.length-1) {
        for (j<-0 to src(0).length-1) {
          tab(i)(j) = src(i)(j)
        }
      }
      return tab
    }

    def greyLevel (src: Array[Array[Int]]) : Array[Array[Int]] = {                                   //Done
      var pixValue = 0
      var blueValue = 0
      var greenValue = 0
      var redValue = 0
      var greyValue = 0.0
      for (row <- 0 to wrappedImage.height-1) {
        for (col <-0 to wrappedImage.width-1) {
          pixValue = src(row)(col)
          blueValue = pixValue% powInt(16,2)
          greenValue = pixValue% powInt(16,4)
          redValue = pixValue% powInt(16,6)
          greenValue = greenValue / powInt(16,2)
          redValue = redValue / powInt(16,4)
          greyValue = 0.2125*redValue.toFloat+0.7154*greenValue.toFloat+0.0721*blueValue.toFloat //Méthode qualitative
          //greyValue = 0.7154*greenValue.toFloat  //Méthode rapide
          src(row)(col) = 0xFF000000 + greyValue.toInt + greyValue.toInt*powInt(16,2) + greyValue.toInt * powInt(16,4)
        }
      }
      var outputFile : String = "greyImage.png";
      wrappedImage.saveImage(outputFile);
      return src
    }

    def edgeDetection(src: Array[Array[Int]],matA: Array[Array[Int]],matB: Array[Array[Int]]) = {
      var greyTab = greyLevel(src)
      var gX = copy(greyTab)
      var gY = copy(greyTab)

      for (k<-0 to greyTab.length-1) {
        for (l<-0 to greyTab(0).length-1) {
          var somme = 0
          var somme2 = 0
          for (i<- 0 to 2 ) {
            for (j <- 0 to 2) {
              if (k-i < 0 || l-j < 0) {
                somme += 0 * matA(i)(j)
                somme2 += 0* matB(i)(j)
              }
              else {
                somme += greyTab(k-i)(l-j) % powInt(16,2) * matA(i)(j)
                somme2 += greyTab(k-i)(l-j) % powInt(16,2) * matB(i)(j)
              }
            }
          }
          var sommeD = (somme.toDouble) * (255.0/1020.0)  //somme+1020.0 * 255.0/2040.0
          var somme2D = (somme2.toDouble) * (255.0/1020.0)
          gX(k)(l) = sommeD.toInt
          gY(k)(l) = somme2D.toInt
        }
      }
      var gFinal = sqrtMatrice(addMatrice(prod_elparel(gX,gX),prod_elparel(gY,gY)))
      for (i<-0 to src.length-1) {
        for (j<-0 to src(0).length-1) {
          gFinal(i)(j) = (gFinal(i)(j) * (255.0/360.0)).toInt
          gFinal(i)(j) = 0xFF000000 + gFinal(i)(j).toInt + gFinal(i)(j).toInt*powInt(16,2) + gFinal(i)(j).toInt * powInt(16,4)
          src(i)(j) = gFinal(i)(j)
        }
      }
      var sortie : String = "contour.png";
      wrappedImage.saveImage(sortie);
    }

    def traceStreets(src: Array[Array[Int]],matPassage: Array[Array[Int]],prof:Int,nbDir:Int,dirI:Int,rigid:Int,x:Int,y:Int,chemin:List[Array[Int]]) : Array[Array[Int]] = {                                //To do
      var north = tracerLigne(x,y,"N",10):::chemin
      var north_east = tracerLigne(x,y,"NE",10):::chemin
      var north_west = tracerLigne(x,y,"NW",10):::chemin
      var east = tracerLigne(x,y,"E",10):::chemin
      var west = tracerLigne(x,y,"W",10):::chemin
      if (prof==4) {
        var cout_north = calcul_cout(src,north)
        var cout_east = calcul_cout(src,east)
        var cout_north_east = calcul_cout(src,north_east)
        var cout_north_west = calcul_cout(src,north_west)
        var cout_west = calcul_cout(src,west)

        for (i<-0 to north.length-1) {
          matPassage(north.head(0))(north.head(1)) = 0
          matPassage(east.head(0))(east.head(1)) = 0
          matPassage(west.head(0))(west.head(1)) = 0
          matPassage(north_east.head(0))(north_east.head(1)) = 0
          matPassage(north_west.head(0))(north_west.head(1)) = 0
          north = north.tail
          east= east.tail
          west = west.tail
          north_east = north_east.tail
          north_west = north_west.tail
        }
        return matPassage
      }
      else {
        traceStreets(src,matPassage,prof+1,nbDir,dirI,rigid,north.head(0),north.head(1),north)
        traceStreets(src,matPassage,prof+1,nbDir,dirI,rigid,east.head(0),east.head(1),east)
        traceStreets(src,matPassage,prof+1,nbDir,dirI,rigid,west.head(0),west.head(1),west)
        traceStreets(src,matPassage,prof+1,nbDir,dirI,rigid,north_east.head(0),north_east.head(1),north_east)
        traceStreets(src,matPassage,prof+1,nbDir,dirI,rigid,north_west.head(0),north_west.head(1),north_west)
      }

    }

    def rechercheRoute(pixPrec : Array[Int]) = {

    }

    def superImpoStreets(background: Array[Array[Int]], street: Array[Array[Int]]) ={

          for (row <- 0 to background.length-1) {
            for (col <-0 to background(0).length-1) {
              if (street(row)(col)%powInt(16,2) == 0){
                background(row)(col) = 0xFF0000FF
              }
            }
          }
          var outputFile : String = "superpose.png";
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



    def calculVariance(src: Array[Array[Int]],pixTrav : List[Array[Int]]) : Double = {
      var variance = 0.0
      var moyenne = 0.0
      var liste = pixTrav
      for (i<-0 to pixTrav.length-1) {
        moyenne += src(liste.head(0))(liste.head(1))
        liste= liste.tail
      }
      moyenne = moyenne / pixTrav.length
      liste = pixTrav
      for (i<-0 to pixTrav.length-1) {
        variance += (src(liste.head(0))(liste.head(1)) - moyenne) * (src(liste.head(0))(liste.head(1)) - moyenne)
        liste= liste.tail
      }
      variance = variance / pixTrav.length

      return variance
    }


///////////////////////////////////Zone de Test/////////////////////////////////


var fileName3 : String = "whiteImg.png"
var wrappedImage3 : ImageWrapper = new ImageWrapper(fileName3)
var matPassage : Array[Array[Int]] = wrappedImage3.getImage();

traceStreets(imagetest,matPassage,0,5,1,1,623,278,List())
wrappedImage3.saveImage("imageTrace.png")


var filename4 : String = "imageTrace.png"
var wrappedImage4 = new ImageWrapper(filename4)
var streets : Array[Array[Int]] = wrappedImage4.getImage()
superImpoStreets(toAnalyze,streets)



//edgeDetection(toAnalyze,matA,matB)


//Il n'est en fait pas possible d'imprimer un l'image copier, il faut effectuer des sauvegardes aux moments clefs!


////////////////////////////////////////////////////////////////////////////////



  }
}
