#!/bin/sh
exec scala -classpath "./source/API/imagewrapper_2.11-1.0.0.jar" "$0" "$@"
!#

////////////////////////////////////////////////////////////////////////////////

object Main {
  def main(args: Array[String]) : Unit = {
    import com.tncy.top.image.ImageWrapper;

////////////////////////////////////////////////////////////////////////////////

    var fileName : String = "sampleImage2.png";
    var wrappedImage : ImageWrapper = new ImageWrapper(fileName);
    var image2D : Array[Array[Int]] = wrappedImage.getImage();
    var fileName2 : String = "testimpo.png"
    var wrappedImage2 : ImageWrapper = new ImageWrapper(fileName2)
    var imagetest : Array[Array[Int]] = wrappedImage2.getImage();


    var wrappedImageGrey : ImageWrapper = new ImageWrapper("greyImage.png");
    var imageGrey : Array[Array[Int]] = wrappedImageGrey.getImage();

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

    def greyLevel (src: Array[Array[Int]]) = {                                   //Done
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
    }

    def edgeDetection(src: Array[Array[Int]],matA: Array[Array[Int]],matB: Array[Array[Int]]) = {
      var gX = copy(src)
      var gY = copy(src)

      for (k<-0 to src.length-1) {
        for (l<-0 to src(0).length-1) {
          var somme = 0
          var somme2 = 0
          for (i<- 0 to 2 ) {
            for (j <- 0 to 2) {
              if (k-i < 0 || l-j < 0) {
                somme += 0 * matA(i)(j)
                somme2 += 0* matB(i)(j)
              }
              else {
                somme += src(k-i)(l-j) % powInt(16,2) * matA(i)(j)
                somme2 += src(k-i)(l-j) % powInt(16,2) * matB(i)(j)
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
      var sortie : String = "convolution.png";
      wrappedImageGrey.saveImage(sortie);
    }

    def traceStreets(src: Array[Array[Int]]) = {                                //To do

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



    def calculVariance(src: Array[Array[Int]]) = { //sur 8 directions avec 1pixels DE LE MERDE!!!
      var tabVariance = copy(src)

      for (k <- 1 to src.length -2) {
        for (l<- 1 to src(0).length -2) {

          var moyenne = 0
          var variance = 0
            for (i <- k-1 to k+1) {
              for (j<- l-1 to l+1) {
                moyenne += src(i)(j)% powInt(16,2)
              }
            }
            moyenne = moyenne / 9

            for (i <- k-1 to k+1) {
              for (j<- l-1 to l+1) {
                variance += 1/256 * powInt(src(i)(j)% powInt(16,2) - moyenne, 2)
              }
            }
            tabVariance(k)(l) = variance
          }
        }
    }


///////////////////////////////////Zone de Test/////////////////////////////////



greyLevel(image2D);



edgeDetection(imageGrey,matA,matB)


//Il n'est en fait pas possible d'imprimer un l'image copier, il faut effectuer des sauvegardes aux moments clefs!


////////////////////////////////////////////////////////////////////////////////



  }
}
