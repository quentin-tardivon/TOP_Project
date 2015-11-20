#!/bin/sh
exec scala -classpath "./source/API/imagewrapper_2.9.2-1.0.0.jar" "$0" "$@"
!#

////////////////////////////////////////////////////////////////////////////////

object Main {
  def main(args: Array[String]) : Unit = {
    import com.tncy.top.image.ImageWrapper;

////////////////////////////////////////////////////////////////////////////////

    var fileName : String = "sampleImage2.png";
    var wrappedImage : ImageWrapper = new ImageWrapper(fileName);
    var image2D : Array[Array[Int]] = wrappedImage.getImage();

    var fileName2 = "testimpo.png";
    var wrappedImage2 = new ImageWrapper(fileName2);
    var imagetest : Array[Array[Int]] = wrappedImage2.getImage();


////////////////////////////////////////////////////////////////////////////////

    def powInt(num: Int, n: Int): Int = n match {
      case 0 => return 1
      case _ => num * powInt(num, n-1)
    }

    def test_API() = {


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

    def copy (src: Array[Array[Int]]) : Array[Array[Int]] = {   // faute dans l'argument ??
      var tab = Array.ofDim[Int](src.length, src(0).length)   // matrice rempli de 0
      for (i<-0 to src.length-1) {
        for (j<-0 to src(0).length-1) {
          tab(i)(j) = src(i)(j)
        }
      }
      return tab
    }

    def superImpoStreets(background: Array[Array[Int]], street: Array[Array[Int]]) ={

          for (row <- 0 to background.length-1) {
            for (col <-0 to background(0).length-1) {
              if (street(row)(col)%powInt(16,2) == 0){
                background(row)(col) = 0xFF0000FF
              }
            }
          }
          var outputFile : String = "superpose.jpg";
          wrappedImage.saveImage(outputFile);
        }


    def greyLevel (src: Array[Array[Int]]) = { //Gris = 0.2125 Rouge + 0.7154 Vert + 0.0721 Bleu (en Int)
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
      var outputFile : String = "outputImage2.jpg";
      wrappedImage.saveImage(outputFile);
    }

    def edgeDetection(src: Array[Array[Int]]) = {

    }

    def traceStreets(src: Array[Array[Int]]) = {

    }

    def superImpoStreets(background: Array[Array[Int]], street: Array[Array[Int]]) {

    }


///////////////////////////////////Zone de Test/////////////////////////////////


println(image2D(0)(0));

var clone=copy(image2D)
var outputFile = "outputImagecopie.jpg";
wrappedImage.saveImage(outputFile);

greyLevel(image2D);
superImpoStreets(image2D,imagetest);
//Regarder les API pour savoir comment imprimer clone


////////////////////////////////////////////////////////////////////////////////



  }
}
