#!/bin/sh
exec scala -classpath "./source/API/imagewrapper_2.11-1.0.0.jar" "$0" "$@"
!#


object Main {
  def main(args: Array[String]) : Unit = {




    import com.tncy.top.image.ImageWrapper;

    var fileName : String = "sampleImage.jpg";
    var wrappedImage : ImageWrapper = new ImageWrapper(fileName);
    var image2D : Array[Array[Int]] = wrappedImage.getImage();
    println("The image height is: " + wrappedImage.height + " px.");
    println("The image width is: " + wrappedImage.width + " px.");

  }
}
