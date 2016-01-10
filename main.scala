object Main {
  def main(args: Array[String]) : Unit = {
    import com.tncy.top.image.ImageWrapper;


////////////////////////////////////////////////////////////////////////////////


    var matA = Array.ofDim[Int](3, 3)   //Initialisation des matrices de Sobel
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

    var tolerance = 1E11  //Tolerance du passage de variance

////////////////////////////////////////////////////////////////////////////////

def powInt(num: Int, n: Int): Int = n match {                   //Puissance entière
  case 0 => return 1
  case _ => num * powInt(num, n-1)
}

def addMatrice(mat1 : Array[Array[Int]],mat2 : Array[Array[Int]] ) : Array[Array[Int]] = { //Ajout de 2 matrices
  var resultat =  Array.ofDim[Int](mat1.length, mat2(0).length)
  for (i<-0 to mat1.length-1) {
    for (j<-0 to mat2(0).length-1) {
        resultat(i)(j) = mat1(i)(j) + mat2(i)(j)
    }
  }
  return resultat
}

def sqrtMatrice(mat1 : Array[Array[Int]]) : Array[Array[Int]] = { //Racine carrée des éléments d'un matrice
  var resultat =  Array.ofDim[Int](mat1.length, mat1(0).length)
  for (i<-0 to mat1.length-1) {
    for (j<-0 to mat1(0).length-1) {
        resultat(i)(j) = math.sqrt(mat1(i)(j)).toInt
    }
  }
  return resultat
}

def prod_elparel(mat1 : Array[Array[Int]],mat2 : Array[Array[Int]] ) : Array[Array[Int]] = {  //Produit de 2 matrices élément par élément
  var resultat =  Array.ofDim[Int](mat1.length, mat1(0).length)
  for (i<-0 to mat1.length-1) {
    for (j<-0 to mat1(0).length-1) {
        resultat(i)(j) = powInt(mat1(i)(j),2)
    }
  }
  return resultat
}

def prodMatrice(mat1 : Array[Array[Int]],mat2 : Array[Array[Int]] ) : Array[Array[Int]] = { //Produit matriciel
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

def tracerLigne (x:Int,y:Int,dir:String,long:Int) : List[Array[Int]]= dir match { //Trace une ligne dans une direction donnée
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
      cord(0) = x+i
      cord(1) = y-i
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

def reverse(l : List[Array[Int]],swap:Boolean,signeX:Boolean,signeY:Boolean) : List[Array[Int]] = { //Inverse une liste
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

def calcul_cout(src:Array[Array[Int]],l:List[Array[Int]]) : Double= { //Calcul le cout d'un chemin
  var liste = l
  var cout = 0.0

    cout = cout + (calculVariance(src,liste) / liste.length)

    return cout
}

def sort(tab_chemin: Array[List[Array[Int]]],tab_cout:Array[Double]) { //Tri rapide d'une liste et d'un tableau qui lui est associé
  def swap(i: Int, j: Int) {
    val t = tab_cout(i); tab_cout(i) = tab_cout(j); tab_cout(j) = t
    val k = tab_chemin(i); tab_chemin(i) = tab_chemin(j); tab_chemin(j) = k
  }
  def sort1(l: Int, r: Int) {
    val pivot = tab_cout((l + r) / 2)
    var i = l; var j = r
    while (i <= j) {
     while (tab_cout(i) < pivot) i += 1
     while (tab_cout(j) > pivot) j -= 1
    if (i <= j) {
      swap(i, j)
      i += 1
      j -= 1
    }
    }
     if (l < j) sort1(l, j)
     if (j < r) sort1(i, r)
     }
   sort1(0, tab_cout.length - 1)
 }

def superImpoStreets(background: Array[Array[Int]], street: Array[Array[Int]]) ={ //Superpose street à background

      for (row <- 0 to background.length-1) {
        for (col <-0 to background(0).length-1) {
          if (street(row)(col)%powInt(16,2) == 0){
            background(row)(col) = 0xFF0000FF
          }
        }
      }

    }

def calculVariance(src: Array[Array[Int]],pixTrav : List[Array[Int]]) : Double = { //Calcul la variance d'une suite de coordonnées associées à une image
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

def copy (src: Array[Array[Int]]) : Array[Array[Int]] = { //Copie d'une matrice sans effet de bord
  var tab = Array.ofDim[Int](src.length, src(0).length)
  for (i<-0 to src.length-1) {
    for (j<-0 to src(0).length-1) {
      tab(i)(j) = src(i)(j)
    }
  }
  return tab
}

def greyLevel (src: Array[Array[Int]]) : Array[Array[Int]] = { //Passe une image en niveau de gris
  var pixValue = 0
  var blueValue = 0
  var greenValue = 0
  var redValue = 0
  var greyValue = 0.0
  for (row <- 0 to src.length-1) {
    for (col <-0 to src(0).length-1) {
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

  return src
}

def edgeDetection(src: Array[Array[Int]],matA: Array[Array[Int]],matB: Array[Array[Int]]) = { //Applique le filtre de Sobel
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
      var sommeD = (somme.toDouble) * (255.0/1020.0)  //coefficient de normalisation
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

}

def traceSreets(src: Array[Array[Int]],matPassage: Array[Array[Int]],profi:Int,x:Int,y:Int,chemin:List[Array[Int]]) : Unit = { //Détecte les routes
  var tab_chemin : Array[List[Array[Int]]] = Array()
  var tab_cout : Array[Double] = Array()
  trouve_chemin(src,matPassage,1,x,y,List()," ")

  if (profi == 5 || tab_cout.length==0) {
  }


  else {
    var copie_chemin = tab_chemin.clone
    var k = 0
    while (tab_cout(k)<tolerance) {
      for (i<-0 to tab_chemin(k).length-1) {
        matPassage(copie_chemin(k).head(0))(copie_chemin(k).head(1)) = 0
        copie_chemin(k) = copie_chemin(k).tail
      }
      k+=1
    }
    var j = 0
    while (tab_cout(j)<tolerance) {
      traceSreets(src,matPassage,profi+1,tab_chemin(j).head(0),tab_chemin(j).head(1),List())
      j+=1
    }
  }


def trouve_chemin(src: Array[Array[Int]],matPassage: Array[Array[Int]],prof:Int,x:Int,y:Int,chemin:List[Array[Int]],provenance:String) : Unit = {
  var long = 7
  var north = tracerLigne(x,y,"N",long):::chemin
  var north_east = tracerLigne(x,y,"NE",long):::chemin
  var north_west = tracerLigne(x,y,"NW",long):::chemin
  var east = tracerLigne(x,y,"E",long):::chemin
  var west = tracerLigne(x,y,"W",long):::chemin
  if (prof==4) {
    var cout_north = calcul_cout(src,north)
    var cout_east = calcul_cout(src,east)
    var cout_north_east = calcul_cout(src,north_east)
    var cout_north_west = calcul_cout(src,north_west)
    var cout_west = calcul_cout(src,west)

    tab_cout =tab_cout ++ Array(cout_north) ++ Array(cout_east) ++ Array(cout_west) ++ Array(cout_north_west) ++ Array(cout_north_east)
    tab_chemin = tab_chemin ++ Array(north) ++ Array(east) ++ Array(west) ++ Array(north_west) ++ Array(north_east)

    sort(tab_chemin,tab_cout)
  }
  else {
    if(calcul_cout(src,north)<tolerance && provenance != "S") {
      trouve_chemin(src,matPassage,prof+1,north.head(0),north.head(1),north,"N")
    }

    if(calcul_cout(src,east)<tolerance && provenance != "W") {
      trouve_chemin(src,matPassage,prof+1,east.head(0),east.head(1),east,"E")
    }

    if(calcul_cout(src,west)<tolerance && provenance != "E") {
      trouve_chemin(src,matPassage,prof+1,west.head(0),west.head(1),west,"W")
    }

    if(calcul_cout(src,north_east)<tolerance && provenance != "SW") {
      trouve_chemin(src,matPassage,prof+1,north_east.head(0),north_east.head(1),north_east,"NE")
    }

    if(calcul_cout(src,north_west)<tolerance && provenance != "SE") {
      trouve_chemin(src,matPassage,prof+1,north_west.head(0),north_west.head(1),north_west,"NW")
    }

  }

}
}


////////////////////////////////////////////////////////////////////////////////

println("Choisir un numéro d'image de 1 à 8:")
var num = "1"   //scala.io.StdIn.readLine
var filename : String = ""
var filename2 : String = ""
var InitX = 0
var InitY = 0

num match {
  case "1" => filename  = "./src/1.jpg"
              filename2 = "./src/whiteImg.jpg"
              InitX = 623
              InitY = 278

  case "2" => filename  = "./src/2.jpg"
              filename2  = "./src/whiteImg.jpg"
              InitX = 623
              InitY = 55

  case "3" => filename  = "./src/3.jpg"
              filename2  = "./src/whiteImg.jpg"
              InitX = 623
              InitY = 192

  case "4" => filename  = "./src/4.tiff"
              filename2  = "./src/whiteImg.tiff"
              InitX = 695
              InitY = 312

  case "5" => filename  = "./src/5.tiff"
              filename2 = "./src/whiteImg.tiff"
              InitX = 694
              InitY = 445

  case "6" => filename = "./src/6.jpg"
              filename2  = "./src/whiteImg.jpg"
              InitX = 623
              InitY = 478

  case "7" => filename  = "./src/7.jpg"
              filename2 = "./src/whiteImg.jpg"
              InitX = 623
              InitY = 429

  case "8" => filename  = "./src/8.jpg"
              filename2 = "./src/whiteImg.jpg"
              InitX = 623
              InitY = 169
}

var wrappedToAnalyze = new ImageWrapper(filename)
var toAnalyze : Array[Array[Int]] = wrappedToAnalyze.getImage()


greyLevel(toAnalyze)
var outputFile : String = "greyImage.jpg";
wrappedToAnalyze.saveImage(outputFile);
var wrappedGrey = new ImageWrapper("greyImage.jpg")
var grey = wrappedGrey.getImage()

//edgeDetection(toAnalyze,matA,matB)
//var sortie : String = "contour.jpg";
//wrappedToAnalyze.saveImage(sortie);

var wrappedFond = new ImageWrapper(filename2)
var streets : Array[Array[Int]] = wrappedFond.getImage()
//var wrappedContour = new ImageWrapper("contour.jpg")
//var imageContour : Array[Array[Int]] = wrappedContour.getImage()
traceSreets(grey,streets,0,InitX,InitY,List())
outputFile = "streets.jpg"
wrappedFond.saveImage(outputFile)

var wrappedRoad = new ImageWrapper("streets.jpg")
var road : Array[Array[Int]] = wrappedRoad.getImage()

superImpoStreets(toAnalyze,road)
outputFile  = "resultat.png";
wrappedToAnalyze.saveImage(outputFile);

////////////////////////////////////////////////////////////////////////////////





  }
}
