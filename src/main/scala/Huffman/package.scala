package object Huffman {
  abstract class ArbolH
  case class Nodo (izq: ArbolH, der: ArbolH, cars: List[Char], peso: Int) extends ArbolH
  case class Hoja (car: Char, peso: Int) extends ArbolH

  //Parte 1: Funciones esenciales y sencillas
  def peso(arbol: ArbolH): Int = arbol match {
    case Nodo(izq, der, cars, peso) => peso
    case Hoja(car, peso) => peso
  }

  def cars(arbol: ArbolH): List[Char] = arbol match {
    case Nodo(izq, der, cars, peso) => cars
    case Hoja(car, peso) => car :: Nil
  }

  def hacerNodoArbolH(izq: ArbolH, der: ArbolH):ArbolH = {
    Nodo(izq, der, cars(izq) ::: cars(der), peso(izq) + peso(der)):ArbolH
  }

  // Parte 2: Construyendo árboles de Huffman
  def cadenaALista(cad: String): List [Char] = {
    val cadAux = cad.split("\\s+")    //Ignora los espacios en blanco y devuelve Array de String
    cadAux.flatMap(_.toSeq).toList           //Convierte cada String en Seq[Char],
                                             //aplana las secuencias en una Seq[Char] y la convierte en una List[Char]

  }

  def ocurrencias(cars: List[Char]): List[(Char, Int)] = cars match {
    case Nil => Nil
    case x::xs =>
      val (igualesAx:List[Char], distintos:List[Char]) = xs.partition(_ == x)
      (x, igualesAx.length + 1) :: ocurrencias(distintos)
  }

  def listaDeHojasOrdenadas(frecs: List[(Char, Int)]): List[Hoja] = {
    def merge (l1: List[Hoja], l2: List[Hoja]): List[Hoja] = l1 match {
      case Nil => l2
      case m::ms => l2 match {
        case Nil => l1
        case n::ns => if (m.peso <= n.peso) m::merge(ms,l2) else n::merge(l1,ns)
      }
    }
    val n = frecs.length/2
    if (n == 0) frecs.map(tupla => Hoja(tupla._1,tupla._2)):List[Hoja]
    else {
      val (l1, l2) = frecs splitAt n
      merge(listaDeHojasOrdenadas(l1), listaDeHojasOrdenadas(l2)):List[Hoja]
    }
  }

  def listaUnitaria(arboles: List[ArbolH]): Boolean = arboles match{
    case List(_) => true
    case _ => false
  }

  def combinar(arboles: List[ArbolH]): List[ArbolH] = arboles match{
    case Nil => Nil
    case x :: Nil => List(x)
    case x :: y :: xs => hacerNodoArbolH(x,y) :: xs
  }

  def hastaQue(cond: List[ArbolH] => Boolean, mezclar: List[ArbolH] => List[ArbolH])
              (listaOrdenadaArboles: List[ArbolH]): List[ArbolH] = {
    if (cond(listaOrdenadaArboles) || listaOrdenadaArboles == Nil) listaOrdenadaArboles
    else {
      val listaAux = mezclar(listaOrdenadaArboles)
      hastaQue(cond, mezclar)(listaAux)
    }
  }

  def crearArbolDeHuffman(cars: List[Char]): ArbolH = {
    if(cars.isEmpty) throw new UnsupportedOperationException("Lista de caracteres vacía")
    else
      hastaQue(listaUnitaria,combinar)(listaDeHojasOrdenadas(ocurrencias(cars))).head
  }
}
