
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
    cad.toList
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

  type Bit = Int

  def decodificar(arbol: ArbolH, bits: List[Bit]): List[Char] = {
    def seleccionBit(nodo: Nodo, bits: List[Bit]): (ArbolH, List[Bit]) = bits match{
      case 0 :: x => (nodo.izq, x)
      case 1 :: x => (nodo.der, x)
      case _ => throw new Error("Argumento no binario.")
    }

    def decodCar(arbol: ArbolH, bits: List[Bit]): (Char, List[Bit]) = arbol match{
      case Nodo(izq, der, cars, peso) => {
        val (nArbol, nBits) = seleccionBit(Nodo(izq, der, cars, peso), bits)
        val (car, fBits) = decodCar(nArbol, nBits)
        (car, fBits)
      }
      case Hoja(car, _) => (car,bits)
    }

    def decodMsj(arbol: ArbolH, bits: List[Bit], cars: List[Char]): List[Char] = bits match{
      case Nil => cars
      case _ => {
        val (car, nBits) = decodCar(arbol, bits)
        decodMsj(arbol, nBits, cars ::: (car :: Nil))
      }
    }

    decodMsj(arbol, bits, Nil)
  }

  def codificar(arbol: ArbolH)(texto: List[Char]): List[Bit] = {

    def codCar(arbol: ArbolH, car: Char, bits: List[Bit]): List[Bit] = arbol match{
      case Hoja(_, _) => bits
      case Nodo(izq, der, _, _) => {
        if (cars(izq).contains(car)) codCar(izq, car, bits ::: (0 :: Nil))
        else codCar(der, car, bits ::: (1 :: Nil))
      }
    }

    def codMsj(arbol: ArbolH, texto: List[Char], bits: List[Bit]): List[Bit] = texto match{
      case Nil => bits
      case x :: xs => codMsj(arbol, xs, bits ::: codCar(arbol, x, Nil))
    }

    codMsj(arbol, texto, Nil)
  }

  type TablaCodigos = List[(Char, List[Bit])]

  def codigoEnBits(tabla: TablaCodigos)(car: Char): List[Bit] = tabla match{
    case Nil => Nil
    case x :: xs => if (x._1 == car) x._2 else codigoEnBits(xs)(car)
  }

  def mezclarTablasDeCodigos(a: TablaCodigos, b: TablaCodigos): TablaCodigos = {
    a ::: b
  }

  def convertir(arbol: ArbolH): TablaCodigos = arbol match{
    case Nodo(izq, der, _, _) => mezclarTablasDeCodigos(convertir(izq).map(x => (x._1, 0 :: x._2)), convertir(der).map(x => (x._1, 1 :: x._2)))
    case Hoja(car, _) => (car, Nil) :: Nil
  }

  def codificarRapido(arbol: ArbolH)(texto: List[Char]): List[Bit] = {
    val tabla = convertir(arbol)
    def codMsj(cod: Char => List[Bit], texto: List[Char], bits: List[Bit]): List[Bit] = texto match{
      case Nil => bits
      case x :: xs => cod(x) ::: codMsj(cod, xs, bits)
    }
    codMsj(codigoEnBits(tabla), texto, Nil)
  }
}
