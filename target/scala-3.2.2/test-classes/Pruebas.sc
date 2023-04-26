import Huffman . _

///////////////// Pruebas de PoJohn

val stringDePrueba = "qssoueee"
val stringHechoLista = cadenaALista(stringDePrueba)

val arbol = crearArbolDeHuffman(stringHechoLista)

val codigo = codificar(arbol)(stringHechoLista)
val codigo2 = codificarRapido(arbol)(stringHechoLista)

decodificar(arbol, codigo)
decodificar(arbol, codigo2)

////////////////// Fin pruebas de PoJohn

val a1 = Hoja('a', 1)
peso(a1) // Resultado esperado: 1

val a2 = Nodo(Hoja('a', 1), Hoja('b', 2), List('a', 'b'), 3)
peso(a2) // Resultado esperado: 3

val a3 = Nodo(Hoja('a', 1), Hoja('b', 2), List(), 3)
peso(a3) // Resultado esperado: 3

val a4 = Nodo(
  Nodo(
    Hoja('a', 1),
    Hoja('b', 2),
    List('a', 'b'),
    3
  ),
  Hoja('c', 4),
  List('a', 'b', 'c'),
  7)
peso(a4) // Resultado esperado: 7

val a5 = Hoja('a', 0)
peso(a5) // Resultado esperado: 0

cars(a1) // Resultado esperado: List('a')
cars(a2) // Resultado esperado: List('a', 'b')
cars(a3) // Resultado esperado: List()
cars(a4) // Resultado esperado: List('a', 'b', 'c')
cars(a5) // Resultado esperado: List('a')

val o1 = List()
ocurrencias(o1) // Resultado esperado: List()

val o2 = List('F')
ocurrencias(o2) // Resultado esperado: List(('F', 1))

val o3 = List('F', 'F')
ocurrencias(o3) // Resultado esperado: List(('F', 2))

val o4 = List('a', 'b', 'c')
ocurrencias(o4) // Resultado esperado: List(('a', 1), ('b', 1), ('c', 1))

val o5 = List('a', 'b', 'c', 'a', 'b', 'c', 'd', 'c', 'd', 'd', 'd')
ocurrencias(o5) // Resultado esperado: List(('a', 2), ('b', 2), ('c', 3), ('d', 4))

listaDeHojasOrdenadas(ocurrencias(o1)) // Resultado esperado: List():List[Hoja]
listaDeHojasOrdenadas(ocurrencias(o2)) // Resultado esperado: List(Hoja('F', 1))
listaDeHojasOrdenadas(ocurrencias(o3)) // Resultado esperado: List(Hoja('F', 2))
listaDeHojasOrdenadas(ocurrencias(o4)) // Resultado esperado: List(Hoja('a', 1), Hoja('b', 1), Hoja('c', 1))

val o6 = List('a', 'a', 'a', 'a', 'b', 'b', 'b', 'c', 'c', 'd', 'd')
listaDeHojasOrdenadas(ocurrencias(o6)) // Resultado esperado:
                                       // List(Hoja('c',2), Hoja('d',2), Hoja('b',3), Hoja('a',4))

val lu1 = List():List[ArbolH]
listaUnitaria(lu1)  //Resultado esperado: false

val lu2 = List(Hoja('a', 3))
listaUnitaria(lu2)  //Resultado esperado: true

val lu3 = List(Nodo(Hoja('a', 3), Hoja('b', 2), List('a', 'b'), 5), Hoja('c', 1))
listaUnitaria(lu3)  //Resultado esperado: false

val lu4 = List(Nodo(Hoja('a', 3), Hoja('b', 2), List('a', 'b'), 5))
listaUnitaria(lu4)  //Resultado esperado: true

val lu5 = List(Hoja('a', 3), Hoja('b', 2), Hoja('c', 1))
listaUnitaria(lu5)  //Resultado esperado: false

combinar(lu1)  //Resultado esperado: List(): List[ArbolH]
combinar(lu2)  //Resultado esperado: List(Hoja(a,3))
combinar(lu3)  //Resultado esperado: List(Nodo(Nodo(Hoja(a,3),Hoja(b,2),List(a, b),5),Hoja(c,1),List(a, b, c),6))
val c1 = List(Hoja('c', 1), Nodo(Hoja('a', 3), Hoja('b', 2), List('a', 'b'), 5))
combinar(c1)  //Resultado esperado: List(Hoja(c,1), Nodo(Hoja(a,3),Hoja(b,2),List(a, b),5))
combinar(lu5)  //Resultado esperado: List(Nodo(Hoja(a,3),Hoja(b,2),List(a, b),5), Hoja(c,1))

hastaQue(listaUnitaria, combinar)(lu1)  //Resultado esperado: List():List[ArbolH]
hastaQue(listaUnitaria, combinar)(lu2)  //Resultado esperado: List(Hoja(a,3))
hastaQue(listaUnitaria, combinar)(lu5)
hastaQue(listaUnitaria, combinar)(listaDeHojasOrdenadas(ocurrencias(o4)))
hastaQue(listaUnitaria, combinar)(listaDeHojasOrdenadas(ocurrencias(o6)))


val lc1 = cadenaALista("A")
val lc2 = cadenaALista("AS")
val lc3 = cadenaALista("la ola")
val lc4 = cadenaALista("pera")
val lc5 = cadenaALista("aaaa bbb cc d")
crearArbolDeHuffman(lc1) //Resultado esperado: Hoja(A, 1)
crearArbolDeHuffman(lc2)
crearArbolDeHuffman(lc3)
crearArbolDeHuffman(lc4)
crearArbolDeHuffman(lc5)