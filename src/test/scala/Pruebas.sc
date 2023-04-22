import Huffman . _

val lc = cadenaALista("La vida es dura")
val lc2 = cadenaALista("perro")
val lc3 = cadenaALista("aaaabbbccde")
val lho = listaDeHojasOrdenadas(ocurrencias(lc))
listaUnitaria(lho)
crearArbolDeHuffman(lc)
crearArbolDeHuffman(lc2)
crearArbolDeHuffman(lc3)
