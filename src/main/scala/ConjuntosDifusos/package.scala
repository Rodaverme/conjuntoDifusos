package object ConjuntosDifusos {

  type ConjDifusos = Int => Double

  def pertenece(elem: Int, s: ConjDifusos): Double = {
    s(elem)
    //TODO HACER LA IMPLEMENTACION DE PERTENECE
  }


  def muchoMayorQue(a: Int, m: Int): ConjDifusos = {
    def mma(x: Int): Double = {
      if (x <= a) 0.0
      else if (x > a && x <= m) (x - a).toDouble / (m - a).toDouble
      else 1.0
    }

    mma
  }

  //TODO COMO DEFINIR QUE UN NUEMERO ENTERO N ES GRANDE LA IDEA ES CALCULAR N/(N+D)
  //TODO D  ES UN NUMERO MAYOR O IGUAL A 1
  //TODO N/(N+D) ELEVADO A LA e

  def grande(d: Int, e: Int): ConjDifusos = {
    //TODO HACER LA IMPLEMENTACION DE GRANDE
    def potencia(base: Double, expo: Int): Double = {
      if (expo == 0) 1.0
      else base * potencia(base, expo - 1)
    }

    def big(n: Int): Double = {
      if (d < 1) 0.0
      else if (e > 1) {
        potencia(n.toDouble / (n + d), e)
      } else 1.0
    }

    big
  }

  // RESTAR MENOS UNO LA FUNCION DE PERTENENCIA
  def complemento(c: ConjDifusos): ConjDifusos = {
    def comp(elem: Int): Double = {
      1 - c(elem)
    }

    comp
  }

  def union(cd1: ConjDifusos, cd2: ConjDifusos): ConjDifusos = {
    //TODO IMPLEMENTAR UNION
    def max(elem1: Int): Double = {
      if ( cd1(elem1) > cd2(elem1)) cd1(elem1)
      else  cd2(elem1)
    }
    max
  }



    def interseccion(cd1: ConjDifusos,cd2: ConjDifusos): ConjDifusos = {

      def min(elem1: Int): Double = {

        if (cd1(elem1) < cd2(elem1)) cd1(elem1)
        else cd2(elem1)
      }
      min
    }

  //  def inclusion (cd1: ConjDifusos, cd2: ConjDifusos):  Boolean ={true}
  //
  //  def igualdad (cd1: ConjDifusos, cd2: ConjDifusos):  Boolean ={true}


}