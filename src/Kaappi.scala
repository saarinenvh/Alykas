package src
import scala.collection.mutable.Buffer


class Kaappi(nimi: String) {
  private var tuotteet = Buffer[Raakaaine]()
  
  def tyhjenna() = {
    tuotteet = Buffer[Raakaaine]()
   
  }
  
  def palautaTuotteet = tuotteet
  
  def lisääTuote(tuote: Raakaaine) = {
    tuotteet += tuote
  }
  
  def poistaTuote(tuote: Raakaaine) = {
    tuotteet -= tuote
  }
  
  def etsiTuote(tuote: Raakaaine) = {
    if (tuotteet.contains(tuote)) {
      tuote
    } else {
      println("Tuotetta ei löytynyt!")
    }
  }
  
  def onkoTuoteKaapissa(tuote: String): Boolean = {
    var ret = false
    for (i <- tuotteet) {
      if ( i.tulostaNimi == tuote ) {
        return true
      }
    }
    ret
  }
  
  def palautaTuote(tuote: String): Option[Raakaaine] = {
    val s = None
    for (i <- tuotteet) {
      if ( i.tulostaNimi == tuote ) {
        return Some(i)
      }
    }
    s
  }
  
  def listaaTuotteet = {
    for ( i <- 0 until tuotteet.size) {
      println(tuotteet(i).tulostaNimi)
    }
  }
  
}