package src

import scala.collection.mutable.Buffer

class Raakaaine(nimi: String, maara: Double, tyyppi: String) {
  
  private var name = nimi
  private var maaraA = maara
  private var mitta = tyyppi
  private var allergeenit = ""
  private var merkinta = ""
  
  def tulostaNimi = name
  def tulostaMitta = mitta
  def tulostaMaara = maaraA
  def tulostaAllergeenit = allergeenit
  def tulostaMerkinta = merkinta
  
  def muutaMitta(s: String) = {
    mitta = s
  }
  
  def muutaNimi(s: String) {
    name = s
  }
  
  def lisaaTuote(n: Double) = {
    maaraA = n
  }
  
  def riittaakoTuote(n: Double) = if (maara >= n) true else false
  
  def kaytaTuote(n: Double) = {
    if ( maara < n ) {
      println("Tuotetta liian vähän")
    } else {
      maaraA -= n
    }
  }
  
  def lisaaAllergeeni(a: String) = {
    allergeenit = a
  }
  
  
  def lisaaMerkinta(s: String) = {
    merkinta = s
  }
  
  
}