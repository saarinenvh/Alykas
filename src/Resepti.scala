package src
import scala.collection.mutable.Buffer

class Resepti(nimi: String) {
  
  private var name = nimi
  private var tuotteet = Buffer[(Raakaaine, Int)]()
  private var reseptit = Buffer[(Resepti, Int)]()
  private var kuvaus = ""
  
  def palautaTuotteet = tuotteet
  def palautaKuvaus = kuvaus
  def palautaNimi = name
  def palautaReseptit = reseptit
  
  def muutaNimi(s:String) = {
    name = s
  }
  
  def lisaaKuvaus(s: String) = {
    kuvaus = s
  }
  
  def lisaaResepti(resepti: Resepti, maara: Int) = {
    val ohje = (resepti, maara)
    reseptit += ohje
  }
  
  def lisaaTuote(raakaaine: Raakaaine, maara: Int, mitta: String) = {
    val ohje = (raakaaine,maara)
    tuotteet += ohje
  }
  
  def tyhjennaTuotteet() {
    tuotteet = Buffer[(Raakaaine, Int)]()
  }
  
  def tyhjennaReseptit() = {
    reseptit = Buffer[(Resepti, Int)]()
  }
  
  def poistaTuote(raakaaine: Raakaaine, maara: Int) = {
    val ohje = (raakaaine,maara)
    
    if (tuotteet.contains(ohje)){
      tuotteet -= ohje
    } else {
      println("Tuotetta ei ole reseptissä")  
    }
  }
  
  def toteutaResepti(): Boolean = {
    var res = false
    for (i <- 0 until tuotteet.size) {
      if ( tuotteet(i)._1.tulostaMaara >= tuotteet(i)._2) {
        res = true
      }
    }
    res
  }
  
  def tulostaResepti = {
    println("Reseptin nimi: " + this.name)
    for (i <- 0 until tuotteet.size) {
      println("Tuote: " + tuotteet(i)._1.tulostaNimi + ", määrä: " + tuotteet(i)._2)
    }
  }
  
  
  
}