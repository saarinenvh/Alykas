package src
import java.io._
import scala.collection.mutable.Buffer

class reseptikirja {
  
  val jaakaappi = new Kaappi("Jaakaappi")
  val kuivakaappi = new Kaappi("Kuivakaappi")
  val pakastin = new Kaappi("Pakastin")
  var reseptit = Buffer[Resepti]()
  
  def tyhjennaReseptit() = {
    reseptit = Buffer[Resepti]()
  }
  
  def poistaResepti(s: String): Boolean = {
    for ( i <- reseptit ) {
      if ( i.palautaNimi.toLowerCase() == s.toLowerCase() ) {
        reseptit -= i
        return true
      }
    }
    false
  }
  
  def onkoResepti(s: String): Boolean = {
    for (i <- reseptit) {
      if (i.palautaNimi.toLowerCase == s.toLowerCase) {
        return true
      }
    }
    false
  }
  
  
  def lisaaRaakaine(info: Array[String]) = {
    if (info.size == 6) {
      val raaka = new Raakaaine(info(1),info(2).toDouble, info(3))
      this.jaakaappi.lisääTuote( raaka )
      raaka.lisaaAllergeeni(info(4))
    }
    
  }
  
  def lisaaValmisResepti(resepti: Resepti) {
    reseptit += resepti
  }
  
  def lisaaResepti(info: Array[String]) = {
    var i = 3
    var r = new Resepti(info(1))
    
    while (info(i) != "ra" && info(i) != "*")  {
      if (reseptit.map( n => n.palautaNimi.toLowerCase).contains(info(i).toLowerCase())) {
        r.lisaaResepti(this.palautaResepti(info(i)).get, info(i+1).toInt)
      }

      i += 1
    }
    i += 1
    while (info(i) != "*" && info(i) != "ohje") {
      if (jaakaappi.onkoTuoteKaapissa(info(i))) {
        val tuote = jaakaappi.palautaTuote(info(i))
        r.lisaaTuote(tuote.get, info(i + 1).toInt, info(i+2))
      } else {
        val tuote = new Raakaaine(info(i),0, info(i+2))
        jaakaappi.lisääTuote(tuote)
        r.lisaaTuote(tuote, info(i + 1).toInt, info(i+2))
      }
      i += 3
    }
    r.lisaaKuvaus(info(i+1))
    
    reseptit += r
  }
  
  def uusiRaakaine(nimi: String, maara: Double, mitta: String, allergeenit: String) {
    val uusi = new Raakaaine(nimi, maara,mitta)
    this.jaakaappi.lisääTuote(uusi)
    uusi.lisaaAllergeeni(allergeenit)
    
  }
  
  def palautaResepti(s: String): Option[Resepti] = {
    var resepti: Option[Resepti] = None 
    for ( i <- 0 until reseptit.size) {
      if ( reseptit(i).palautaNimi == s ) {
        resepti = Some(reseptit(i))
      }
        
      }
    resepti
  }
  
  
  
  
}