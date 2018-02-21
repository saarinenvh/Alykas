import scala.swing._
import scala.collection.mutable.Buffer
import java.io._
import scala.util.Try
import scala.swing.event._
import scala.swing.BorderPanel.Position._
import src._

object GUI extends SimpleSwingApplication{
  val kirja = new reseptikirja
  val valitsin = new ComboBox(Array[String]("Valitse...", "Raaka-aineet", "Reseptit", "Toteutettavissa olevat reseptit"))
  var tuoteNimi = new TextField
  var tuoteMaara = new TextField
  var allergeenit = new TextField
  var lista = new ListView(Seq[String]())
  var listaBuffer = Buffer[Resepti]()
  
  
  // Avaa tiedosto ja tallenna se tietokantaan
  def avaa(): Boolean = {
    kirja.jaakaappi.tyhjenna()
    kirja.tyhjennaReseptit()
    
    val chooser = new FileChooser
    if (chooser.showOpenDialog(null)==FileChooser.Result.Approve) {
      val tiedostonLukija = new FileReader(chooser.selectedFile);
      val riviLukija = new BufferedReader( tiedostonLukija )

       try {
          var luettuRivi = riviLukija.readLine()
          while( luettuRivi != null ) {
            val info = luettuRivi.split(';')
    
            info(0).toLowerCase match {
              case "raakaaine" => kirja.lisaaRaakaine(info)
              case "resepti"   => kirja.lisaaResepti(info)
              case _           => 
            }
            luettuRivi = riviLukija.readLine()
          }
       } catch {
         case e:IOException =>
            println( "Lukeminen päättyi virheeseen" )
            return false
       }
       return true
    }
    listaBuffer = kirja.reseptit
    lista.listData = kirja.jaakaappi.palautaTuotteet.toSeq.map( _.tulostaNimi)
    lista.repaint()
    true
  }
  
  // Tallenna tekstitiedostoon
  def tallenna(): Boolean = {
    val chooser = new FileChooser
    if (chooser.showSaveDialog(null)==FileChooser.Result.Approve) {
    val file = chooser.selectedFile
    val bw = new BufferedWriter(new FileWriter(file))
     for (i <- 0 until kirja.jaakaappi.palautaTuotteet.size) {
          bw.write("Raakaaine;"+kirja.jaakaappi.palautaTuotteet(i).tulostaNimi + ";" + kirja.jaakaappi.palautaTuotteet(i).tulostaMaara + ";" + kirja.jaakaappi.palautaTuotteet(i).tulostaMitta + ";" + kirja.jaakaappi.palautaTuotteet(i).tulostaAllergeenit + ";*" + "\n")   
        }
        
        for (i <- 0 until kirja.reseptit.size) {
          bw.write("Resepti;" + kirja.reseptit(i).palautaNimi + ";")
          bw.write("res;")
          for (x <- 0 until kirja.reseptit(i).palautaReseptit.size) {
            if (kirja.reseptit(i).palautaReseptit.size != 0) {
              bw.write(kirja.reseptit(i).palautaReseptit(x)._1.palautaNimi + ";")
              bw.write(kirja.reseptit(i).palautaReseptit(x)._2 + ";")
            }
          }
          bw.write("ra;")
          for ( j <- 0 until kirja.reseptit(i).palautaTuotteet.size) {
            bw.write(kirja.reseptit(i).palautaTuotteet(j)._1.tulostaNimi + ";" + kirja.reseptit(i).palautaTuotteet(j)._2 + ";" + kirja.reseptit(i).palautaTuotteet(j)._1.tulostaMitta + ";")
          }
          
          bw.write("ohje;" + kirja.reseptit(i).palautaKuvaus + ";*")
          bw.write("\n")
        }
        bw.close()
    }
    
    true
  }
  
  // GridPanel raakaaineille
  var raakaaine = new GridPanel(5,2) {
    contents += new Label("Nimi: ")
    contents += tuoteNimi
    contents += new Label("Määrä: ")
    contents += tuoteMaara
    contents += new Label("Allergeenit: ")
    contents += allergeenit
    val muokkaa = new Button("Tallenna muutokset")
    contents += muokkaa
    val uusiTuote = new Button("Lisää uusi tuote")
    contents += uusiTuote
    val tyhjenna = new Button("Tyhjennä")
    contents += tyhjenna
    val poistaTuote = new Button("Poista Raaka-aine")
    contents += poistaTuote
  }
  
  var reseptinNimi = new TextField
  var reseptinTuotteet = new TextArea
  var reseptinKuvaus = new TextArea
  var reseptinReseptit = new TextArea
  
  // GridPanel Resepteille
  var resepti = new GridPanel(6,2) {
    contents += new Label("Nimi: ")
    contents += reseptinNimi
    contents += new Label("Raaka-aineet:")
    contents += reseptinTuotteet
    contents += new Label("Reseptit:")
    contents += reseptinReseptit
    contents += new Label("Kuvaus:")
    contents += reseptinKuvaus
    val muokkaaResepti = new Button("Tallenna muutokset")
    contents += muokkaaResepti
    val uusiResepti = new Button("Lisää uusi Resepti")
    contents += uusiResepti
    val tyhjenna = new Button("Tyhjennä")
    contents += tyhjenna
    val poistaResepti = new Button("Poista Resepti")
    contents += poistaResepti
    
  }
  
  // Aloitusnäkymä
  var info = new GridPanel(1,1) {
    contents += new Label("Tervetuloa käyttämään älykästä reseptikirjaa") {
      this.horizontalAlignment = Alignment.Center
      this.verticalAlignment = Alignment.Center
    }
  }
  
  // Swingin top metodi, yleinen GUI
  def top = new MainFrame {
    title = "Älykäs Reseptikirja"
    contents = ui
      ui.layout(new BorderPanel {
        layout(valitsin) = North
        layout(lista) = Center
      }) = West
      ui.layout(ylaOsa) = North
      ui.layout(info) = Center
    
    menuBar = new MenuBar {
    contents += new Menu("Tiedosto") {
      contents += new MenuItem(Action("Avaa") {
        avaa()
      })
      contents += new MenuItem(Action("Tallenna") {
        tallenna()
      })
      contents += new MenuItem(Action("Ohjeita") {
        Dialog.showMessage(ui, "Näin syötät arvoja! \n Raaka-aineet: \n Määrä: <Luku> <Tyyppi> \n Allergeenit: <Allergeeni1> <Allergeeni2> \n Reseptit: Raaka-aineet: <Nimi> <Määrä> <Tyyppi> \n Reseptit: <Nimi> <Määrä> \n Molemmat erotellaan rivivaihdoin", "Ohjeet")
      })

      contents += new Separator
      contents += new MenuItem(Action("Lopeta") {
        
      })
      
    }
  }
    size = new Dimension(800,800)
    centerOnScreen()
  }
  
  // GUI
  var ui = new BorderPanel
  
  // Hakuosuus
  val ylaOsa = new BorderPanel {
    val hakuButtonit = new GridPanel(2,1) {
      val haku = new Button("Haku")
      val tarkkaHaku = new Button("Tarkempi haku")
      contents += haku
      contents += tarkkaHaku
    }
    
    val hakuKentta = new TextField
    
    layout(new Label("Syötä hakusana   ")) = West
    layout(hakuKentta) = Center
    layout(hakuButtonit) = East
  }
  
    
  def checkIfDouble(maara: Array[String]): (Option[Double], String) = {
    (Try {
      maara(0).toDouble
    }.toOption, maara(1))
    
  }
  
  // Pikahaku nimillä
  def pikaHaku(s: String) = {

    for ( i <- kirja.reseptit ) {
      if ( i.palautaNimi.toLowerCase().contains(s.toLowerCase)) {
        hakuArr += i.palautaNimi
        hakuInfo.hits.listData = hakuArr
        }
      }
    }
  
  // Haku kaikilla kriteereillä
  def tarkkaHaku(nimi: String, tuotteet: String, allergeenit: String) {
    var hakuBuffer = Buffer[Resepti]()
    val tuote = tuotteet.split(" ")
    val allerg = allergeenit.split(" ")
    var allergeeni = Buffer[String]()
    var found = 0
    
    for (i <- kirja.reseptit) {
      for (y <- i.palautaTuotteet) {
        allergeeni += y._1.tulostaAllergeenit
      }
    }
    
    for (k <- kirja.reseptit) {
      if (nimi != "" && !k.palautaNimi.contains(nimi)) {
        hakuBuffer += k
      }
      
      for (j <- 0 until tuote.size) {
        if (k.palautaTuotteet.map(n => n._1.tulostaNimi).contains(tuote(j))) {
            found += 1
        }
        
        if (found == tuote.size) {
          hakuBuffer += k
          }
        }
      
      found = 0
      for (s <- 0 until allerg.size) {
        if (!k.palautaTuotteet.map(n => n._1.tulostaAllergeenit).contains(allerg(s))) {
          if (!k.palautaReseptit.isEmpty) {
            for (r <- k.palautaReseptit) {
            if (k.palautaReseptit.isEmpty || !r._1.palautaTuotteet.map(n => n._1.tulostaAllergeenit).contains(allerg(s))) {
              hakuBuffer += k
            }
          }
          } else {
            hakuBuffer += k
          }
        }
      }
      
      }
    hakuInfo.hits.listData = hakuBuffer.map(n => n.palautaNimi)
  }
      
  var hakuArr = Buffer[String]()

  // GridPaneeli kun ollaan haettu
  var hakuInfo = new GridPanel(2,2) {
    val hits = new ListView(hakuArr)
    val select = new Button("Valitse")
    contents += hits
    contents += select
    }
  
  // Tarkan haun gridi
  var tarkkaHakuInfo = new GridPanel(5,2) {
    val hakuName = new TextField
    val hakuTuotteet = new TextArea
    val hakuAllergeenit = new TextArea
    contents += new Label("Nimi: ")
    contents += hakuName
    contents += new Label("Raaka-aineet:" + '\n' + "Erottele Raaka-aineet välimerkillä")
    contents += hakuTuotteet
    contents += new Label("Allergeenit: " + '\n' + "Erottele Raaka-aineet välimerkillä")
    contents += hakuAllergeenit
    val hae = new Button("Hae")
    val tyhj = new Button("Tyhjenna")
    contents += hae
    contents += tyhj
  }
  
  
  // Kuunnellaan tarvittavia komponentteja
  listenTo(valitsin.selection)
  listenTo(lista.selection)
  listenTo(raakaaine.muokkaa)
  listenTo(raakaaine.uusiTuote)
  listenTo(raakaaine.tyhjenna)
  listenTo(raakaaine.poistaTuote)
  listenTo(resepti.muokkaaResepti)
  listenTo(resepti.tyhjenna)
  listenTo(resepti.uusiResepti)
  listenTo(resepti.poistaResepti)
  listenTo(ylaOsa.hakuButtonit.haku)
  listenTo(ylaOsa.hakuButtonit.tarkkaHaku)
  listenTo(tarkkaHakuInfo.hae)
  listenTo(tarkkaHakuInfo.tyhj)
  listenTo(hakuInfo.hits.selection)
  listenTo(hakuInfo.select)

  
  reactions += {
    
    case f: ButtonClicked => 
      
      // Raakaainetta muokataan, korvataan tuotteen tiedot
      if (f.source == raakaaine.muokkaa) {
        if (kirja.jaakaappi.onkoTuoteKaapissa(tuoteNimi.text)) {
          kirja.jaakaappi.palautaTuotteet(lista.selection.leadIndex).muutaNimi(tuoteNimi.text)
          val maara = tuoteMaara.text.split(" ")
          kirja.jaakaappi.palautaTuotteet(lista.selection.leadIndex).lisaaTuote(maara(0).toDouble)
          kirja.jaakaappi.palautaTuotteet(lista.selection.leadIndex).muutaMitta(maara(1))
          kirja.jaakaappi.palautaTuotteet(lista.selection.leadIndex).lisaaAllergeeni(allergeenit.text)
          lista.listData = (kirja.jaakaappi.palautaTuotteet.toSeq.map( _.tulostaNimi) )
        } else {
          Dialog.showMessage(ui, "Tuotetta ei voi muokata", "Varoitus")
        }

          }
      
      if (f.source == raakaaine.uusiTuote) {
        val check = tuoteMaara.text.split(" ")
        if (check.size == 2) {
          val maara = checkIfDouble(check)
            if (!kirja.jaakaappi.onkoTuoteKaapissa(tuoteNimi.text) && maara._1 != None) {
              
            kirja.uusiRaakaine(tuoteNimi.text, maara._1.get, maara._2, allergeenit.text)
            lista.listData = (kirja.jaakaappi.palautaTuotteet.toSeq.map( _.tulostaNimi) )
            }
        } else {
          Dialog.showMessage(ui, "Tuotteen " + tuoteNimi.text + " lisäys ei onnistunut. Syötä nimi ilman välilyöntejä!", "Varoitus")
        }
      }
      
      
      if (f.source == raakaaine.tyhjenna) {
        tuoteNimi.text = ""
        tuoteMaara.text = ""
        allergeenit.text = ""

      }
      
      if (f.source == raakaaine.poistaTuote) {
        
        if (kirja.jaakaappi.palautaTuotteet.size != 0 && kirja.jaakaappi.onkoTuoteKaapissa(kirja.jaakaappi.palautaTuotteet(lista.selection.leadIndex).tulostaNimi)){
          kirja.jaakaappi.poistaTuote(kirja.jaakaappi.palautaTuotteet(lista.selection.leadIndex))
          lista.listData = (kirja.jaakaappi.palautaTuotteet.toSeq.map( _.tulostaNimi) )
          tuoteNimi.text = ""
          tuoteMaara.text = ""
          allergeenit.text = ""
        }
        
      }
      
      if (f.source == resepti.poistaResepti) {
        if (kirja.poistaResepti(reseptinNimi.text)) {
          lista.listData = (kirja.reseptit.toSeq.map( n => n.palautaNimi))
        } 
      }
      
      if (f.source == resepti.tyhjenna) {
        reseptinNimi.text = ""
        reseptinTuotteet.text = ""
        reseptinReseptit.text = ""
        reseptinKuvaus.text = ""
      }
      
      if (f.source == resepti.muokkaaResepti) {
        if (kirja.reseptit.size > 0 && kirja.onkoResepti(reseptinNimi.text)) {
          kirja.reseptit(lista.selection.leadIndex).muutaNimi(reseptinNimi.text)
          val arr = reseptinTuotteet.text.split('\n')
          
          kirja.reseptit(lista.selection.leadIndex).tyhjennaTuotteet()
          for(i <- 0 until arr.size) {
            val line = arr(i).split(" ")
            val tuote = line(0)
            if ( line.size != 3 ) {
              Dialog.showMessage(ui, "Tuotteen " + tuote + " lisääminen ei onnistunut", "Varoitus")
              } else {
                kirja.reseptit(lista.selection.leadIndex).lisaaTuote(new Raakaaine(tuote,0,line(2)), line(1).toInt, line(2))
              }
          }
          
          if (reseptinReseptit != "")   {
            val reseptit = reseptinReseptit.text.split('\n')
            for (i <- 0 until reseptit.size) {
              val line = reseptit(i).split(" ")
              if (line.size != 2) {
                Dialog.showMessage(ui, "Reseptin lisääminen reseptiin epäonnistui. Syötä reseptit muodossa: 'Nimi Määrä'. Nimi ilman välilyöntejä", "Varoitus")
              } else {
                if (kirja.palautaResepti(line(0)) != None) {
                  kirja.reseptit(lista.selection.leadIndex).lisaaResepti(kirja.palautaResepti(line(0)).get, line(1).toInt)
                  } else {
                    val uusiResepti = new Resepti(line(0))
                    kirja.reseptit(lista.selection.leadIndex).lisaaResepti(uusiResepti, line(1).toInt)
                    kirja.lisaaValmisResepti(uusiResepti)
                  }
                
              }
            }

          }
          
          kirja.reseptit(lista.selection.leadIndex).lisaaKuvaus(reseptinKuvaus.text)
        } else {
          Dialog.showMessage(ui, "Resepti ei ole tietokannassa", "Varoitus")
        }

      }
      
      if (f.source == resepti.uusiResepti) {
        
        val info = reseptinTuotteet.text.split('\n')
        val resepti = new Resepti(reseptinNimi.text)
        
        for ( i <- 0 until info.size) {
          val line = (info(i).split(" "))
          if ( line.size != 3 ) {
            Dialog.showMessage(ui, "Tuotteen " + line(0) + " lisääminen ei onnistunut", "Varoitus")
            } else {
              if (kirja.jaakaappi.onkoTuoteKaapissa(line(0))) {
                val tuote = kirja.jaakaappi.palautaTuote(line(0))
                resepti.lisaaTuote(tuote.get, line(1).toInt, line(2))
              } else {
                val tuote = new Raakaaine(line(0), 0, line(2))
                kirja.jaakaappi.lisääTuote(tuote)
                resepti.lisaaTuote(tuote, line(1).toInt, line(2))
              }
              
            }
        }
        
        if (reseptinReseptit.text != "") {
          
        val reseptit = reseptinReseptit.text.split('\n')
        for ( i <- 0 until reseptit.size) {
          val line =  reseptit(i).split(" ")
          if (line.size != 2) {
            Dialog.showMessage(ui, "Reseptin " + line(0) + " lisääminen ei onnistunut", "Varoitus")
          } else {
              if (kirja.palautaResepti(line(0)) != None) {
                resepti.lisaaResepti(kirja.palautaResepti(line(0)).get, line(1).toInt)
            } else {
              val uusiResepti = new Resepti(line(0))
              resepti.lisaaResepti(uusiResepti, line(1).toInt)
              kirja.lisaaValmisResepti(uusiResepti)
            }
            
          }
              
        }
      }
        resepti.lisaaKuvaus(reseptinKuvaus.text)
        kirja.lisaaValmisResepti(resepti)
        lista.listData = (kirja.reseptit.toSeq.map( n => n.palautaNimi))
      }
      
      if (f.source == ylaOsa.hakuButtonit.haku) {
        hakuArr = Buffer[String]()
        this.pikaHaku(ylaOsa.hakuKentta.text)
        hakuInfo.hits.revalidate()
        ui.layout(hakuInfo) = Center
        hakuInfo.repaint()
        ui.repaint()
        ui.revalidate()
      }
      
      if (f.source == ylaOsa.hakuButtonit.tarkkaHaku) {
        ui.layout(tarkkaHakuInfo) = Center
        ui.repaint()
        ui.revalidate()
      }
      
      if (f.source == tarkkaHakuInfo.hae) {
        tarkkaHaku(this.tarkkaHakuInfo.hakuName.text,this.tarkkaHakuInfo.hakuTuotteet.text, this.tarkkaHakuInfo.hakuAllergeenit.text)
        ui.layout(hakuInfo) = Center
        hakuInfo.repaint()
        ui.repaint()
        ui.revalidate()
      }
      
      if (f.source == hakuInfo.select) {

        
        if (hakuInfo.hits.listData.size != 0 && kirja.onkoResepti(hakuInfo.hits.listData(hakuInfo.hits.selection.leadIndex))) {
          ui.layout(resepti) = Center
          ui.repaint()
          ui.revalidate()
          
          val resept = kirja.palautaResepti(hakuInfo.hits.listData(hakuInfo.hits.selection.leadIndex)).get
          reseptinNimi.text = resept.palautaNimi
          reseptinTuotteet.text = ""

        for ( i <- 0 until resept.palautaTuotteet.size) {
          reseptinTuotteet.text += resept.palautaTuotteet(i)._1.tulostaNimi + " "
          reseptinTuotteet.text += resept.palautaTuotteet(i)._2 + " "
          reseptinTuotteet.text += resept.palautaTuotteet(i)._1.tulostaMitta
          reseptinTuotteet.text += '\n'
        }
        reseptinReseptit.text = ""
        
        if (resept.palautaReseptit.size != 0) {
        for ( i <- 0 until resept.palautaReseptit.size) {
          reseptinReseptit.text = resept.palautaReseptit(i)._1.palautaNimi + " "
          reseptinReseptit.text += resept.palautaReseptit(i)._2.toString
         
        }
          }
        reseptinKuvaus.text = resept.palautaKuvaus
        } else {
          Dialog.showMessage(ui, "Haulla ei yhtään tulosta", "Varoitus")
        }
        
      }
      
      if (f.source == tarkkaHakuInfo.tyhj) {
        tarkkaHakuInfo.hakuName.text = ""
        tarkkaHakuInfo.hakuTuotteet.text = ""
        tarkkaHakuInfo.hakuAllergeenit.text = ""
      }
      
    case e:SelectionChanged => 
      if (valitsin.selection.index == 1 && e.source == valitsin) {
        lista.listData = (kirja.jaakaappi.palautaTuotteet.toSeq.map( _.tulostaNimi) )
        ui.layout(raakaaine) = Center
        ui.repaint()
      } else if (valitsin.selection.index == 1 && e.source == lista && tuoteNimi != "" && !kirja.jaakaappi.palautaTuotteet.isEmpty) {
        
        if (lista.selection.leadIndex < lista.listData.size) {
          tuoteNimi.text = kirja.jaakaappi.palautaTuotteet(lista.selection.leadIndex).tulostaNimi
          tuoteMaara.text = kirja.jaakaappi.palautaTuotteet(lista.selection.leadIndex).tulostaMaara.toString() + " "
          tuoteMaara.text += kirja.jaakaappi.palautaTuotteet(lista.selection.leadIndex).tulostaMitta
          allergeenit.text = kirja.jaakaappi.palautaTuotteet(lista.selection.leadIndex).tulostaAllergeenit
        }

        
      } else if (valitsin.selection.index == 2 && e.source == valitsin) {
        lista.listData = kirja.reseptit.toSeq.map( n => n.palautaNimi)
        listaBuffer = kirja.reseptit
        ui.layout(resepti) = Center
        ui.repaint()
      } else if (valitsin.selection.index == 3 && e.source == valitsin) {
        
        var toteutuvat = Buffer[Resepti]()
        
        for (i <- kirja.reseptit) {
          if (i.toteutaResepti()) {
            toteutuvat += i
          }
        }
        ui.layout(resepti) = Center
        
        lista.listData = toteutuvat.map(n => n.palautaNimi)
        listaBuffer = toteutuvat
        ui.repaint()
        ui.revalidate()
      }else if ( valitsin.selection.index == 2 || valitsin.selection.index == 3 && e.source == lista){
        ui.layout(resepti) = Center
        ui.repaint()
        lista.revalidate()
        if (lista.selection.leadIndex < listaBuffer.size && lista.selection.leadIndex >= 0) {
          
        
        reseptinNimi.text = listaBuffer(lista.selection.leadIndex).palautaNimi
        reseptinTuotteet.text = ""
        for ( i <- 0 until listaBuffer(lista.selection.leadIndex).palautaTuotteet.size) {
          
          reseptinTuotteet.text += listaBuffer(lista.selection.leadIndex).palautaTuotteet(i)._1.tulostaNimi + " "
          reseptinTuotteet.text += listaBuffer(lista.selection.leadIndex).palautaTuotteet(i)._2 + " "
          reseptinTuotteet.text += listaBuffer(lista.selection.leadIndex).palautaTuotteet(i)._1.tulostaMitta
          reseptinTuotteet.text += '\n'
        }
        
        reseptinReseptit.text = ""
        
        if (kirja.reseptit(lista.selection.leadIndex).palautaReseptit.size != 0) {
        for ( i <- 0 until listaBuffer(lista.selection.leadIndex).palautaReseptit.size) {
          reseptinReseptit.text = listaBuffer(lista.selection.leadIndex).palautaReseptit(i)._1.palautaNimi + " "
          reseptinReseptit.text += listaBuffer(lista.selection.leadIndex).palautaReseptit(i)._2.toString
         
        }
          }
        reseptinKuvaus.text = listaBuffer(lista.selection.leadIndex).palautaKuvaus
        }
      } 
      }
}