# Diatech Fuzzy Clustering Proiektua

**Andoni Sudupe**  
2025ko martxoaren 17a  
Github kodea: [https://github.com/ansuehu/Dialektoak_praktika](https://github.com/ansuehu/Dialektoak_praktika)

## 1. Proiektuaren deskribapena

Proiektu honen helburua datu multzo bat Fuzzy clustering metodoen bidez analizatzea da, eta bi multzoen arteko desberdintasun esanguratsuenak aztertzea. Aplikazio honek erabiltzaileari aukera ematen dio CSV fitxategiak igotzeko, multzokatze prozesua exekutatzeko eta multzo desberdinak konparatzeko.

## 2. RStudioren instalazioa eta aplikazioa nola ireki

Instalatu R eta RStudio hemendik: [RStudio Desktop - Posit](https://posit.co/download/rstudio-desktop/)

### 2.1. Beharrezko liburutegiak instalatu

Kontsolan (> ikurra agertzen den tokian)
RStudio-ren script editore batean idatzi eta exekutatu ondorengo kodea:

```r
install.packages(c("shiny", "shinyjs", "cluster", "shinycssloaders", "stringdist", "ggplot2", "parallel"))
```

### 2.2. Shiny aplikazioa ireki eta exekutatzeko

Aplikazioa irekitzeko:

- RStudio programan: File > Open File
- app.R fitxategia hautatu

Aplikazioa exekutatzeko:

- Run App botoi berdea sakatu RStudio-ren goiko eskuinean

## 3. Aplikazioaren atalak eta funtzionamendua

### 3.1. Panel nagusia: Datuen inportazioa eta multzoen sortzea

Aplikazioak lau fitxategi inportatzeko aukera ematen du:
- Distantzia matrizearen fitxategia: Herrien arteko distantziak gordetzen dituen CSV bat.
- Galderen fitxategia: Galderak gordetzen dituen CSV fitxategia.
- Erantzunen fitxategia: Galderen erantzunak gordetzen dituen CSV fitxategia.
- Kokalekuen fitxategia: Herrien kokalekuak gordetzen dituen CSV fitxategia.

Ezaugarriak:
- Fitxategi bat ondo inportatzean, mezu bat agertzen da.
- Fitxategia inportatzean errorea badago, mezu bat bistaratzen da.
- Inportatutako datuen laburpen bat Dashboard atalean erakusten da.
- Erabiltzaileak Perform Clustering botoia sakatzen duenean, Fanny clustering algoritmoa exekutatzen da.

Ezaugarriak:
- Fanny funtzioa erabiltzen da multzoak sortzeko, eta Number of Clusters eta Exponential Parameter parametroak erabiltzaileak definitzen ditu. Lehenengoak egin nahi diren multzo kopurua definitzen du. Bigarrenak berriz, fanny algoritmoak multzoak egiterako orduan erabiltzen duen aldagai bat definitzen du. Balioa handiagoa denean, puntu bakoitzaren kidetasuna gehiago lausotzen da (fuzziness handitzen da). Aldiz, balioa txikiagoa bada, puntuaren kidetasun balioak multzo jakin batean zentratuago egoten dira, hau da, fuzziness gutxiago dago eta emaitzak k-means bezalako metodo klasiko baten antz handiagoa hartzen du.

### 3.3. Multzoen panela: Multzoen aukeraketa eta taulen bistarapena

Multzoen aukeraketa egin ondoren, erabiltzaileak multzoen hiriak ikusi ditzake:

Ezaugarriak:
- Erabiltzaileak bi multzo aukeratzen ditu (Cluster Selection-eko dropdown menuen bidez).
- Bi menuetan multzo bera aukeratzen bada, multzo hori gainontzeko multzoekin alderatzen da
- Aukeratutako multzoei dagozkien herriak eskubiko tauletan erakusten dira (Cluster Towns).
- Download Clustering Results botoiarekin Fanny algoritmoak lortutako emaitzak CSV formatuan deskargatu daitezke.
- Erabiltzaileak Compare Clusters botoia sakatzean, aukeratutako bi multzoen arteko konparaketa egiten da.

Ezaugarriak:
- Erabiltzaileak distantzia funtzio desberdinen artean aukeratu dezake (Bilbao distantzia (IRI) edo Levenshtein distantzia).

### 3.4. Konparazio panela: Galdera aukeratzea eta emaitzen bistaratzea

Erabiltzaileak galdera bat hautatzen duenean, bi multzoen erantzunak eta analisi grafikoa erakusten dira:

Ezaugarriak:
- Galdera hauek Relevant Questions taulan bistaratzen dira diferentziazio, egonkortasun eta aldakortasun metriken arabera.
- Sliderrekin erakutsiko diren galdera kopurua kontrolatu daiteke.
- Galdera bat aukeratzean, Cluster 1 eta Cluster 2 tauletan erantzunak multzoka erakusten dira.
- Item Frequency Visualization grafikoan, erantzunen maiztasunak barraka bistaratzen dira, multzo bakoitzarentzat.
- Emaitzen bi deskarga aukera daude: grafikoa Download Plot, grafikoa deskargatzen duena, edo taula Download Table, taula deskargatzen duena.