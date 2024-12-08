main.R izeneko fitxategi batek kontrolatzen du guztia. Fitxategi honek hainbat parametro ditu:
1. Maptype: 
	1. polygon map with no statistic
	2. polygon map with statistic
	3. centroid map (barycenter map) 
	4. honeycomb map (boundary map)
	5. cluster map
	6. multidimensional scaling
	7. Fuzzy clustering
2. Matabase to be used
3. User
4. Password
5. Path to save distance matrix
6. Path to save json file
7. Path where the neighborhood object is
8. Parts of the project to be analyzed
9. Measure
	1. IPI
	2. IRI
	3. Levenshtein
	4. ...
10. Number of dimensions 


Honekin, Maptype-ren arabera, dei batzuk egingo ditugu. 7.Maptype-an zentratuko gara, hau da, Fuzzy Clustering-ean.

Fuzzy Clustering egiteko lehenik Diastantzia Matrizea kargatzen du espezifikatu den distantziarekin. Hau egiteko loadMeasure izekeo funtzio bat du, honek itzuliko duena (set, measure) moduko lista bat itzuliko digu. 

```
measure <- loadMeasure(MEASURE, DATABASE, USER, PASSWORD, PROJECT.PARTS, returnSet = TRUE)
```

```set``` aldagaiak honelako itxurako estruktura bat izango da: 

```
set <- structure(list(gender = structure(c(1L, 2L, 2L, 1L, 2L, 1L, 1L, 1L, 2L, 1L, 1L, 2L, 1L, 1L, 1L, 2L, 1L, 2L, 2L, 2L, 1L, 1L, 2L, 1L, 1L, 2L), 
.Label = c("Female", "Male"), class = "factor"), 

location = c("Itziar", "Itziar", "Maule", "Urketa", "Urketa", "Urketa", "Senpere", "Itziar", "Maule", "Uztartze", "Uztartze", "Urketa", "Urketa", "Urketa", "Senpere", "Itziar", "Itziar", "Maule", "Maule", "Urketa", "Urketa", "Senpere", "Itziar", "Itziar", "Maule", "Uztartze"),

question = c("q1", "q1", "q1", "q1", "q1", "q1", "q1", "q2", "q2", "q2", "q2", "q2", "q2", "q2", "q2", "q3", "q3", "q3", "q3", "q3", "q3", "q3", "q4", "q4", "q4", "q4"),

answer = c("deitu", "deitatu", "deitu", "deitatu", "erran", "atxikitu", "deitu", "diru", "sos", "diru", "sos", "diru", "sos", "dihura", "dihura", "freskatu", "urreztatu", "freskatu", "urreztatu", "ihintzatu", "urritu", "urritu", "herots", "hots", "herots", "harrabots")),

.Names = c("gender", "location", "question", "answer"),   

row.names = c(NA, -26L), class = "data.frame")
```

```measure```  berriz nxn ko distantzia matrize baten goi triangularra da, non n=herri kopurua den. beraz, main.R fitxategian measure deitzen dion aldagaia (set, measure) motako lista bat da, eta distantzia matrizea lortu nahi ezkero, ```measure$measure``` egin beharko da.  
Hau da 4 herriko distantzia matrize baten goi triangularraren adibide bat:

```
1, 2, 3
   4, 5
      6
```

Honi ordea hainbat atributu gehitu zaizkio:
1. 'diagv': distantzia matrizearen lehen balioa da, hau da, 0 herriak 0 herriarekin duen distantzia.
2. 'Size': n, herri kopurua
3. 'Labels': Herrien izenak
4. 'class': 'diaMeasure'
5. 'idVars': herrien izenak gordetzen dituen data frame bat 

```
similarity <- attr(measure$measure, 'diagv') == 100L
```

Goiko funtzioarekin ```measure$measure```-ren 'diagv' atributua 100 den ikusten du, eta baldin bada, bukatu egingo da funtzioa, zeren eta ez da distantzi matrize bat antzekotasun matrizea baizik.

```
measure$measure[is.na(measure$measure)] <- 0L
```

Goiko funtzioarekin distantzia matrizeko balioren bat ```NA``` bada, 0 balio esleituko da posizio horretan.

Ondoren ```fanny``` algoritmoari deitzen zaio eta honen balioa ```clusters``` aldagaian gordeko da.

