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

set honelako itxurako estruktura bat izango da: 

'''
set <- structure(list(gender = structure(c(1L, 2L, 2L, 1L, 2L, 1L, 1L, 
                          1L, 2L, 1L, 1L, 2L, 1L, 1L, 1L, 2L, 1L, 2L, 2L, 2L, 1L, 1L, 2L, 
                          1L, 1L, 2L), .Label = c("Female", "Male"), class = "factor"), 
                      location = c("Itziar", "Itziar", "Maule", "Urketa", "Urketa", 
                          "Urketa", "Senpere", "Itziar", "Maule", "Uztartze", "Uztartze", 
                          "Urketa", "Urketa", "Urketa", "Senpere", "Itziar", "Itziar", 
                          "Maule", "Maule", "Urketa", "Urketa", "Senpere", "Itziar", 
                          "Itziar", "Maule", "Uztartze"),
                      question = c("q1", "q1", "q1", "q1", "q1", "q1", "q1", "q2", "q2", "q2",
                          "q2", "q2", "q2", "q2", "q2", "q3", "q3", "q3", "q3", "q3", "q3", "q3", 
                          "q4", "q4", "q4", "q4"),
                      answer = c("deitu", "deitatu", "deitu", 
                          "deitatu", "erran", "atxikitu", "deitu", "diru", "sos", "diru", 
                          "sos", "diru", "sos", "dihura", "dihura", "freskatu", "urreztatu", 
                          "freskatu", "urreztatu", "ihintzatu", "urritu", "urritu", 
                          "herots", "hots", "herots", "harrabots")),
                 .Names = c("gender", "location", "question", "answer"),
                 row.names = c(NA, -26L), class = "data.frame")
'''

