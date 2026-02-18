* Encoding: UTF-8.
* Fehlende Werte: Demografische Angaben und Gruppierungsvariable 
    
FREQUENCIES VARIABLES=Alter Geschlecht Bildung Muttersprache Gruppe 
  /ORDER=ANALYSIS.

* Fehlende Werte BAI (T1-T3) und RTI (T1-T3)
    
FREQUENCIES VARIABLES=BAI_T1 BAI_T2 BAI_T3 RTI_T1 RTI_T2 RTI_T3 
  /ORDER=ANALYSIS.

* Fälle ausschließen, die keinen Wert in der Gruppierungsvariable haben und keinen Wert sowohl zu BAI T2 als auch zu BAI T3 und Wert im BAI > 63 haben
    
COMPUTE filter_$=(NOT MISSING(Gruppe) AND NOT (MISSING(BAI_T2) AND NOT MISSING(BAI_T3)) AND BAI_T2 
    <= 63). 
VARIABLE LABELS filter_$ 'NOT MISSING(Gruppe) AND NOT (MISSING(BAI_T2) AND NOT MISSING(BAI_T3)) '+ 
    'AND BAI_T2 <= 63 (FILTER)'. 
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'. 
FORMATS filter_$ (f1.0). 
FILTER BY filter_$. 
EXECUTE.

* Demografische Angaben und Gruppe: Mittelwert, Standardabweichug, Spannweite, Min-Max, n 
    
FREQUENCIES VARIABLES=Alter Geschlecht Bildung Muttersprache Gruppe 
  /STATISTICS=STDDEV RANGE MINIMUM MAXIMUM MEAN MEDIAN 
  /ORDER=ANALYSIS.

* Streu- und Lagemaße BAI (T1-T3), RTI (T1-T3) mit jeweiligen n und Prüfung auf Normalverteilung mittels Shapiro Wilk Test
    
EXAMINE VARIABLES=BAI_T1 BAI_T2 BAI_T3 RTI_T1 RTI_T2 RTI_T3 BY Gruppe 
  /PLOT BOXPLOT NPPLOT 
  /COMPARE GROUPS 
  /STATISTICS DESCRIPTIVES 
  /CINTERVAL 95 
  /MISSING LISTWISE 
  /NOTOTAL.

* z-Standardisierung zur Ausreißer Analyse
    
DESCRIPTIVES VARIABLES=BAI_T1 BAI_T2 BAI_T3 RTI_T1 RTI_T2 RTI_T3 
  /SAVE 
  /STATISTICS=MEAN STDDEV MIN MAX.

SORT CASES BY ZBAI_T1 (D). 
SORT CASES BY ZBAI_T2 (D). 
SORT CASES BY ZBAI_T3 (D). 
SORT CASES BY ZRTI_T1 (D). 
SORT CASES BY ZRTI_T2 (D). 
SORT CASES BY ZRTI_T3 (D).


* Grafische Darstellung der Angstwerte nach Alter und Geschlecht
    * Diagrammerstellung. 
GGRAPH 
  /GRAPHDATASET NAME="graphdataset" VARIABLES=Alter BAI Geschlecht MISSING=LISTWISE REPORTMISSING=NO 
  /GRAPHSPEC SOURCE=INLINE 
  /FITLINE TOTAL=NO SUBGROUP=NO. 
BEGIN GPL 
  SOURCE: s=userSource(id("graphdataset")) 
  DATA: Alter=col(source(s), name("Alter")) 
  DATA: BAI=col(source(s), name("BAI")) 
  DATA: Geschlecht=col(source(s), name("Geschlecht"), 
notIn("3"), unit.category()) 
  GUIDE: axis(dim(1), label("Alter")) 
  GUIDE: axis(dim(2), label("BAI")) 
  GUIDE: legend(aesthetic(aesthetic.color.interior), label("Geschlecht")) 
  GUIDE: text.title(label("Streudiagramm von BAI  Schritt: Alter  Schritt: Geschlecht")) 
  SCALE: cat(aesthetic(aesthetic.color.interior), include( 
"1", "2")) 
  ELEMENT: point(position(Alter*BAI), color.interior(Geschlecht)) 
END GPL.


* Diagrammerstellung H1: Korrelation IB und BAI zu T1
    
* Diagrammerstellung. 
GGRAPH 
  /GRAPHDATASET NAME="graphdataset" VARIABLES=RTI BAI Zeit MISSING=LISTWISE REPORTMISSING=NO 
  /GRAPHSPEC SOURCE=INLINE 
   TEMPLATE=["C:\PROGRA~1\IBM\SPSSST~1\Looks\APA_Styles.sgt"] 
  /FITLINE TOTAL=NO SUBGROUP=NO. 
BEGIN GPL 
  SOURCE: s=userSource(id("graphdataset")) 
  DATA: RTI=col(source(s), name("RTI")) 
  DATA: BAI=col(source(s), name("BAI")) 
  DATA: Zeit=col(source(s), name("Zeit"), 
notIn("2", "3"), unit.category()) 
  GUIDE: axis(dim(1), label("RTI")) 
  GUIDE: axis(dim(2), label("BAI")) 
  GUIDE: legend(aesthetic(aesthetic.color.interior), label("Messzeitpunkte")) 
  GUIDE: text.title(label("Streudiagramm von BAI  Schritt: RTI  Schritt: Messzeitpunkte")) 
  SCALE: cat(aesthetic(aesthetic.color.interior), include( 
"1")) 
  ELEMENT: point(position(RTI*BAI), color.interior(Zeit)) 
END GPL.

* Pearson Korrelation BAI T1 und RTI T1
    
STATS CORRELATIONS VARIABLES=BAI_T1 RTI_T1 
/OPTIONS CONFLEVEL=95 METHOD=FISHER 
/MISSING EXCLUDE=YES PAIRWISE=YES.


    
* Prüfung auf Normalverteilung der Mittelwertsdifferenzen für die Durchführung des Abhängigen t-Tests
    
COMPUTE filter_$=(Gruppe = 1). 
VARIABLE LABELS filter_$ 'Gruppe = 1 (FILTER)'. 
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'. 
FORMATS filter_$ (f1.0). 
FILTER BY filter_$. 
EXECUTE. 
EXAMINE VARIABLES=Diff_Angst_T3T2 
  /PLOT BOXPLOT HISTOGRAM NPPLOT 
  /COMPARE GROUPS 
  /STATISTICS DESCRIPTIVES 
  /CINTERVAL 95 
  /MISSING LISTWISE 
  /NOTOTAL.

* Abhängiger t-Test
    
COMPUTE filter_$=(Gruppe = 1). 
VARIABLE LABELS filter_$ 'Gruppe = 1 (FILTER)'. 
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'. 
FORMATS filter_$ (f1.0). 
FILTER BY filter_$. 
EXECUTE. 
FREQUENCIES VARIABLES=Gruppe 
  /STATISTICS=RANGE MINIMUM MAXIMUM MODE 
  /ORDER=ANALYSIS.

T-TEST PAIRS=BAI_T3 WITH BAI_T2 (PAIRED) 
  /ES DISPLAY(TRUE) STANDARDIZER(SD) 
  /CRITERIA=CI(.9500) 
  /MISSING=ANALYSIS.



    






