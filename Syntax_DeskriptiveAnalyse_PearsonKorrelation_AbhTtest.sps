* Encoding: UTF-8.
* Fehlende Werte: Demografische Angaben und Gruppierungsvariable 
    
FREQUENCIES VARIABLES=Alter Geschlecht Bildung Muttersprache Gruppe 
  /ORDER=ANALYSIS.

* Fehlende Werte BAI (T1-T3) und RTI (T1-T3)
    
FREQUENCIES VARIABLES=BAI_T1 BAI_T2 BAI_T3 RTI_T1 RTI_T2 RTI_T3 
  /ORDER=ANALYSIS.

* Fälle ausschließen, die keinen Wert in der Gruppierungsvariable, keinen Wert  zu BAI T2 und BAI T3, einen Wert im BAI  zu T1-T3 > 63 haben 

COMPUTE filter_$ = 
 (NOT (MISSING(BAI_T2) AND MISSING(BAI_T3)) 
 AND (MISSING(BAI_T1) OR BAI_T1 <= 63)
 AND (MISSING(BAI_T2) OR BAI_T2 <= 63)
 AND (MISSING(BAI_T3) OR BAI_T3 <= 63)).
FILTER BY filter_$.
EXECUTE.

* Demografische Angaben und Gruppe: Mittelwert, Standardabweichug, Spannweite, Min-Max, n 
    
FREQUENCIES VARIABLES=Alter Geschlecht Bildung Muttersprache Gruppe 
  /STATISTICS=STDDEV RANGE MINIMUM MAXIMUM MEAN MEDIAN 
  /ORDER=ANALYSIS.

* Streu- und Lagemaße BAI (T1-T3), RTI (T1-T3) mit jeweiligen n und Prüfung auf Ausreißer mittels Boxplot
    
EXAMINE VARIABLES=BAI_T1 BAI_T2 BAI_T3 RTI_T1 RTI_T2 RTI_T3 BY Gruppe 
  /PLOT BOXPLOT NPPLOT 
  /COMPARE GROUPS 
  /STATISTICS DESCRIPTIVES 
  /CINTERVAL 95 
  /MISSING LISTWISE 
  /NOTOTAL.

* z-Standardisierung zur analytischen Ausreißer Analyse
    
DESCRIPTIVES VARIABLES=BAI_T1 BAI_T2 BAI_T3 RTI_T1 RTI_T2 RTI_T3 
  /SAVE 
  /STATISTICS=MEAN STDDEV MIN MAX.

SORT CASES BY ZBAI_T1 (D). 
SORT CASES BY ZBAI_T2 (D). 
SORT CASES BY ZBAI_T3 (D). 
SORT CASES BY ZRTI_T1 (D). 
SORT CASES BY ZRTI_T2 (D). 
SORT CASES BY ZRTI_T3 (D).

* Diagrammerstellung H1: Korrelation IB und BAI zu T1

* Diagrammerstellung.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=RTI_T1 BAI_T1 MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE
  /FITLINE TOTAL=YES SUBGROUP=NO.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: RTI_T1=col(source(s), name("RTI_T1"))
  DATA: BAI_T1=col(source(s), name("BAI_T1"))
  GUIDE: axis(dim(1), label("RTI T1"))
  GUIDE: axis(dim(2), label("BAI T1"))
  GUIDE: text.title(label("Streudiagramm von BAI T1 Schritt: RTI T1"))
  ELEMENT: point(position(RTI_T1*BAI_T1))
END GPL.


*********** Hypothese 1: Pearson Korrelation BAI T1 und RTI T1 ***************
    
STATS CORRELATIONS VARIABLES=BAI_T1 RTI_T1 
/OPTIONS CONFLEVEL=95 METHOD=FISHER 
/MISSING EXCLUDE=YES PAIRWISE=YES.

* Prüfung auf Normalverteilung der Mittelwertsdifferenzen vor Durchführung des Abhängigen t-Tests
    
COMPUTE Diff_AngstT3T2=BAI_T3 - BAI_T2. 
EXECUTE.
    
COMPUTE filter_$=(Gruppe = 1). 
VARIABLE LABELS filter_$ 'Gruppe = 1 (FILTER)'. 
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'. 
FORMATS filter_$ (f1.0). 
FILTER BY filter_$. 
EXECUTE. 
EXAMINE VARIABLES=Diff_AngstT3T2 
  /PLOT BOXPLOT HISTOGRAM NPPLOT 
  /COMPARE GROUPS 
  /STATISTICS DESCRIPTIVES 
  /CINTERVAL 95 
  /MISSING LISTWISE 
  /NOTOTAL.

****** Hypothese 2: Abhängiger t-Test *************
    
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


    






