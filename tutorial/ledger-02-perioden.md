---
title:     "Ledger: 2. Buchführungsperioden"
author:    "ertes"
date:      "2015-08-06"
lang:      "de"
copyright: "Ertugrul Söylemez"
want-toc:  "yes"
...

Im [letzten Artikel](ledger-01-intro.html) haben wir die Grundlagen der
doppelten Buchführung mit Ledger gelernt.  Jetzt wollen wir ein neues
Buch beginnen, und diesmal wollen wir es so führen, dass es uns
wesentlich mehr Information vermittelt als ein Blick in unseren
Geldbeutel.  Wir werden *Unterkonten* und *Buchungsperioden*
kennenlernen.

[![Creative Commons Attribution-ShareAlike 4.0 International License](https://i.creativecommons.org/l/by-sa/4.0/88x31.png)][cc-by-sa]
This work is licensed under a
[Creative Commons Attribution-ShareAlike 4.0 International License][cc-by-sa].

[cc-by-sa]: http://creativecommons.org/licenses/by-sa/4.0/


Unterkonten
===========

Wie jedes gute Buchführungsprogramm erlaubt Ledger uns, Unterkonten
anzulegen.  Dadurch hat jedes Konto einen *Pfad*, ähnlich wie
Dateipfade, aber mit einem Doppelpunkt (`:`) als Trenner.

Wir machen wieder eine Zeitreise, diesmal zum 1. November 2014, und
beginnen unser Buch mit dem Dateinamen `tutorial2.ledger`:

    2014-11-01 Start
        aktiv:umlauf:bank:1234        200,00 EUR
        aktiv:umlauf:kasse            150,00 EUR
        aktiv:umlauf:port              35,00 EUR
        aktiv:umlauf:spardose           4,30 EUR
        eigen:start:archiv

Und wir werfen auch gleich einen Blick auf unsere Bilanz:

     389,30 EUR  aktiv:umlauf
     200,00 EUR    bank:1234
     150,00 EUR    kasse
      35,00 EUR    port
       4,30 EUR    spardose
    -389,30 EUR  eigen:start:archiv

Erneut haben wir mit zwei Hauptkonten begonnen, aber diesmal sind sie in
Unterkonten aufgeteilt.  Das Aktivkonto enthält ein Unterkonto für unser
Umlaufvermögen, also für das Geld, das wir konkret besitzen.  Da wir
auch wissen wollen, wo das Geld ist, ist auch das Umlaufkonto in
Unterkonten aufgeteilt.  Wir besitzen eine Kasse (`kasse`) mit 150 Euro,
einen Geldbeutel (`port` wie *portabel* oder *Portemonnaie*) mit 35
Euro, eine Spardose und ein Bankkonto.  Da wir mehrere Bankkonten haben
könnten, haben wir auch `bank` in Unterkonten aufgeteilt.

Hier sehen wir auch gleich, dass eine Buchung sich aus mehr als zwei
Posten zusammensetzen kann.  Obwohl das im Privathaushalt eher die
Ausnahme ist, werden wir einige weitere Beispiele davon sehen.


Buchführungsperioden
====================

Anders als im letzten Artikel führen wir unser Buch nicht mehr
fortlaufend, sondern in einzelnen Abschnitten, den
*Buchführungsperioden*.  Eine solche Periode geht dabei für uns einen
Monat lang.  Man kann natürlich auch andere Perioden wählen oder gar die
Perioden variieren, aber es ist sinnvoll, eine zu wählen, die dieselbe
Länge hat wie unsere periodischen Einnahmen und Ausgaben.  Die meisten
von uns beziehen monatlich Lohn und haben monatliche Ausgaben (Miete,
Strom, etc.).  Daher ist ein monatlich geführtes Buch fast immer die
beste Wahl.

Unser Eigenkapital setzt sich aus zwei Teilen zusammen, um diese
Perioden wiederzugeben, nämlich aus dem *Startkapital* und den
*Erfolgskonten*.

Wir starten in den Monat mit einem bestimmten *Startkapital* (`start`).
Dieses setzt sich zusammen aus den monatlichen Gewinnen und Verlusten
der vergangenen Perioden.  Die Perioden, über die wir keine
Aufzeichnungen haben oder die wir nicht mehr in der Kontenübersicht
sehen wollen, befinden sich im Archiv.  Da wir unser Buch erst jetzt
beginnen, ist die gesamte Vergangenheit im Archiv.

Samstags ist Marktplatztag.  Wir wollen auf dem Marktplatz einkaufen
gehen.  Dazu benötigen wir etwas mehr Bargeld.  Um uns den Weg zum
Bankautomaten zu ersparen, nehmen wir es einfach aus der Kasse:

    2014-11-01 Barauszahlung
        aktiv:umlauf:port              50,00 EUR
        aktiv:umlauf:kasse

Eine solche Buchung zwischen aktiven Konten wird als *Aktivtausch*
bezeichnet.  Unser Vermögen ändert sich dadurch nicht.  Wir haben jetzt
85 Euro im Geldbeutel und nur noch 100 Euro in der Kasse.


Erfolgskonten
=============

Der zweite Teil des Eigenkapitals sind die beiden *Erfolgskonten*.  Im
letzten Buch hatten wir unsere Erträge und Aufwendungen in Verbindung
mit dem Eigenkapitalkonto gebucht.  Diesmal wollen wir etwas genauer
sein.  Nun, da wir genug Geld im Geldbeutel haben, gehen wir einkaufen:

    2014-11-01 Marktplatz
        eigen:aufwand:nahrung          40,30 EUR
        eigen:aufwand:pflanzen         12,50 EUR
        aktiv:umlauf:port

Unsere Bilanz nach dem Einkauf:

     336,50 EUR  aktiv:umlauf
     200,00 EUR    bank:1234
     100,00 EUR    kasse
      32,20 EUR    port
       4,30 EUR    spardose
    -336,50 EUR  eigen
      52,80 EUR    aufwand
      40,30 EUR      nahrung
      12,50 EUR      pflanzen
    -389,30 EUR    start:archiv

Die Konten `aufwand` und später `ertrag` werden als *Erfolgskonten*
bezeichnet.  Streng genommen ist der Name `aufwand` falsch (es müsste
`aufwendungen` heißen), aber wir bevorzugen den kürzeren Namen gegenüber
der strikten Sprachgenauigkeit.

Wir haben also in diesem Monat bereits Aufwendungen gehabt.  Wie im
letzten Buch haben wir diese zum Eigenkapital aufgebucht, allerdings
diesmal getrennt vom Startkapital.

Fast hätten wir vergessen, dass die Miete fällig ist.  Diese können wir
noch nicht bezahlen, da wir unseren Lohn noch nicht erhalten haben.
Daher buchen wir sie aus dem Fremdkapital heraus:

    2014-11-01 Miete
        eigen:aufwand:wohnung         600,00 EUR
        fremd:vermieter

Diese Buchung bedarf einer Erklärung.  Fremdkapital ist, wie wir gesehen
haben, gewissermaßen das, was wir uns ausgeliehen haben.  Hier haben wir
uns aber nichts ausgeliehen, oder?  Doch, buchhalterisch betrachtet
schon.  Der Vermieter hat uns 600 Euro geliehen, von denen wir direkt
die Aufwendung getätigt haben.  So direkt, dass das Geld an sich nie bei
uns angekommen ist.  Nun schulden wir dem Vermieter diese 600 Euro.

Glücklicherweise haben wir nicht nur Aufwendungen zu verzeichnen, denn
wir bekommen ja noch Lohn.  Wir haben ihn zwar noch nicht bekommen, aber
er steht uns zu.  Wir erheben eine *Forderung* gegenüber unserem
Arbeitgeber Zek.  Daher ist er aktiv und sollte gleich gebucht werden.
Forderungen buchen wir in ein eigenes Aktivkonto `zahlbar`:

    2014-11-01 Zeks Bankenimperium
        aktiv:zahlbar:zek           1.800,00 EUR
        eigen:ertrag:lohn

Es ist praktisch, für jede Person bzw. Firma ein eigenes Unterkonto
unter `aktiv:zahlbar` und `fremd` zu erstellen, denn so sehen wir immer
schon direkt in der Bilanz, wer uns wie viel schuldet und wem wir wie
viel schulden.  Bilanz:

     2.136,50 EUR  aktiv
       336,50 EUR    umlauf
       200,00 EUR      bank:1234
       100,00 EUR      kasse
        32,20 EUR      port
         4,30 EUR      spardose
     1.800,00 EUR    zahlbar:zek
    -1.536,50 EUR  eigen
       652,80 EUR    aufwand
        40,30 EUR      nahrung
        12,50 EUR      pflanzen
       600,00 EUR      wohnung
    -1.800,00 EUR    ertrag:lohn
      -389,30 EUR    start:archiv
      -600,00 EUR  fremd:vermieter

Der Name `zahlbar` ist willkürlich aus der englischen Bezeichnung
*accounts payable* für das Forderungskonto übersetzt.  Wir bevorzugen
mal wieder den kürzeren Namen.

Unser Vermögen beträgt 2.136,50 Euro.  Davon haben wir auf 336,50 Euro
direkt Zugriff.  Nun schlafen wir zwei mal.  Es ist Montag, und Zek hat
unser Geld überwiesen.  Dies ist wieder ein Aktivtausch, da sich unser
Vermögen nicht verändert:

    2014-11-03 Zeks Bankenimperium
        aktiv:umlauf:bank:1234      1.800,00 EUR
        aktiv:zahlbar:zek

Davon gleichen wir sofort unsere Mietschuld aus:

    2014-11-03 Miete
        fremd:vermieter               600,00 EUR
        aktiv:umlauf:bank:1234

Auch diese Buchung muss erklärt werden.  Wir sind soeben 600 Euro
losgeworden.  Warum ist das keine Aufwendung?  Weil die Aufwendung
bereits getätigt wurde und wir jetzt nur noch unsere Schulden bezahlen,
die daraus entstanden sind.  *Verloren* hatten wir das Geld bereits am
1. November!

Erträge und Aufwendungen können wir übrigens so genau nehmen wie wir
möchten.  Wenn wir nach der Arbeit etwas essen oder trinken gehen,
können wir etwa zwischen Preis und Trinkgeld unterscheiden:

    2014-11-03 Quarks Bar
        eigen:aufwand:gastro           23,30 EUR
        eigen:aufwand:trinkgeld         1,70 EUR
        aktiv:umlauf:port


Monatsabschluss
===============

Wir schlafen eine ganze Weile und wachen am Abend des 30. November 2014
auf.  Unsere aktuelle Bilanz sieht folgendermaßen aus:

     1.511,50 EUR  aktiv:umlauf
     1.400,00 EUR    bank:1234
       100,00 EUR    kasse
         7,20 EUR    port
         4,30 EUR    spardose
    -1.511,50 EUR  eigen
       677,80 EUR    aufwand
        23,30 EUR      gastro
        40,30 EUR      nahrung
        12,50 EUR      pflanzen
         1,70 EUR      trinkgeld
       600,00 EUR      wohnung
    -1.800,00 EUR    ertrag:lohn
      -389,30 EUR    start:archiv

Der Monat ist vorbei.  Daher wird es Zeit, ihn abzuschließen.  Wir
führen die so genannte *Gewinn- und Verlustrechnung* (GuV) durch.  Das
heißt nichts anderes als dass wir alle Erträge und Aufwendungen ins
Startkapital hineinbuchen:

    2014-11-30 GuV
        eigen:aufwand:gastro             = 0 EUR
        eigen:aufwand:nahrung            = 0 EUR
        eigen:aufwand:pflanzen           = 0 EUR
        eigen:aufwand:trinkgeld          = 0 EUR
        eigen:aufwand:wohnung            = 0 EUR
        eigen:ertrag:lohn                = 0 EUR
        eigen:start:2014:11

Wenn wir als Buchungsbetrag `= 0 EUR` schreiben, dann buchen wir damit
genau den Betrag, der das Konto auf 0 Euro reduziert.  Im Fall von
`eigen:aufwand:pflanzen` etwa buchen wir hier genau -12,50 Euro.  Wir
werfen einen Blick auf die Bilanz, die nach der GuV schon wesentlich
aufgeräumter aussieht:

        1.511,50 EUR  aktiv:umlauf
        1.400,00 EUR    bank:1234
          100,00 EUR    kasse
            7,20 EUR    port
            4,30 EUR    spardose
       -1.511,50 EUR  eigen:start
       -1.122,20 EUR    2014:11
         -389,30 EUR    archiv

Das Startkapital für Dezember setzt sich nun zusammen aus der
archivierten Vergangenheit und dem Gewinn (in Höhe von 1.122,20 Euro),
den wir im November erwirtschaftet haben.  Nun vergeht der Dezember.
Den simulieren wir hier mit einer einzigen Buchung:

    2014-12-01 Dezember
        eigen:aufwand:nahrung         250,00 EUR
        eigen:aufwand:wohnung         600,00 EUR
        eigen:ertrag:lohn          -1.800,00 EUR
        aktiv:umlauf:bank:1234        950,00 EUR

Wieder führen wir eine GuV durch, um den Dezember abzuschließen:

    2014-12-31 GuV
        eigen:aufwand:nahrung            = 0 EUR
        eigen:aufwand:wohnung            = 0 EUR
        eigen:ertrag:lohn                = 0 EUR
        eigen:start:2014:12

Im Eigenkapital der Bilanz sehen wir, dass wir erneut einen Gewinn
gemacht haben, aber diesmal war er nicht so hoch wie im November:

    -2.461,50 EUR  eigen:start
    -2.072,20 EUR    2014
    -1.122,20 EUR      11
      -950,00 EUR      12
      -389,30 EUR    archiv

Diese Liste wächst natürlich mit der Zeit.  Daher können wir ab und zu
mal aufräumen, indem wir die einzelnen 2014-Monate archivieren.  Wir
möchten aber immer noch sehen, wie sich unser Kapital im Jahr 2014
insgesamt entwickelt hat.  Daher legen wir unter `2014` ein Unterkonto
`archiv` an, unser Jahresarchiv, in dem wir alle Monate zusammenfassen:

    2014-12-31 Archiv
        eigen:start:2014:11              = 0 EUR
        eigen:start:2014:12              = 0 EUR
        eigen:start:2014:archiv

Diese Monatsarchivierung können wir auch während des Jahres vornehmen,
wenn wir wollen.  Falls wir beispielsweise immer nur die letzten drei
Monate sehen wollen, buchen wir einfach etwa am Ende des Monats April
das Startkapital vom Januar ins Jahresarchiv, Ende Mai buchen wir den
Februar ins Jahresarchiv, etc.

Irgendwann, vielleicht am Ende des Jahres 2024, interessiert uns das
Jahr 2014 überhaupt nicht mehr.  Dann können wir es endgültig
archivieren, indem wir den Kontostand ins Gesamtarchiv umbuchen:

    2024-12-31 Archiv
        eigen:start:2014:archiv          = 0 EUR
        eigen:start:archiv

Das Eigenkapital können wir verwalten wie wir wollen.  Wir können etwa
gänzlich auf eine Unterteilung des Startkapitals verzichten und unsere
GuV direkt über `eigen:start` abschließen.  Auch eine Unterteilung in
einzelne Ertrags- und Aufwendungskategorien dient nur unserer
persönlichen Übersicht.
