---
title:     "Ledger: 1. Grundlagen"
author:    "ertes"
date:      "2015-08-06"
lang:      "de"
copyright: "Ertugrul Söylemez"
want-toc:  "yes"
...

Vor Kurzem [habe ich euch versprochen][diaspora-post], eine
Software-Demo zu [Ledger] zu schreiben.  Ohne das entsprechende
Grundwissen, nämlich der *doppelten Buchführung*, wäre diese aber
ziemlich nutzlos, und dieses Wissen ist außer bei BWLern nicht sehr weit
verbreitet.  Daher beschloss ich, stattdessen nun eine komplette
deutsche Einführung zu schreiben, sowohl in die doppelte Buchführung
selbst als auch in das Programm.

Eines vorweg:  Weder bin ich BWLer, noch war ich je ein großer Freund
von BWL als Schulfach.  Die *doppelte Buchführung*, die wir hier lernen
werden, ist aber genau das, was ich mir unter einem guten Werkzeug
vorstelle:  sehr einfach und sehr mächtig.  Sie zu lernen lohnt sich und
zahlt sich sehr schnell aus, das verspreche ich euch.  Okay, los geht's!

[![Creative Commons Attribution-ShareAlike 4.0 International License](https://i.creativecommons.org/l/by-sa/4.0/88x31.png)][cc-by-sa]
This work is licensed under a
[Creative Commons Attribution-ShareAlike 4.0 International License][cc-by-sa].

[cc-by-sa]: http://creativecommons.org/licenses/by-sa/4.0/
[diaspora-post]: https://nerdpol.ch/posts/1814437
[ledger]: http://ledger-cli.org/


Vorwort
=======

Wir sind alle ein wenig gierig, nicht wahr?  Es kann dabei um Geld
gehen, muss es aber nicht.  Genau so gut können wir auch gierig sein
nach

  * Filmen oder Musik,
  * Karten in einem Rollenspiel,
  * Kunstwerken oder Büchern, etc.

Wir können auch von einer oder mehrerer dieser *Resourcen* eine
beachtliche Sammlung vorweisen, haben aber lange den Überblick verloren,
besonders darüber, wie genau sich unsere Sammlung zusammensetzt, wie sie
sich über die Jahre verändert hat, wem wir was verliehen haben und wer
uns was ausgeliehen hat.

[Ledger] ist ein sehr mächtiges und dennoch einfaches
Buchführungsprogramm, das uns den Überblick und die Kontrolle
zurückgeben wird.  Es basiert auf einem simplen Prinzip:  Wir schreiben
jeden Tag unsere Buchungen in eine einfache reine Textdatei.  Dazu
können wir unseren Lieblingseditor verwenden, egal ob Emacs, TextMate,
Vim oder auch einfach nur Notepad.  Anschließend generiert uns Ledger
daraus nahezu beliebige *Reporte*, etwa unsere aktuellen Kontostände,
unsere Schulden, etc.  Ledger kann uns zu jedem Konto genau sagen, wie
sein Betrag zustandekommt.  Wir können dabei so genau werden wie wir
möchten.

Ich selbst verwalte meine Finanzen, meine Hardware, meine Bücher und
meine Aufgaben und Projekte damit.  Es ersetzt für mich nicht nur das
herkömmliche Buchhaltungsprogramm für Finanzen, sondern auch den
Aufgabenplaner (etwa org-mode oder TaskWarrior) und Datenbanken für
Sachanlagen, also das, wofür die meisten eine Tabellenkalkulation
o.ä. nehmen würden.  In meiner Hardwareverwaltung mit Ledger sehe ich
nicht nur, welche Geräte genau ich besitze, sondern auch wie ich deren
Werte einschätze, falls ich sie mal verkaufen sollte, und wie er sich
über die Monate verändert.

An sich ist Ledger ein Kommandozeilenprogramm, und wir werden es in
diesem Artikel als solches kennenlernen.  Später ist es sinnvoll, es in
den Texteditor zu integrieren, sodass wir nicht nur die Kommandozeile
nicht mehr brauchen, sondern auch Eingaben viel schneller tätigen können
(mit etwas Übung in wenigen Sekunden!).  Für die Integration in [Emacs]
werde ich einen eigenen Artikel schreiben.  Falls ihr Emacs nutzt und
affin seid, könnt ihr das aber auch jetzt schon [selbst
erledigen][emacs-integration], wenn ihr wollt.

[emacs]: https://www.gnu.org/software/emacs/
[emacs-integration]: http://ledger-cli.org/3.0/doc/ledger-mode.html


Einführung
==========

Ledger ist ein Kommandozeilenprogramm, das eine reine Textdatei einliest
und daraus einen *Report* erzeugt.  Diese Datei bezeichnen wir als unser
*Buch*.  Es gibt verschiedene Reporte, aber für die meisten sind nur
zwei davon interessant, nämlich die *Bilanz*, die alle aktuellen
Kontostände anzeigt, und das *Register*, das den Buchungsverkehr zu
einem bestimmten Thema anzeigt.

Besonders mächtig ist das Register, da wir unserem Buch damit sehr viele
verschiedene Fragen stellen können.  Je genauer wir unsere Finanzen
verwalten, desto genauere Fragen können wir stellen.  Hier etwa einige
Beispiele zu Hardware, falls wir sie so genau verwalten (und das werden
wir, aber erst in einem späteren Artikel):

  * Wie viel habe ich diesen Monat für Hardware ausgegeben?

  * Wie viel muss ich in den nächsten drei Monaten für meine aktuellen
    Hardware-Finanzierungen (Kredite) ausgeben?

  * Welche Hardware besitze ich momentan?

  * Wie hoch schätze ich ihren Wert ein?

  * Wie viel Wert hat mein Laptop in den letzten drei Monaten meiner
    Meinung nach verloren?

  * Letzten Monat habe ich Hardware verkauft.  Welche Gewinne oder
    Verluste habe ich dabei durch Fehleinschätzungen der Werte gemacht?


Die erste Buchung
=================

Kommen wir gleich zur Sache.  Wir machen eine Zeitreise zum 1. August
2015.  An diesem Tag eröffnen wir unser Buch.  Wir starten also unseren
Texteditor und erstellen eine Datei namens `tutorial.ledger`.  Es
spricht nichts dagegen, sie `tutorial.txt` oder auch anders zu nennen,
wenn ihr wollt.  In dem Fall müsst ihr aber die unten aufgeführten
Ledger-Befehle entsprechend abändern.  Wir tragen folgenden Inhalt ein:

    2015-08-01 Start
        aktiv          500,00 EUR
        eigen         -500,00 EUR

Bei der Formatierung des Inhalts ist Ledger relativ flexibel.  Wichtig
ist nur, dass die erste Zeile ganz links steht, während die restlichen
eingerückt sind.  Außerdem müssen zwischen dem Kontonamen (`aktiv`
bzw. `eigen`) und dem Betrag mindestens zwei Leerzeichen stehen.  Das
Datum wird von Ledger verarbeitet und muss daher in einem von zwei
Formaten notiert werden, entweder `JJJJ-MM-TT` oder `JJJJ/MM/TT`.

Zusammen mit diesem Eintrag werden wir jetzt schon die wichtigsten
Begriffe kennenlernen, die wir für die Buchführung brauchen.  Was wir
hier geschrieben haben, wird als *Buchung* (auch *Transaktion*)
bezeichnet.  Werte, egal ob Geld-, Zeit-, Sach- oder sonstige Mengen,
befinden sich stets in *Konten*, und eine Buchung bewegt Werte zwischen
Konten.  In diesem Fall haben wir 500 Euro vom Konto `eigen` abgebucht
und auf das Konto `aktiv` aufgebucht (wir dürfen das als *Überweisung*
bezeichnen).  Die einzelnen Wertveränderungen einer Buchung werden als
*Posten* bezeichnet.  Unsere Buchung setzt sich aus zwei Posten
zusammen.  Die Namen der Konten sind bewusst gewählt und werden in Kürze
klarer.

Wir haben eine *Währung* verwendet: `EUR`.  Sowohl der Name der Währung
als auch die Formatierung der Menge sind nahezu frei wählbar, und Ledger
wird sich uns anpassen.  Wir hätten auch `500 Euro`, `EUR 500.00` oder
`EUR 500,00` schreiben können.  Bei größeren Beträgen dürfen wir auch
Tausendermarkierungen verwenden, z.B. `1.500,00 EUR`.  Für Ledger ist
eine *Währung* alles, was quantifiziert werden kann.  Dazu gehört
natürlich Geld, aber auch Sachanlagen oder Zeit.  Wir können etwa `50
Bücher` oder `5h` (5 Stunden) als Mengen angeben.

Nun, da wir unsere erste Buchung geschrieben haben, wollen wir Ledger
unsere aktuellen Kontostände berechnen lassen.  Wir starten also eine
Kommandozeile, wechseln in das Verzeichnis, das unser Buch enthält und
geben ein:

    ledger -f tutorial.ledger bal

Ledger berechnet die aktuelle *Bilanz* (englisch *balance*, daher
auch `bal`) und zeigt sie an:

              500,00 EUR  aktiv
             -500,00 EUR  eigen
    --------------------
                       0

Alle Konten beginnen mit dem Wert 0.  Da wir 500 Euro von `eigen` auf
`aktiv` gebucht haben, weist `eigen` natürlich einen negativen Betrag
auf.  Das ist erwartungsgemäß.

Bei der doppelten Buchführung dreht sich alles um Ausgeglichenheit.
*Ausgeglichen* bedeutet dabei ganz konkret: "die Summe ist null."  Jede
Buchung muss ausgeglichen sein.  Die Summe der einzelnen Posten muss
also genau 0 betragen.  Hätten wir von `eigen` etwa nur 400 Euro oder
gar nichts abgebucht, hätte Ledger eine Fehlermeldung ausgegeben.
Daraus folgt, dass auch die Summe aller Kontostände stets 0 ist sein
wird.

Der Text, der neben dem Datum steht, in unserem Fall "Start", wird als
*Empfänger* (englisch *payee*) bezeichnet.  In Wirklichkeit ist dieser
Text aber völlig frei wählbar und ist einfach nur eine Buchungsnotiz.
Wir geben also an, dass wir am Anfang 500 Euro besitzen.

Nun kaufen wir uns, immer noch am 1. August, ein Buch für 10,99 Euro.
Das notieren wir, indem wir eine weitere Buchung eintragen, einfach
direkt unter der obigen Buchung (ihr dürft Leerzeilen lassen, um die
Lesbarkeit zu verbessern):

    2015-08-01 Buch
        eigen           10,99 EUR
        aktiv

Das gesamte Buch sieht jetzt also folgendermaßen aus:

    2015-08-01 Start
        aktiv          500,00 EUR
        eigen         -500,00 EUR

    2015-08-01 Buch
        eigen           10,99 EUR
        aktiv

Wir haben nun vom Konto `aktiv` 10,99 Euro abgebucht und auf das Konto
`eigen` aufgebucht.  Da Buchungen ohnehin ausgeglichen sein müssen,
können wir einen der Beträge immer weglassen.  Ledger weiß, dass der
Buchungsbetrag beim zweiten Posten -10,99 Euro betragen muss.  Nun
fragen wir erneut nach der Bilanz:

              489,01 EUR  aktiv
             -489,01 EUR  eigen
    --------------------
                       0

Die letzen beiden Zeilen der Bilanz zeigen die Kontensumme an.  Wie
bereits erwähnt, ist diese (bei uns) stets 0.  Daher werde ich sie in
den weiteren Bilanzen weglassen.


Vermögen und Kapital
====================

Nun wollen wir verstehen, was es mit den Konten auf sich hat.  Soweit es
die Buchführung betrifft, sind wir ein *Unternehmen*, und jedes
Unternehmen hat einen bestimmten Wert, sein *Vermögen*.  Der Fachbegriff
für Vermögen ist *Aktiva*, daher auch der Kontoname `aktiv`.

Wie bereits angedeutet kommt das Vermögen aber nicht aus dem Nichts,
sondern es wird *beigesteuert*, und zwar aus *Kapital*.  Der Fachbegriff
des buchhalterischen Kapitals ist *Passiva*.  In diesem Fall haben wir
das Vermögen selbst beigesteuert.  Es handelt sich um unser
*Eigenkapital*, daher auch der Kontoname `eigen`.

Mit anderen Worten:  Vermögen ist, worüber wir verfügen, und Kapital
ist, wem wir es schulden (daher der negative Betrag bei `eigen`), also
wem es gehört.  Wir verfügen im Moment also über 489,01 Euro und
schulden es gewissermaßen uns selbst.

Neben dem Eigenkapital gibt es auch *Fremdkapital*.  Dieses tritt auf,
wenn wir uns etwas ausleihen, und das wollen wir jetzt tun.  Unser
Freund Quark leiht uns 50 Euro aus, und wir verbuchen das:

    2015-08-01 Quark
        aktiv           50,00 EUR
        fremd

Werfen wir einen Blick auf unsere Bilanz:

     539,01 EUR  aktiv
    -489,01 EUR  eigen
     -50,00 EUR  fremd

Unser Vermögen ist auf 539,01 Euro gewachsen, und wir sehen nun
deutlich, dass es sich aus 489,01 Euro Eigenkapital und 50 Euro
Fremdkapital zusammensetzt.  Am nächsten Tag kaufen wir uns ein Notebook
im Wert von 500 Euro:

    2015-08-02 Ishkas Computerladen
        eigen          500,00 EUR
        aktiv

Für den Anfang betrachten wir Einkäufe als *Aufwendungen*, also als
Verluste.  Das ist natürlich nicht ganz richtig, da das Notebook selbst
einen Wert hat, aber das ignorieren wir vorerst mal.

Jede Aufwendung ist eine Buchung auf das Eigenkapital, weil wir den
Verlust selbst verantworten.  Wir können mit einem Verlust etwa nicht
die Schuld tilgen, die wir gegenüber Quark haben.  Die Bilanz nach dem
Verlust:

     39,01 EUR  aktiv
     10,99 EUR  eigen
    -50,00 EUR  fremd

Hier sehen wir auch gleich, dass Kapitalkonten nicht immer einen
negativen Betrag aufweisen.  Das Eigenkapitalkonto ist dann positiv,
wenn wir überschuldet sind.  Der Kauf des Notebooks hat uns also pleite
gemacht.  Wir haben zwar noch Vermögen, aber das gesamte Vermögen gehört
jemand anderem.  Da unser Eigenkapital ins Positive gerutscht ist, haben
wir nicht mal genug Vermögen, um unsere Schuld zu bezahlen.  Auf gut
deutsch:  Wir sind *bankrott*.  Schlecht!

Natürlich hätten wir diesen Kauf nicht getätigt, wenn wir nicht wüssten,
dass uns ab dem 3. August 1.800 Euro von unserem Arbeitgeber zustehen.
Dabei handelt es sich um einen *Ertrag*, also einen Gewinn.  Erträge
kommen aus dem Eigenkapital, da wir diese niemandem schulden.  Wir
verantworten sie selbst, denn immerhin haben wir für diesen Ertrag
gearbeitet:

    2015-08-03 Zeks Bankenimperium
        aktiv        1.800,00 EUR
        eigen

Unsere Bilanz zeigt, dass es uns jetzt wieder wesentlich besser geht:

     1.839,01 EUR  aktiv
    -1.789,01 EUR  eigen
       -50,00 EUR  fremd

Es gibt nur ein Problem:  Das Geld gehört zwar zu unserem Vermögen, aber
wir besitzen es noch nicht.  Das ist der Unterschied zwischen Besitz und
Verfügung.  Es handelt sich um eine Forderung, die wir gegenüber Zeks
Bankenimperium erheben.  Am nächsten Tag, also am 4., haben wir unser
Geld auch tatsächlich erhalten, aber das werden wir erst im nächsten
Artikel buchen können.  Immerhin können wir unsere Schulden begleichen:

    2015-08-04 Quark
        fremd           50,00 EUR
        aktiv

Dadurch, dass wir 50 Euro ins Fremdkapital zurückgebucht haben, stehen
wir wieder komplett auf eigenen Beinen:

     1.789,01 EUR  aktiv
    -1.789,01 EUR  eigen

Wir könnten dieses Buch nun so weiterführen, aber auf diese Weise können
wir nur einen kleinen Teil des Potentials von Ledger ausreizen.  Wir
haben ausschließlich mit Hauptkonten gearbeitet, um das Konzept von
Vermögen und Kapital zu lernen, worauf alles Weitere basiert.

Im [nächsten Artikel](ledger-02-perioden.html) werden wir unsere drei
Hauptkonten in Unterkonten aufteilen und die Idee der
Buchführungsperioden einführen, damit unsere Bilanz uns anzeigt, worüber
wir verfügen, was davon wir konkret besitzen, wem wir wie viel schulden
und wie wir uns im Vergleich zu einem vergangenen Zeitpunkt entwickeln.
Alles, was dann nicht schon in der Bilanz sichtbar ist, wird uns das
Register mitteilen, das wir hier noch überhaupt nicht eingesetzt haben.


Traditionelle Buchführung
=========================

Wer BWL studiert hat oder als Schulfach hatte, erinnert sich vielleicht
daran, dass das, was dort erklärt wurde, von unserer Buchführung ein
wenig abweicht.  Es war von zweispaltigen "T-Konten", von *Soll* und
*Haben* und von *Salden* die Rede.

Diese traditionelle Methode stammt aus einer Zeit, als es noch keine
Computer gab.  Buchungen wurden stets *doppelt* getätigt (daher auch der
Name), und zwar so, dass die Beträge, die entstanden, stets positiv
waren.  Abschlüsse wurden mit Hilfe von *Salden* gemacht, um die beiden
Spalten dieser Konten auszugleichen.

Was wir hier gelernt haben, ist aber vollständig äquivalent dazu und
darüber hinaus einfacher zu verstehen.  Im Wesentlichen besteht der
Unterschied darin, dass wir unsere Kapitalkonten mit negativen Beträgen
führen (sofern wir nicht überschuldet sind), und dass wir positive und
negative Beträge statt zweier positiver Spalten haben.

Anders gesagt:  Keine Sorge, ihr lernt hier genau dasselbe, nur in
moderner computergestützter Form. =)
