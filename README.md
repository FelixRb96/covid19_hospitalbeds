# Covid19 Tool zum Agreggieren und Visualisieren von Krankenhauskapazitäten | #WirvsVirus Hackathon

### Idee

Wir erstellen einen Proof of Concept um:

    Daten von Krankenhauskapazitäten zusammenzutragen
    diese interaktiv in einer Karte gegen die aktuelle Lage der Coronafälle zu visualisieren

Krankenhäusern, Behörden oder anderen Gruppen wird dadurch ein Tool zu geben, welche Einrichtungen im Umkreis bezüglich freier Kapazitäten anzeigt um somit die Auslastung der Krankenhäuser effizienter gestalten zu können. Ebenso soll es somit erleichtert werden, Krisengebiete schnell auf der Karte sichtbar zu machen und durch entsprechende Erweiterungen mithilfe von Predictive Modelling zukünftige Kriesengebiete frühzeitig zu erkennen. Somit wird die rechtzeitige Planung entsprechender Massnahmen effektiver. Das Predictive Modelling kann auf den zusammengetragenen Daten (Zeitreihen zu lokalisierten Fällen und Kapazitäten), ist aber mangels Zeit noch nicht implementiert.

Ferner unterscheidet die Plattform Nutzergruppen und ist datenbankzentriert, um unvorhersehbare Herausforderungen flexibel koordinieren zu können - Idee: Keine 10.000 verschiedene Plattformen für einzelne Probleme wie Helferbörse, Krankenhausverfügbarkeit, Schutzmaskenallokation oder Rückflugsplanung, sondern eine einziges Datenbanksystem für alle COVID-19-relevanten Herausforderungen. Wenn etwas neues hinzukommt, sollen einfach ein weiterer "Tab" und eine weitere Checkbox für der Karte entstehen.
Umsetzung

Aufgrund der Tatsache, dass wir ein zwei Personen Team sind und somit unsere Ressourcen eingeschraenkt sind haben wir mit einem Fork des folgenden Dashboards begonnen: Fork

Wir konzentrieren uns auf die Funktionalität, dass Krankenhäuser oder andere Nutzer mit entsprechender Berechtigung Daten eingeben können. Denkbar wäre eine Nutzung, die zentral durch den Bund koordiniert wird. Insbesondere ist es hier besonders einfach in R entwickelte Vorhersagetools einzubinden und Trend Predictions live in der Karte zu simulieren. Ebenso ist eine Anbindung an bestehende Datenbanken und ein schnelles Deployment auf Servern möglich. Vor allem aber lassen sich schnell weitere Tools in das Framework einbinden, um die oben genannte Modularität für unvorhergesehene oder dringende Herausforderungen zu gewährleisten.

### Installation und starten des Dashboards

```
git clone https://github.com/FelixRb96/covid19_hospitalbeds.git
R # jump to R console
```
```
> runApp("covid19_hospitalbeds")
```

Das Dashboard sollte sich atuomatisch im Standardbrowser öffnen, an sonsten den Link aus der R Konsole kopieren.

### Struktur

Landing Page ist eine Karte auf der man eine Gegenüberstellung (momentan beispielhafter) Bettenkapazitäten mit Zahlen des Coronavirus vergleichen kann. Nutzende Krankenhäuser können die Daten unter dem Daten hinzugügen Tab eingeben. 

### TODO's und Ausblick

* Marker für die Bettenzahlen am jeweiligen Standort
* richtige Daten für die Betten bekommen
* Deployment auf Server
* UI refactoring 
* Bei ausreichender Datenlage: predictive Modelling
* Dashboard Tab für Bundesländer/ Landkreise

### Sonstiges:
Fork von [diesem Repo](https://github.com/eparker12/nCoV_tracker)
