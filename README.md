# Covid19 Tool zum Agreggieren und Visualisieren von Krankenhauskapazitäten | #WirvsVirus Hackathon

### Idee

Wir erstellen einen proof of concept um Daten von Krankenhauskapazitäten zusammenzutragen und interaktiv in einer Karte gegen die aktuelle Lage der Coronafälle zu Visualisieren. Der Impact einer solchen Anwendung ist es Krankenhauesern ein Tool zu geben, woraus ersichtlich ist welche Einrichtungen im Umkreis freie Kapazitaeten haben und somit die Auslastung der Krankenhaeuser effizienter gestalten zu koennen. Ebenso soll es somit erleichtert werden, Krisengebiete schnell auf der Karte sichtbar zu machen und durch entsprechende Erweiterungen mithilfe von Predictive Modelling zukuenftige Kriesengebiete fruhzeitig zu erkennen. Somit wird die rechtzeitige Planung entsprechender Massnahmen effektiver erhoffen wir uns.

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

### Sonstiges:
Fork von [diesem Repo](https://github.com/eparker12/nCoV_tracker)
