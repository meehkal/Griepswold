# Portfolio: Namensgebung bei trans\* Personen

# Projektidee

Bei trans\* Personen sind andere Voraussetzungen bei der Namenswahl
gegeben, als bei der Namenswahl für Babys. Zum einen erfolgt die Wahl
offensichtlich nicht in den Monaten um die Geburt, sondern später im
Leben. Zum anderen treffen bei der Wahl zumeist die Person selbst -
nicht deren Eltern - die ausschlaggebende Entscheidung. Zudem sind
trans\* Personen als Teil der lgbtq+ community auch Teil deren
Subkultur, welche sich nicht nur auf Deutschland beschränkt. Daraus
ergibt sich die Fragestellung, ob es bei trans\* Menschen bestimmte
Präferenzen gibt die Struktur der Namen betreffend; und ob bzw.
inwiefern sich oben genannte Faktoren auf einen von Babynamen
divergierenden Trend auswirken.

### Arbeitsaufteilung

Jana und ich haben die Idee gemeinsam entworfen und den Umfragebogen
erstellt. Die die Analyse und die schriftliche Ausführung wurden von mir
(Jona) geschrieben.

------------------------------------------------------------------------

# Überlegungen zur Umsetzung

## Ethische Überlegungen

Bei Namen handelt es sich um persönliche Daten. Umso sorgsamer gilt es
solche Daten zu schützen, wenn sie eine marginalisierte und
diskriminierte Gruppe betreffen. Deshalb fragen wir nicht nach
Wohnregion, Nachname oder anderen Daten, die die Identifikation der
Teilnehmenden möglich machen könnten. Aus demselben Grund fordern wir
Teilnehmende dazu auf, für ihren Zweitnamen - falls vorhanden - den
Fragebogen erneut auszufüllen, damit sich Erst- und Zweitname nicht
eindeutig kombinieren lassen und somit die Anonymität erhöht wird. Zudem
schließen wir Minderjährige aus der Befragung aus.

## Inhaltliche Überlegungen

Um die erste Frage der Projektidee zu beantworten - ob es bei trans\*
Menschen bestimmte Präferenzen bzgl. der Namenstruktur gibt - können
Faktoren wie Länge, sowohl Silben- als auch Buchstabenzahl, Vokalanteil
und Konsonanten betrachtet werden. Bei der Analyse phonetischer Merkmale
ergibt sich jedoch das Problem, dass Namen verschieden ausgesprochen
werden können, sodass man eine Audioaufnahme der\*des Befragten
bräuchte. Diese Daten sind durch eine online Umfrage schwerer zu
erheben, denn - abgesehen vom technischen und rechtlichen Rahmen -
würden möglicherweise weniger trans\* Menschen teilnehmen aufgrund von
Stimmdysphorie. Deswegen orientieren wir uns in diesem exemplarischen
Projekt nur an der Schreibung anstatt einer phonetischen Transkription.

Damit diese Analyse aufschlussreich sein kann und auch um die zweite
Frage zu beantworten - ob es unterschiedliche Trends bei trans\* Namen
und Babynamen gibt - braucht man eine Vergleichsgruppe. In diesem Fall
sind das Babynamen aus dem Zeitraum der Geburtsjahre und Jahre der
Namenswahl der befragten trans\* Menschen. Damit kann untersucht werden,
ob der ausgewählte Name eher dem Trend des Geburtsjahrs oder eher dem
entspricht, welche Namen im Jahr des Namenswechsels beliebt waren.

Das Problem dabei ist, dass keine Vornamensstatistiken auffindbar sind,
die open source zur Verfügung stehen. D.h. entweder man müsste auch die
Namen der Vergleichsgruppe selbst durch eine Umfrage erheben. Als Daten
reichen hier der Vorname und das Geburtsjahr aus. Natürlich könnte dafür
auch die Babynamen der trans\* Personen erfragen, da es sich bei sog.
Deadnames jedoch für viele um ein sensibles Thema, entmutigt eine solche
Frage Teilnehmende und führt außerdem in Kombination mit dem
selbstgewählten Namen zu einer geringeren Anonymisierung.

Da dies nur ein exemplarisches Projekt ist, verwende ich als
Vergleichsgruppe Namen aus den Top Ten der vergangenen Jahre.[^1]

Motive für die Namenswahl wie z.B. Wohlklang, Nachbenennung, Bedeutung
oder Ähnlichkeit zum Deadname werden hier ausgeklammert, könnten aber in
einer nicht-exemplarischen Untersuchung dieses Themas miteinbezogen

Wir wollen eine quantitative Analyse durchführen, weshalb die Analyse
mittels R erfolgt.

------------------------------------------------------------------------

# Datenerhebung

Die Umfrage erfolgte im Dezember 2023 mittels *evasys* und wurde
sechsmal ausgefüllt. Im Unterordner “Datenerhebung” ist ein leerer
Umfragebogen (Fragebogen_leer) im .pdf Format, die erhobenen Primärdaten
(primärdaten_umfrage) im .csv Format zu finden, ebenso wie der
exemplarisch erstellte Datensatz der Vergleichsgruppe
(daten_vergleichsgruppe).

Wenn das Projekt nicht nur exemplarisch wäre, bräuchte die Umfrage eine
deutlich größere Reichweite. Dafür könnte man z.B. auf sozialen Medien
Konten, die sich mit queeren Themen beschäftigen, anschreiben und darum
bitten, den Link zur Umfrage zu teilen.

------------------------------------------------------------------------

# Analyse

Zuerst erfolgt …

Mithilfe folgenden Codes (geschrieben in R) ergeben sich diese beiden
Tabelle mit den sekundären Daten.

<details>
<summary>Code</summary>

``` r
library(tidyverse)
library(readr)
# import data sets
show_col_types = FALSE
primärdaten_umfrage <- read_csv("primärdaten_umfrage.csv")
daten_vergleichsgruppe <- read_delim("daten_vergleichsgruppe.csv", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

# tidy data
transnames <- primärdaten_umfrage |> 
  select(-(zeitstempel:datensatz_ursprung))

transnames$name <- tolower(transnames$name)
daten_vergleichsgruppe$name <- tolower(daten_vergleichsgruppe$name)

# function to analyze tibble
analyse_names <- function(tbl) {
  tbl|> 
  mutate(length_by_letters = str_length(name),
         first_letter = str_extract(name, "^\\w"),
         last_letter = str_extract(name, "\\w$"),
         vowel_prop = (str_count(name, "[aeiou]")/str_length(name)),
         kons_plosiv = str_count(name, "[pbtdkg]"),
         kons_nasal = str_count(name, "[mn]"),
         kons_frikativ = str_count(name, "[fvsz]"),
         kons_vibrant = str_count(name, "[r]"),
         kons_approximant = str_count(name, "[lj]")
  )}

# analyzing transnames and vergleichsgruppe
transnames_analysed <- analyse_names(transnames)
vergleichsgruppe_analysed <- analyse_names(daten_vergleichsgruppe)

# output
knitr::kable(transnames_analysed,)
```

</details>

| ref | name       | namen_position | year_of_birth | year_of_name_change | maennlich | weiblich | transmaskulin | transfeminin | nicht_binaer | agender | genderfluid | anders_gender | anders_genderidentity_specified | length_by_letters | first_letter | last_letter | vowel_prop | kons_plosiv | kons_nasal | kons_frikativ | kons_vibrant | kons_approximant |
|----:|:-----------|---------------:|--------------:|--------------------:|----------:|---------:|--------------:|-------------:|-------------:|--------:|------------:|--------------:|:--------------------------------|------------------:|:-------------|:------------|-----------:|------------:|-----------:|--------------:|-------------:|-----------------:|
|   1 | jona       |              1 |          2002 |                2022 |         0 |        0 |             1 |            0 |            0 |       0 |           0 |             0 | NA                              |                 4 | j            | a           |       0.50 |           0 |          1 |             0 |            0 |                1 |
|   2 | robin      |              2 |          2002 |                2022 |         0 |        0 |             1 |            0 |            0 |       0 |           0 |             0 | NA                              |                 5 | r            | n           |       0.40 |           1 |          1 |             0 |            1 |                0 |
|   3 | finn       |              1 |          1999 |                2020 |         0 |        0 |             1 |            0 |            1 |       0 |           0 |             0 | NA                              |                 4 | f            | n           |       0.25 |           0 |          2 |             1 |            0 |                0 |
|   4 | noah       |              1 |          1999 |                2014 |         1 |        0 |             0 |            0 |            0 |       0 |           0 |             0 | NA                              |                 4 | n            | h           |       0.50 |           0 |          1 |             0 |            0 |                0 |
|   5 | elias      |              2 |          1999 |                2014 |         1 |        0 |             0 |            0 |            0 |       0 |           0 |             0 | NA                              |                 5 | e            | s           |       0.60 |           0 |          0 |             1 |            0 |                1 |
|   6 | aleksander |              1 |          2000 |                2023 |         1 |        0 |             0 |            0 |            1 |       0 |           0 |             0 | NA                              |                10 | a            | r           |       0.40 |           2 |          1 |             1 |            1 |                1 |

<details>
<summary>Code</summary>

``` r
knitr::kable(vergleichsgruppe_analysed, )
```

</details>

| ref | name       | year_of_birth | length_by_letters | first_letter | last_letter | vowel_prop | kons_plosiv | kons_nasal | kons_frikativ | kons_vibrant | kons_approximant |
|----:|:-----------|--------------:|------------------:|:-------------|:------------|-----------:|------------:|-----------:|--------------:|-------------:|-----------------:|
|   1 | maximilian |          2002 |                10 | m            | n           |  0.5000000 |           0 |          3 |             0 |            0 |                1 |
|   2 | noah       |          2022 |                 4 | n            | h           |  0.5000000 |           0 |          1 |             0 |            0 |                0 |
|   3 | leon       |          2020 |                 4 | l            | n           |  0.5000000 |           0 |          1 |             0 |            0 |                1 |
|   4 | matteo     |          2023 |                 6 | m            | o           |  0.5000000 |           2 |          1 |             0 |            0 |                0 |
|   5 | lukas      |          1999 |                 5 | l            | s           |  0.4000000 |           1 |          0 |             1 |            0 |                1 |
|   6 | alexander  |          2000 |                 9 | a            | r           |  0.4444444 |           1 |          1 |             0 |            1 |                1 |

Mit den wenigen Daten sind selbstverständlich keine aussagekräftigen
Graphiken möglich. Deshalb sind folgende Graphiken nur beispielhaft.

Folgende Graphiken sollen veranschaulichen, ob es bei den Namen von
trans\* Personen verhältnismäßig eine Präferenz bzgl. verschiedener
Konsonantenarten, z.B. Plosive gibt.

![](Portfolio_files/figure-commonmark/unnamed-chunk-2-1.png)

![](Portfolio_files/figure-commonmark/unnamed-chunk-2-2.png)

![](Portfolio_files/figure-commonmark/unnamed-chunk-2-3.png)

![](Portfolio_files/figure-commonmark/unnamed-chunk-2-4.png)

![](Portfolio_files/figure-commonmark/unnamed-chunk-2-5.png)

Die nächsten beiden Graphiken vergleichen die durchschnittliche Länge
von Babynamen mit der von trans\* Namen, wobei Erstere das Geburtsjahr
und Zweitere das Wahljahr als Anhaltspunkt verwendet.

![](Portfolio_files/figure-commonmark/unnamed-chunk-3-1.png)

![](Portfolio_files/figure-commonmark/unnamed-chunk-3-2.png)

------------------------------------------------------------------------

# Fazit

Natürlich sind aufgrund des geringen Umfang des Datensatzes keine
Schlussfolgerungen möglich.

[^1]: https://gfds.de/vornamen/beliebteste-vornamen/ (eingesehen:
    19.01.2024)
