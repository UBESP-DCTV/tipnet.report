# tipnet.report

Pacchetto R che implementa il sistema di reportistica della **TIP-Net** (Rete Italiana delle Unità di Terapia Intensiva Pediatrica), sviluppato dall'Unità di Biostatistica, Epidemiologia e Sanità Pubblica ([UBEP](https://www.unipd-ubep.it/)) — Università di Padova.

Il sistema produce due tipologie di report:

1. **Report dinamico interattivo** (`index.Rmd`) — dashboard Shiny per l'esplorazione continua dei dati, aggiornabile in tempo reale dai server di produzione.
2. **Report statico annuale/semestrale** (`analyses/YYYY-static_report.R`) — snapshot parametrizzato in HTML, generato periodicamente.

---

## Report dinamico (`index.Rmd`)

### Cosa fa

È una web app Shiny (`runtime: shiny_prerendered`) che si connette automaticamente al database REDCap della rete TIP-Net e produce una serie di sezioni analitiche interattive:

| Sezione | File | Contenuto |
|---|---|---|
| Qualità dati | `005-quality.Rmd` | Completezza dei record per centro e tipo di scheda |
| Descrittive | `010-descriptives.Rmd` | Distribuzione per genere, etnia, età |
| Provenienza | `020-origin.Rmd` | Origine del ricovero |
| Continuità cure | `030-ccc.Rmd` | Centri di cura continua |
| Focus | `035-focus.Rmd` | Analisi tematiche specifiche |
| Durata degenza | `050-los.Rmd` | LOS e dimissione dalla PICU |

Ogni sezione è implementata tramite un **modulo Shiny** (`R/module-XxxReport.R`) con le relative funzioni di supporto (`R/funs-XxxReport.R`). I grafici sono interattivi (plotly). L'utente può filtrare i dati per intervallo di anni tramite il selettore `yearRangeUI`.

### Flusso dati

```
REDCap API
    ↓  db_update_from_server()
../tipnet-data/tipnet.rds          ← cache locale (aggiornamento automatico se scaduta)
    ↓  generate_main_data()
data_ranged[["full_filtered"]]     ← struttura nested usata dai moduli
```

- Al primo avvio (o quando il file è scaduto) il report scarica automaticamente i dati da REDCap tramite il token `REDCAP_TIPNET_PAT` nel file `.Renviron`.
- I dati vengono salvati anche come snapshot datato: `../tipnet-data/YYYY-MM-DD-tipnet.rds`.
- La data dell'ultimo aggiornamento è mostrata nell'intestazione del report.

### Parametri YAML (`index.Rmd`)

| Parametro | Default | Descrizione |
|---|---|---|
| `cache` | `FALSE` | Attiva la cache knitr per il chunk `data` |
| `export` | `FALSE` | Modalità export statico (disabilita Shiny, abilita output fisso) |
| `need_privacy` | `TRUE` | Oscura le città dei centri nell'output |
| `force_update` | `FALSE` | Forza il re-download da REDCap anche se i dati sono recenti |

### Avviare il report in locale

Aprire `index.Rmd` in RStudio e cliccare **Run Document**, oppure:

```r
rmarkdown::run("index.Rmd")
```

In modalità interattiva (`.Rprofile` imposta `options(tipnet.dev = TRUE)`) vengono eseguiti automaticamente `roxygen2::roxygenise()` e `devtools::load_all()` prima del render.

### Deployment in produzione

Il report gira come app Shiny su un server Linux in `/srv/shiny-server/TIPNet`. Il `Makefile` gestisce gli aggiornamenti:

```bash
make update        # aggiorna codice (git pull + renv::restore) + dati (REDCap)
make update-code   # solo codice
make update-data   # solo dati
```

---

## Report statico (`analyses/YYYY-static_report.R`)

### Cosa fa

Genera uno snapshot HTML parametrizzato dei dati TIP-Net per un anno solare (o semestre), con tre sezioni per ogni centro e per la rete complessiva:

- **Accettazione**: distribuzione per genere ed etnia
- **Descrittive**: tabella riassuntiva stratificata per sesso (quartili, media, SD per variabili continue; frequenze per discrete)
- **SMR**: Standardized Mortality Rate calcolato su PIM2 e PIM3

### Come creare il report di un nuovo anno

1. Copiare il report dell'anno precedente:
   ```bash
   cp analyses/2025-static_report.R analyses/2026-static_report.R
   ```

2. Nel nuovo file aggiornare:
   - `params$year` → anno di interesse
   - `params$first_month` / `params$last_month` → `1`/`12` per report annuale, `1`/`6` per I semestre, `7`/`12` per II semestre
   - La stringa di versione nel campo `date:` dell'intestazione YAML → es. `(ver. 1.0.0)`
   - Il nome del file RDS nella riga `tip_data <- read_rds(...)` → usare l'ultimo snapshot disponibile in `../tipnet-data/` (es. `2026-03-17-tipnet.rds`)

3. Compilare (vedi sotto).

### Compilare il report

Dall'interno di una sessione R con il progetto aperto:

```r
knitr::spin("analyses/2026-static_report.R", knit = FALSE)
rmarkdown::render("analyses/2026-static_report.Rmd",
  params = list(year = 2026, first_month = 1, last_month = 12)
)
```

Da terminale (adattare il percorso di Rscript e pandoc all'ambiente):

```bash
RSTUDIO_PANDOC="<percorso pandoc>" Rscript -e "
  knitr::spin('analyses/2026-static_report.R', knit = FALSE)
  rmarkdown::render('analyses/2026-static_report.Rmd')
"
```

> **Nota**: il file `.R` viene prima convertito in `.Rmd` da `knitr::spin()`, poi compilato da `rmarkdown::render()`. I file `.Rmd` e `.md` intermedi possono essere ignorati; l'output finale è l'`.html`.

### Parametri disponibili

| Parametro | Valori tipici | Descrizione |
|---|---|---|
| `year` | `2025` | Anno di riferimento del report |
| `first_month` | `1` o `7` | Primo mese del periodo |
| `last_month` | `6` o `12` | Ultimo mese del periodo |

Il `report_type` (`"annual"`, `"I semester"`, `"II semester"`) viene derivato automaticamente dai valori di `first_month` e `last_month`.

---

## Sviluppo del pacchetto

```r
devtools::load_all()   # carica tutte le funzioni di R/ nella sessione
devtools::document()   # rigenera NAMESPACE e man/ da roxygen2
devtools::test()       # esegue tutti i test
devtools::test(filter = "utils")  # singolo file di test
```

Le dipendenze sono gestite con **renv**. Per allineare l'ambiente al `renv.lock`:

```r
renv::restore()
```

---

## Struttura dati

Il file RDS principale (`tipnet.rds`) è una lista nested. Le slice rilevanti per i report statici sono:

| Indice | Contenuto |
|---|---|
| `[[3]][[1]]` | Anagrafica (codpat, gender, etnia, center) |
| `[[3]][[3]]` | Accettazione (ingresso, età, priorità, comorbidità, …) |
| `[[3]][[5]]` | PIM (pim2, pim3) |
| `[[3]][[9]]` | Ventilazione |
| `[[3]][[10]]` | Infezioni |
| `[[3]][[13]]` | Dimissione (durata degenza, esito, diagnosi, …) |

Il data object `centers_table` (esportato dal pacchetto) mappa codice centro → nome → città.
