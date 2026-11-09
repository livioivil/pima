# Integrazione di PIMA parametrico con `multcomp::mmm`

## Obiettivo

Fare in modo che `pima(..., parametric = TRUE)` restituisca un oggetto utilizzabile dalle funzioni descrittive del pacchetto (`summary`, `plot`, `spec_curve`, `voe`, `volcano`, `janus`, `pima_tree`), mantenendo esplicita la differenza inferenziale rispetto a `flipscores`. Le operazioni che richiedono le statistiche dei flip devono terminare con un errore chiaro.

Questa è una proposta di implementazione. La preparazione del report non comporta modifiche al codice del pacchetto.

## Stato attuale e problemi da risolvere

1. Il ramo parametrico di [`pima()`](R/pima.R) restituisce direttamente il `data.frame` prodotto da [`pima.mmm()`](R/parametric_pima.R). Mancano classe `pima`, `summary_table`, `info`, `tested_coeffs`, `response`, `extra` e metadati dell'aggiustamento.
2. `pima()` non passa `id` a `pima.mmm()` e non trasferisce `extra` al risultato. `method`, `n_flips`, `tail` e `...` non hanno effetto nel ramo parametrico, senza una segnalazione specifica.
3. `pima.mmm()` chiama `mmm` senza namespace, mentre `multcomp` è in `Suggests`. Caricando il pacchetto e chiamando il ramo parametrico ho ottenuto `object 'mmm' not found`.
4. In una prova con due modelli `glm` e due coefficienti, `pima.mmm()` ha restituito zero righe. La combinazione di `sapply()` e `Reduce(intersect, ...)` non costruisce correttamente l'insieme dei coefficienti comuni; inoltre `names(mc$model)` viene riciclato nella tabella dei contrasti e non identifica ogni riga del test.
5. La colonna `p` deriva dal riepilogo del singolo `glm`, mentre `p.adj` deriva da `glht`. Questi p-value possono avere distribuzioni di riferimento diverse. `summary(glht)` usa per default un aggiustamento *single-step*: non è corretto chiamarlo `maxT` di `flipscores`. Si vedano i [metodi di test di multcomp](https://search.r-project.org/CRAN/refmans/multcomp/html/methods.html).

## Contratto proposto per l'oggetto

Usare una classe come `c("pima_parametric", "pima", "list")`. L'ereditarietà da `pima` permette ai metodi descrittivi di leggere una struttura comune; la classe specifica consente di distinguere il backend.

| Componente | Contenuto parametrico proposto |
| --- | --- |
| `summary_table` | Una riga per coppia modello–ipotesi, con almeno `model`, `response`, `coefficient`, `estimate`, `se`, `z`, `p`, `p.adj`; aggiungere `wald.p` solo come risultato distinto del modello singolo. |
| `info` | Una riga per modello, costruita con `.get_info_models()`, inclusi gli attributi `xs` ed `extra` necessari a `spec_curve()` e `voe_variance()`. |
| `tested_coeffs` | Coefficienti effettivamente testati, normalizzati e coerenti con `summary_table`. |
| `extra` | Metadati delle specificazioni, con chiave `model`. |
| `p.adjust.method` | Il metodo effettivo, per esempio `"single-step"` oppure `"none"`. |
| `backend` | Identificatore esplicito, per esempio `"mmm"`. |
| `glht` | Oggetto originale, utile per intervalli simultanei al livello richiesto. |

Non aggiungere `Tspace`, `score`, `.assign` o classi `flipscores` fittizi. La presenza di questi elementi farebbe sembrare disponibili procedure che richiedono i flip.

Per la prima versione conviene limitare le ipotesi a singoli coefficienti contro zero. Con contrasti generali, `coefficient` e `estimate` non hanno più necessariamente il significato richiesto dalle funzioni descrittive; servirà un contratto separato per `contrast` e per il valore nullo.

## Step di implementazione

### 1. Validare modelli, osservazioni e ipotesi

- Assegnare ai modelli nomi stabili e univoci; verificare la corrispondenza con `extra$model`.
- Inoltrare `id` da `pima()` e rifiutare ID mancanti o duplicati. Se `id` non è fornito, accettare il caso solo quando è possibile verificare che tutti i modelli usino le stesse osservazioni nello stesso ordine.
- Verificare `estfun.padded` su sottoinsiemi e valori mancanti, controllando dimensione e ordine delle righe. `mmm` assume che le righe di `estfun` corrispondano alle stesse osservazioni fra modelli; la [documentazione di `mmm`](https://search.r-project.org/CRAN/refmans/multcomp/html/mmm.html) descrive anche l'uso di `na.exclude`.
- Definire cosa accade se un coefficiente richiesto manca in un modello: errore esplicito nella prima versione, oppure ipotesi specifiche per modello in un'estensione successiva. Non usare un'intersezione silenziosa.

### 2. Calcolare i test e costruire una tabella senza join ambigui

- Preparare *prima* di `glht` una mappa con `model`, `coefficient` e ipotesi. Verificare che ogni contrasto restituito corrisponda esattamente a una riga della mappa.
- Calcolare `p` con `summary(glht_object, test = multcomp::univariate())` e `p.adj` con l'aggiustamento parametrico esplicito, per esempio `multcomp::adjusted("single-step")`. Conservare `wald.p` solo come informazione aggiuntiva.
- Evitare di estrarre il coefficiente dai nomi delle righe con `endsWith()` e di assegnare il modello riciclando `names(mc$model)`.
- Dichiarare il metodo e la famiglia di ipotesi nell'oggetto. L'aggiustamento riguarda l'insieme esatto di contrasti passato a `glht`; cambiando quell'insieme cambiano i p-value aggiustati.

### 3. Comporre l'oggetto `pima_parametric`

Riutilizzare `.get_response_name()` per la risposta e `.get_info_models()` per `info` ed `extra`; controllare che ogni riga di `summary_table` trovi un modello in `info`. Conservare l'oggetto `glht` in un campo dedicato. Evitare merge che eliminino righe senza segnalare il problema.

### 4. Adattare i metodi descrittivi

| Funzione | Intervento |
| --- | --- |
| `summary()`, `print()` | Dovrebbero funzionare con il contratto comune. |
| `plot()` | Usare `estimate` come default parametrico per l'asse x se `pcor` non ha una definizione coerente; mantenere il colore per gruppo e la forma per significatività. |
| `spec_curve()` | Usare `estimate` anche con più coefficienti quando `pcor` non è disponibile. Per `conf.int = TRUE`, usare gli intervalli di `confint(glht_object, level = 1 - alpha)`; l'attuale `.get_conf_int()` ricava intervalli dai p-value e non dovrebbe essere presentato come intervallo parametrico simultaneo. [Documentazione `confint.glht`](https://search.r-project.org/CRAN/refmans/multcomp/html/methods.html). |
| `voe()`, `volcano()`, `voe_variance()` | Applicare lo stesso default `estimate`; verificare che `response`, `link` e i metadati in `info` siano presenti. Separare il calcolo descrittivo di `voe_variance()` dalla ricostruzione degli intervalli, che oggi è chiamata anche quando non serve. |
| `janus()`, `pima_tree()` | Verificare i casi parametrico e non parametrico con la stessa tabella comune. |

Non riempire `pcor` con una trasformazione ad hoc di `z`: nei GLM e nei modelli con link diversi il significato potrebbe non essere comparabile. Anche `se` va documentato secondo il backend, senza presupporre che coincida con la colonna `se` prodotta da `flipscores`.

### 5. Bloccare le funzioni che richiedono i flip

[`combine_tests()`, `combine_contrasts()` e `p.adjust()`](R/flipscores_export.R) sono riesportazioni di funzioni `flipscores` che usano `Tspace` e altri elementi specifici. Non sono metodi S3 di `pima`: per intercettare una chiamata tramite l'API del pacchetto servono wrapper che controllino `inherits(x, "pima_parametric")` e terminino con un messaggio del tipo:

> `combine_tests()` richiede le statistiche dei flip; usa un risultato di `pima(..., parametric = FALSE)`.

Il controllo deve precedere qualsiasi accesso a `Tspace`. Per ogni nuova funzione che usa flip o oggetti `flipscores`, applicare la stessa regola. La chiamata diretta a `flipscores::combine_tests()` resta fuori dal controllo del pacchetto `pima`.

### 6. Chiarire l'API e verificare i risultati

L'attuale `method` ha valori `maxT`, `minP`, `none`, ma nel ramo parametrico viene ignorato. Definire un'opzione parametrica esplicita, per esempio `"single-step"`/`"none"`, e rifiutare combinazioni di argomenti non applicabili. `n_flips` e `tail` non devono essere accettati silenziosamente come se avessero effetto.

Verifiche minime prima di considerare l'integrazione completa:

1. Confrontare `estimate`, `se`, `z`, `p` e `p.adj` con gli oggetti `glht` su pochi modelli noti.
2. Controllare due o più coefficienti, più risposte, modelli senza alcuni coefficienti, sottoinsiemi differenti e valori mancanti.
3. Verificare che `summary`, `plot`, `spec_curve`, `voe` e `volcano` funzionino con i default documentati e che `conf.int = TRUE` produca gli intervalli attesi.
4. Verificare che le funzioni basate sui flip diano il messaggio previsto sul backend parametrico e continuino a funzionare su quello `flipscores`.
5. Eseguire installazione e `R CMD check` dopo aver aggiornato documentazione, `NAMESPACE` e dipendenze. Se `multcomp` rimane in `Suggests`, le chiamate devono essere qualificate (`multcomp::`) e il ramo parametrico deve verificare la disponibilità del pacchetto.

## Ordine di priorità

**Validità di osservazioni e ipotesi → tabella parametrica corretta → oggetto comune → adattamento delle visualizzazioni e degli intervalli → errori per le operazioni sui flip → verifiche e documentazione.**
