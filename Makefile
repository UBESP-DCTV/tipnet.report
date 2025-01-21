# Makefile
# Definizione variabili configurabili
APP_NAME ?= TIPNet_test
APP_DIR ?= /srv/shiny-server/$(APP_NAME)
DATA_DIR ?= /srv/tipnet-data
RESTART_FILE = $(APP_DIR)/restart.txt

.PHONY: update update-code update-data

## Aggiornamento completo (codice + dati)
update: update-code update-data

## Aggiorna solo il codice: pull, dipendenze e riavvio
update-code:
	git -C $(APP_DIR) pull -X theirs && \
	touch -m $(RESTART_FILE) && \
	chown -R shiny:shiny $(APP_DIR) && \
	runuser -l shiny -c 'cd $(APP_DIR) && R -e "renv::restore()"' && \
	echo "✅ Aggiornamento codice completato per $(APP_NAME)"

## Aggiorna solo i dati
update-data:
	runuser -l shiny -c 'cd $(APP_DIR) && R -e "devtools::load_all(); fb_update_from_server(\"$(DATA_DIR)\")"' && \
	echo "✅ Aggiornamento dati completato per $(APP_NAME)"
