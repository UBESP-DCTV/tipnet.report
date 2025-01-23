# Makefile
APP_NAME ?= TIPNet_test
APP_DIR ?= /srv/shiny-server/$(APP_NAME)
DATA_DIR ?= /srv/tipnet-data
RESTART_FILE = $(APP_DIR)/restart.txt

.PHONY: update update-code update-data

update: update-code update-data

update-code:
	cd $(APP_DIR) && git pull -X theirs && \
	touch -m $(RESTART_FILE) && \
	R -e "renv::restore()" && \
	echo "✅ Code update completed for $(APP_NAME)"

update-data:
	cd $(APP_DIR) && R -e "devtools::load_all(); fb_update_from_server('$(DATA_DIR)')" && \
	echo "✅ Data update completed for $(APP_NAME)"
