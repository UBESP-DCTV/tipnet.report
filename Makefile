update:
	git pull -X theirs && \
	touch -m /srv/shiny-server/TIPNet_test/restart.txt && \
	chown -R shiny:shiny . && \
	runuser -l shiny -c 'R -e "renv::restore()"' && \
	echo "done"
