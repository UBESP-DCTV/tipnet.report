update:
	git pull -X theirs && \
	R -e "renv::restore()"
