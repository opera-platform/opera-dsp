#####################################################
# Chipyard integration documentation
#####################################################
docs_chipyard_integration_html:
	$(MAKE) -C docs/chipyard-integration html
	mkdir -p website/docs/chipyard-integration
	cp -R docs/chipyard-integration/build/html/. website/docs/chipyard-integration
